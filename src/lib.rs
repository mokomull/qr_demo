use std::collections::HashMap;

use encoding::{all::WINDOWS_31J, Encoding};
use itertools::Itertools;
use wasm_bindgen::prelude::*;

#[cfg(test)]
mod test;

struct ReadPosition {
    x: isize,
    y: isize,
    up: bool,
}

impl Default for ReadPosition {
    fn default() -> Self {
        ReadPosition {
            x: 31,
            y: 33,
            up: true,
        }
    }
}

impl Iterator for ReadPosition {
    type Item = (usize, usize);

    fn next(&mut self) -> Option<Self::Item> {
        if self.x < 0 {
            return None;
        }

        // we should never land on the vertical timing pattern
        assert_ne!(self.x, 6);

        // handle the horizontal off-by-a-smidge if we're right of the vertically-oriented timing
        // pattern
        let should_next_line = if self.x > 6 {
            self.x % 2 == 1
        } else {
            self.x % 2 == 0
        };

        let (probex, probey);
        if should_next_line {
            probex = self.x + 1;
            if self.up {
                probey = self.y - 1;
            } else {
                probey = self.y + 1;
            }
        } else {
            probex = self.x - 1;
            probey = self.y;
        }

        let off_end;

        if probey < 0 {
            assert!(self.up); // we shouldn't go off the top if we weren't headed up
            self.up = false;
            off_end = true;
        } else if probey > 32 {
            assert!(!self.up); // we shouldn't go off the bottom if we were headed up
            self.up = true;
            off_end = true;
        } else {
            off_end = false;
        }

        if off_end {
            assert!(should_next_line); // and we should have only gone off the top or bottom if we're moving lines
            self.x -= 2;
            // when we try to cross the vertical timing, we'll have gone from x=7 to x=5, but we need
            // to adjust that down to x=4 (because "should_next_line" will still want to be true next
            // iteration)
            if self.x == 5 {
                self.x = 4;
            }
            self.y = probey;
            return self.next();
        }

        self.x = probex;
        self.y = probey;

        let probex = usize::try_from(probex).expect("x should not be negative by here");
        let probey = usize::try_from(probey).expect("y should not be negative by here");

        if is_forbidden(probex, probey) {
            return self.next();
        }

        Some((probex, probey))
    }
}

fn is_forbidden(x: usize, y: usize) -> bool {
    [
        (0..9, 0..9),     // top left locating and format
        (25..33, 0..9),   // top right locating and format
        (0..9, 25..33),   // bottom left locating and format
        (0..33, 6..7),    // horizontal timing
        (6..7, 0..33),    // vertical timing
        (24..29, 24..29), // bottom right alignment
    ]
    .into_iter()
    .any(|(xs, ys)| xs.contains(&x) && ys.contains(&y))
}

#[wasm_bindgen]
#[derive(Clone, Copy)]
pub enum Edge {
    Left = "left",
    Right = "right",
    Top = "top",
    Bottom = "bottom",
}

#[wasm_bindgen]
pub struct Code {
    byte_edges: HashMap<(usize, usize), Vec<Edge>>,
    bit_positions: Vec<(usize, usize)>,
    orig_data: Vec<bool>,
}

#[wasm_bindgen]
impl Code {
    pub fn new(image: String) -> Code {
        let positions = ReadPosition::default().collect::<Vec<_>>();
        let mut byte_edges = HashMap::<_, Vec<Edge>>::new();

        for locations in positions.chunks(8) {
            for &(x, y) in locations {
                let edges = byte_edges.entry((x, y)).or_default();
                if x == 0 || !locations.contains(&(x - 1, y)) {
                    edges.push(Edge::Left)
                }
                if x == 32 || !locations.contains(&(x + 1, y)) {
                    edges.push(Edge::Right)
                }
                if y == 0 || !locations.contains(&(x, y - 1)) {
                    edges.push(Edge::Top)
                }
                if y == 32 || !locations.contains(&(x, y + 1)) {
                    edges.push(Edge::Bottom)
                }
            }
        }

        // For a version 4 code, the bits are in two groups, bytewise, and interleaved.
        // TODO: make this generic across all versions
        let bit_positions = positions
            .chunks_exact(8)
            .step_by(2)
            .interleave(positions.chunks_exact(8).skip(1).step_by(2))
            .flatten()
            .copied()
            .collect();

        Code {
            byte_edges,
            bit_positions,
            orig_data: image.chars().map(|c| c == '1').collect(),
        }
    }

    pub fn get_byte_edges(&self, x: usize, y: usize) -> Vec<JsValue> {
        self.byte_edges
            .get(&(x, y))
            .map(|v| v.iter().copied().map(JsValue::from).collect())
            .unwrap_or_default()
    }

    pub fn get_decoded_data(&self) -> String {
        // TODO: this *assumes* that it's in 8-bit-byte (Shift-JIS) encoding,
        // and that version 4 has a four-bit Mode prefix follewed by an 8-bit
        // character count.

        let bytes = self
            .bit_positions
            .iter()
            .skip(12)
            .map(|(x, y)| y * 33 + x)
            .tuples()
            .map(|(i1, i2, i3, i4, i5, i6, i7, i8)| {
                // manually decode a byte, MSB-first
                #[allow(clippy::bool_to_int_with_if)]
                // because it doesn't make sense for the 0th bit to be any different
                (if self.orig_data[i1] { 128 } else { 0 }
                    + if self.orig_data[i2] { 64 } else { 0 }
                    + if self.orig_data[i3] { 32 } else { 0 }
                    + if self.orig_data[i4] { 16 } else { 0 }
                    + if self.orig_data[i5] { 8 } else { 0 }
                    + if self.orig_data[i6] { 4 } else { 0 }
                    + if self.orig_data[i7] { 2 } else { 0 }
                    + if self.orig_data[i8] { 1 } else { 0 })
            })
            .collect::<Vec<u8>>();

        WINDOWS_31J
            .decode(&bytes, encoding::DecoderTrap::Replace)
            .expect("Replace should never fail")
    }
}
