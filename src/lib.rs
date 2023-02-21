use std::collections::HashMap;

use encoding::{all::ASCII, EncoderTrap, Encoding};
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
#[derive(Clone, Copy)]
pub enum Color {
    Dark = "dark",
    Light = "light",
    HumanDark = "humandark",
    HumanLight = "humanlight",
    Corrected = "corrected",
}

#[wasm_bindgen]
pub struct Code {
    byte_edges: HashMap<(usize, usize), Vec<Edge>>,
    overrides: HashMap<(usize, usize), Color>,
    orig_data: Vec<bool>,
    orig_decoded: Vec<u8>,

    // each element is one Reed-Solomon block, each one containing a Vec of
    // locations that each byte came from
    blocks: Vec<Vec<[(usize, usize); 8]>>,
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
        let bit_positions: Vec<_> = positions
            .chunks_exact(8)
            .step_by(2)
            .chain(positions.chunks_exact(8).skip(1).step_by(2))
            .flatten()
            .copied()
            .collect();

        let blocks = bit_positions
            .chunks_exact(8)
            .map(|x| x.try_into().expect("8 positions must fit in a [_; 8]..."))
            .collect::<Vec<_>>()
            .chunks_exact(50)
            .map(|x| x.to_vec())
            .collect::<Vec<Vec<[(usize, usize); 8]>>>();

        let orig_data: Vec<bool> = image.chars().map(|c| c == '1').collect();

        // take just the plain-text locations out of the blocks, and drop the
        // first 12 bits... TODO: this *assumes* that it's in 8-bit-byte (ASCII,
        // but should be JIS8) encoding, and that version 4 has a four-bit Mode
        // prefix follewed by an 8-bit character count.
        let plaintext_locations = blocks
            .iter()
            .flat_map(|block| block.iter().take(32).flatten())
            .skip(12)
            .copied()
            .tuples()
            .map(|(a, b, c, d, e, f, g, h)| [a, b, c, d, e, f, g, h])
            .collect_vec();
        let orig_decoded = decode(&plaintext_locations, &orig_data);

        Code {
            byte_edges,
            overrides: HashMap::new(),
            orig_data,
            orig_decoded,
            blocks,
        }
    }

    pub fn get_color(&self, x: usize, y: usize) -> Result<Color, JsError> {
        if let Some(&x) = self.overrides.get(&(x, y)) {
            return Ok(x);
        }
        if let Some(&c) = self.orig_data.get(y * 33 + x) {
            if c {
                return Ok(Color::Dark);
            } else {
                return Ok(Color::Light);
            }
        }
        return Err(JsError::new("x, y out of range"));
    }

    pub fn get_byte_edges(&self, x: usize, y: usize) -> Vec<JsValue> {
        self.byte_edges
            .get(&(x, y))
            .map(|v| v.iter().copied().map(JsValue::from).collect())
            .unwrap_or_default()
    }

    pub fn get_orig_decoded_data(&self) -> String {
        // the encoding is actually JIS8, but assuming that a QR code is a URL
        // or something ... it'll be in the range where ASCII and JIS8 overlap.
        // And with ASCII, we won't try to interpret any multi-byte sequences
        // like we would if we were to erroneously try decoding as Shift-JIS.
        ASCII
            .decode(&self.orig_decoded, encoding::DecoderTrap::Replace)
            .expect("Replace should never fail")
            .replace(|c: char| -> bool { !c.is_ascii_graphic() }, "\u{fffd}")
    }

    pub fn update(&mut self, new_data: &str) -> bool {
        self.overrides = HashMap::new();

        let plaintext_bits = self
            .blocks
            .iter()
            .map(|block| block.iter().take(32).flatten())
            .flatten();

        let plaintext_locations: Vec<[(usize, usize); 8]> = plaintext_bits
            .skip(12)
            .chunks(8)
            .into_iter()
            .filter_map(|c| c.copied().collect_vec().try_into().ok())
            .collect();

        for (locations, (orig, new)) in plaintext_locations
            .into_iter()
            .zip(self.orig_decoded.iter().zip(new_data.chars()))
        {
            // if it's the replacement character or anything else that won't fit
            // in one byte, then don't count it as a change
            let Ok(new_value) = ASCII.encode(&new.to_string(), EncoderTrap::Strict) else {
                continue;
            };

            if new_value.len() != 1 {
                // this shouldn't happen because the ASCII encoder can't handle
                // multi-byte, but handle it anyway
                continue;
            }

            for (&location, bitmask) in locations
                .iter()
                .zip([128, 64, 32, 16, 8, 4, 2, 1].into_iter())
            {
                if orig & bitmask > 0 && new_value[0] & bitmask == 0 {
                    // was a 1 bit, but should be 0
                    if location.0 % 3 == 0 {
                        // 0 bit is *dark*, because we hit the mask
                        self.overrides.insert(location, Color::HumanDark);
                    } else {
                        // 0 bit is light, as is typical
                        self.overrides.insert(location, Color::HumanLight);
                    }
                }
                if orig & bitmask == 0 && new_value[0] & bitmask > 0 {
                    // was a 0 bit, but should be 1
                    if location.0 % 3 == 0 {
                        // 1 bit is *light*, because we hit the mask
                        self.overrides.insert(location, Color::HumanLight);
                    } else {
                        // 1 bit is dark, as is typical
                        self.overrides.insert(location, Color::HumanDark);
                    }
                }
            }
        }

        // incorporate those human colors into it
        let mut data = self.orig_data.clone();
        for (&(x, y), &color) in self.overrides.iter() {
            let bit = match color {
                Color::HumanDark => true,
                Color::HumanLight => false,
                _ => panic!("we only ever put Human* colors into the dictionary"),
            };
            data[y * 33 + x] = bit;
        }

        for block in &self.blocks {
            let bytes = decode(block, &data);
            // TODO: 18 error correction bytes, because 4-M is a (50, 32) code
            let rs = reed_solomon::Decoder::new(18);
            let Ok(corrected) = rs.correct(&bytes, None) else {
                return false;
            };

            let orig_bytes = decode(block, &self.orig_data);
            for (&locations, (old, new)) in block
                .iter()
                .zip(orig_bytes.into_iter().zip(corrected.into_iter()))
            {
                for (location, mask) in locations
                    .into_iter()
                    .zip([128, 64, 32, 16, 8, 4, 2, 1].into_iter())
                {
                    if self.overrides.contains_key(&location) {
                        // a human set this bit, don't re-color it
                        continue;
                    }
                    if old & mask != new & mask {
                        self.overrides.insert(location, Color::Corrected);
                    }
                }
            }
        }
        true
    }
}

fn decode(byte_locations: &[[(usize, usize); 8]], data: &[bool]) -> Vec<u8> {
    byte_locations
        .iter()
        .map(|&[i1, i2, i3, i4, i5, i6, i7, i8]| {
            // manually decode a byte, MSB-first
            #[allow(clippy::bool_to_int_with_if)]
            // because it doesn't make sense for the 0th bit to be any different
            (if bit_at(data, i1.0, i1.1) { 128 } else { 0 }
                + if bit_at(data, i2.0, i2.1) { 64 } else { 0 }
                + if bit_at(data, i3.0, i3.1) { 32 } else { 0 }
                + if bit_at(data, i4.0, i4.1) { 16 } else { 0 }
                + if bit_at(data, i5.0, i5.1) { 8 } else { 0 }
                + if bit_at(data, i6.0, i6.1) { 4 } else { 0 }
                + if bit_at(data, i7.0, i7.1) { 2 } else { 0 }
                + if bit_at(data, i8.0, i8.1) { 1 } else { 0 })
        })
        .collect::<Vec<u8>>()
}

fn bit_at(data: &[bool], x: usize, y: usize) -> bool {
    let dark_module = data[y * 33 + x];
    // TODO: this is hard-coded to mask pattern 010
    if x % 3 == 0 {
        !dark_module
    } else {
        dark_module
    }
}
