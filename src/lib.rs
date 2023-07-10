use std::collections::HashMap;

use encoding::{all::ASCII, EncoderTrap, Encoding};
use itertools::Itertools;
use wasm_bindgen::prelude::*;

#[cfg(test)]
mod test;

struct CodeSpec {
    size: usize,
    alignment_center_coordinates: &'static [usize],
    reed_solomon_blocks: &'static [(usize, usize)],
}

static VERSION_4M: &'static CodeSpec = &CodeSpec {
    size: 33,
    alignment_center_coordinates: &[6, 26],
    reed_solomon_blocks: &[(50, 32); 2],
};

struct ReadPosition {
    code: &'static CodeSpec,
    x: isize,
    y: isize,
    up: bool,
}

impl ReadPosition {
    fn new(code: &'static CodeSpec) -> Self {
        ReadPosition {
            code,
            // place the pointer moving upwards, in the next-to-last column, just off the bottom of the code
            // (i.e. so that its first next() will move it to the exact bottom-right of the code).
            x: code.size as isize - 2,
            y: code.size as isize,
            up: true,
        }
    }

    fn is_forbidden(&self, x: usize, y: usize) -> bool {
        let size = self.code.size;

        let position_detection = [
            (0..9, 0..9),             // top left locating and format
            ((size - 8)..size, 0..9), // top right locating and format
            (0..9, (size - 8)..size), // bottom left locating and format
        ];

        // note for next time you have to read this: .. is the "up-to-but-not-including" operator.
        position_detection
            .iter()
            .cloned()
            .chain(
                [
                    (0..size, 6..7), // horizontal timing
                    (6..7, 0..size), // vertical timing
                ]
                .into_iter(),
            )
            .chain(
                // alignment patterns
                self.code
                    .alignment_center_coordinates
                    .into_iter()
                    .cartesian_product(self.code.alignment_center_coordinates.into_iter())
                    .filter_map(|(x, y)| {
                        // skip the alignment patterns whose centers are within the position-detection regions
                        if position_detection
                            .iter()
                            .any(|(xs, ys)| xs.contains(&x) && ys.contains(&y))
                        {
                            None
                        } else {
                            Some(((x - 2)..(x + 3), (y - 2)..(y + 3)))
                        }
                    }),
            )
            .any(|(xs, ys)| xs.contains(&x) && ys.contains(&y))
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
        } else if probey >= self.code.size as isize {
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

        if self.is_forbidden(probex, probey) {
            return self.next();
        }

        Some((probex, probey))
    }
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
    spec: &'static CodeSpec,

    plaintext_locations: Vec<[(usize, usize); 8]>,

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
        let spec = VERSION_4M;
        let positions = ReadPosition::new(spec).collect::<Vec<_>>();
        let mut byte_edges = HashMap::<_, Vec<Edge>>::new();

        for locations in positions.chunks(8) {
            for &(x, y) in locations {
                let edges = byte_edges.entry((x, y)).or_default();
                if x == 0 || !locations.contains(&(x - 1, y)) {
                    edges.push(Edge::Left)
                }
                if x == (spec.size - 1) || !locations.contains(&(x + 1, y)) {
                    edges.push(Edge::Right)
                }
                if y == 0 || !locations.contains(&(x, y - 1)) {
                    edges.push(Edge::Top)
                }
                if y == (spec.size - 1) || !locations.contains(&(x, y + 1)) {
                    edges.push(Edge::Bottom)
                }
            }
        }

        // For a code with multiple Reed-Solomon blocks, the individual codewords are interleaved.
        let codeword_positions: Vec<[(usize, usize); 8]> = positions
            .chunks_exact(8)
            .map(|i| i.try_into().expect("8 positions must fit in a [_; 8]...()"))
            .collect();

        let blocks = spec
            .reed_solomon_blocks
            .iter()
            .enumerate()
            .map(|(i, &(length, _))| {
                // TODO: this will probably fail on versions which have multiple differently-sized
                // Reed-Solomon blocks, since the spec says that all of the data words come first.
                codeword_positions
                    .iter()
                    .skip(i) // the Reed-Solomon blocks are interleaved in the QR code
                    .step_by(spec.reed_solomon_blocks.len())
                    .take(length)
                    .copied()
                    .collect_vec()
            })
            .collect_vec();

        let orig_data: Vec<bool> = image.chars().map(|c| c == '1').collect();

        // take just the plain-text locations out of the blocks, and drop the
        // first 12 bits... TODO: this *assumes* that it's in 8-bit-byte (ASCII,
        // but should be JIS8) encoding, and that version 4 has a four-bit Mode
        // prefix follewed by an 8-bit character count.
        assert!(blocks.len() == spec.reed_solomon_blocks.len());
        let plaintext_locations = blocks
            .iter()
            .zip(spec.reed_solomon_blocks.iter())
            .flat_map(|(positions, &(_length, plaintext_count))| {
                positions.iter().take(plaintext_count).flatten()
            })
            .skip(12) // 4 bits for type and 8 bits for count; TODO: handle larger codes
            .copied()
            .tuples()
            .map(|(a, b, c, d, e, f, g, h)| [a, b, c, d, e, f, g, h])
            .collect_vec();
        let orig_decoded = decode(spec, &plaintext_locations, &orig_data);

        Code {
            spec,
            plaintext_locations,
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
        if let Some(&c) = self.orig_data.get(y * self.spec.size + x) {
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

        for (locations, (orig, new)) in self
            .plaintext_locations
            .iter()
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
            data[y * self.spec.size + x] = bit;
        }

        for ((block_len, plaintext_len), block) in
            self.spec.reed_solomon_blocks.iter().zip(&self.blocks)
        {
            let bytes = decode(self.spec, block, &data);
            let rs = reed_solomon::Decoder::new(block_len - plaintext_len);
            let Ok(corrected) = rs.correct(&bytes, None) else {
                return false;
            };

            let orig_bytes = decode(self.spec, block, &self.orig_data);
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

fn decode(spec: &CodeSpec, byte_locations: &[[(usize, usize); 8]], data: &[bool]) -> Vec<u8> {
    let get_bit = |x, y| bit_at(spec, data, x, y);

    byte_locations
        .iter()
        .map(|&[i1, i2, i3, i4, i5, i6, i7, i8]| {
            // manually decode a byte, MSB-first
            #[allow(clippy::bool_to_int_with_if)]
            // because it doesn't make sense for the 0th bit to be any different
            (if get_bit(i1.0, i1.1) { 128 } else { 0 }
                + if get_bit(i2.0, i2.1) { 64 } else { 0 }
                + if get_bit(i3.0, i3.1) { 32 } else { 0 }
                + if get_bit(i4.0, i4.1) { 16 } else { 0 }
                + if get_bit(i5.0, i5.1) { 8 } else { 0 }
                + if get_bit(i6.0, i6.1) { 4 } else { 0 }
                + if get_bit(i7.0, i7.1) { 2 } else { 0 }
                + if get_bit(i8.0, i8.1) { 1 } else { 0 })
        })
        .collect::<Vec<u8>>()
}

fn bit_at(spec: &CodeSpec, data: &[bool], x: usize, y: usize) -> bool {
    let dark_module = data[y * spec.size + x];
    // TODO: this is hard-coded to mask pattern 010
    if x % 3 == 0 {
        !dark_module
    } else {
        dark_module
    }
}
