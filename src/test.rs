use super::*;

#[test]
fn correct_number_of_bits() {
    // there are 807 bit positions in a 33x33 QR code
    let positions = ReadPosition::default().collect::<Vec<_>>();
    assert_eq!(positions.len(), 807);
}
