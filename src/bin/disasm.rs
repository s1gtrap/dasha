use std::io::Read;

use dasha::Spanning;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let stdin = std::io::stdin();
    let mut buf = vec![];
    stdin.lock().read_to_end(&mut buf)?;
    if buf
        .iter()
        .all(|&b| (b as char).is_whitespace() || 0x20 <= b && b <= 0x7e)
    {
        // text
        let text = std::str::from_utf8(&buf).unwrap(); // what about unicode?

        for c in dasha::disasm(text)? {
            println!("{:#x}:\t{}", c.1, c);
        }
    } else {
        // binary
        for c in dasha::disasm_bytes(
            buf.bytes()
                .enumerate()
                .map(|(idx, byte)| Spanning(byte.unwrap(), idx, 1, None)),
        )? {
            println!("{:#x}:\t{}", c.1, c);
        }
    }
    Ok(())
}
