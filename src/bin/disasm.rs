use std::io::Read;

use dasha::Spanning;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let stdin = std::io::stdin();
    let mut buf = vec![];
    stdin.lock().read_to_end(&mut buf)?;
    for c in dasha::disasm(
        buf.bytes()
            .enumerate()
            .map(|(idx, byte)| Spanning(byte.unwrap(), idx, 1, None)),
    )? {
        println!("{:#x}:\t{}", c.1, c);
    }
    Ok(())
}
