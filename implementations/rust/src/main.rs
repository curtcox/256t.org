use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

const BASE64URL_ALPHABET: &[u8; 64] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_";

fn base_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(|p| p.parent())
        .map(Path::to_path_buf)
        .expect("unable to determine base directory")
}

fn cids_dir() -> PathBuf {
    base_dir().join("cids")
}

fn encode_length(length: usize) -> String {
    let bytes = (length as u64).to_be_bytes();
    to_base64url(&bytes[2..])
}

fn to_base64url(data: &[u8]) -> String {
    let mut encoded = String::with_capacity((data.len() + 2) / 3 * 4);

    for chunk in data.chunks(3) {
        let b0 = chunk[0];
        let b1 = if chunk.len() > 1 { chunk[1] } else { 0 };
        let b2 = if chunk.len() > 2 { chunk[2] } else { 0 };

        let i0 = (b0 >> 2) as usize;
        let i1 = (((b0 & 0b0000_0011) << 4) | (b1 >> 4)) as usize;
        let i2 = (((b1 & 0b0000_1111) << 2) | (b2 >> 6)) as usize;
        let i3 = (b2 & 0b0011_1111) as usize;

        encoded.push(BASE64URL_ALPHABET[i0] as char);
        encoded.push(BASE64URL_ALPHABET[i1] as char);

        if chunk.len() > 1 {
            encoded.push(BASE64URL_ALPHABET[i2] as char);
        }
        if chunk.len() > 2 {
            encoded.push(BASE64URL_ALPHABET[i3] as char);
        }
    }

    encoded
}

fn sha512_bytes(path: &Path) -> std::io::Result<Vec<u8>> {
    let output = Command::new("sha512sum")
        .arg(path)
        .output()?;

    if !output.status.success() {
        return Err(std::io::Error::new(
            std::io::ErrorKind::Other,
            format!("sha512sum failed with status {}", output.status),
        ));
    }

    let hex_hash = output
        .stdout
        .split(|b| *b == b' ')
        .next()
        .unwrap_or(&[]);

    hex_to_bytes(hex_hash)
}

fn hex_to_bytes(hex: &[u8]) -> std::io::Result<Vec<u8>> {
    if hex.len() % 2 != 0 {
        return Err(std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            "hex string must have even length",
        ));
    }

    let mut bytes = Vec::with_capacity(hex.len() / 2);
    for pair in hex.chunks(2) {
        let high = from_hex_digit(pair[0])?;
        let low = from_hex_digit(pair[1])?;
        bytes.push((high << 4) | low);
    }
    Ok(bytes)
}

fn from_hex_digit(digit: u8) -> std::io::Result<u8> {
    match digit {
        b'0'..=b'9' => Ok(digit - b'0'),
        b'a'..=b'f' => Ok(digit - b'a' + 10),
        b'A'..=b'F' => Ok(digit - b'A' + 10),
        _ => Err(std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            format!("invalid hex digit: {}", digit as char),
        )),
    }
}

fn compute_cid(path: &Path, content: &[u8]) -> std::io::Result<String> {
    let prefix = encode_length(content.len());
    let suffix = if content.len() <= 64 {
        to_base64url(content)
    } else {
        let hash = sha512_bytes(path)?;
        to_base64url(&hash)
    };

    Ok(format!("{}{}", prefix, suffix))
}

fn main() {
    let mut entries: Vec<_> = fs::read_dir(cids_dir())
        .expect("failed to read cids directory")
        .flatten()
        .collect();

    entries.sort_by(|a, b| a.file_name().cmp(&b.file_name()));

    let mut mismatches = Vec::new();
    let mut count = 0usize;

    for entry in entries {
        if entry.file_type().map(|t| t.is_dir()).unwrap_or(false) {
            continue;
        }

        count += 1;
        let path = entry.path();
        let content = match fs::read(&path) {
            Ok(bytes) => bytes,
            Err(err) => {
                eprintln!("Failed to read {}: {}", path.display(), err);
                mismatches.push((path.file_name().unwrap().to_string_lossy().into_owned(), String::new()));
                continue;
            }
        };

        let expected = match compute_cid(&path, &content) {
            Ok(cid) => cid,
            Err(err) => {
                eprintln!("Failed to compute CID for {}: {}", path.display(), err);
                mismatches.push((path.file_name().unwrap().to_string_lossy().into_owned(), String::new()));
                continue;
            }
        };

        let actual = path
            .file_name()
            .and_then(|name| name.to_str())
            .unwrap_or("")
            .to_string();

        if expected != actual {
            mismatches.push((actual, expected));
        }
    }

    if mismatches.is_empty() {
        println!("All {} CID files match their contents.", count);
    } else {
        println!("Found CID mismatches:");
        for (actual, expected) in mismatches {
            if expected.is_empty() {
                println!("- {} could not be validated", actual);
            } else {
                println!("- {} should be {}", actual, expected);
            }
        }
        std::process::exit(1);
    }
}
