use eyre::*;
use postgres::Client;
use std::io::Read;

pub fn is_file_empty(f: &str) -> Result<bool> {
    std::fs::metadata(&f)
        .with_context(|| eyre!("unable to read metadata of `{}`", f))
        .map(|f| f.len() == 0)
}

pub fn decompress(bytes: &[u8]) -> Result<String> {
    use flate2::read::GzDecoder;
    let mut gz = GzDecoder::new(bytes);
    let mut s = String::new();
    gz.read_to_string(&mut s)?;
    Ok(s)
}

pub fn connect_to_db(
    user: &str,
    password: &Option<String>,
    host: &str,
    database: &str,
) -> Result<Client> {
    Client::connect(
        &format!(
            "postgres://{}{}@{}/{}",
            user,
            password
                .as_ref()
                .map(|p| format!(":{}", p))
                .unwrap_or_default(),
            host,
            database
        ),
        postgres::NoTls,
    )
    .with_context(|| format!("while connecting to {}@{}/{}", user, host, database))
}
