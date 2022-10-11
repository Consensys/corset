use eyre::*;

pub fn is_file_empty(f: &str) -> Result<bool> {
    std::fs::metadata(&f)
        .with_context(|| eyre!("unable to read metadata of `{}`", f))
        .map(|f| f.len() == 0)
}
