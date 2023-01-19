use std::process::Command;
fn main() {
    let git_hash = Command::new("git")
        .args(&["rev-parse", "HEAD"])
        .output()
        .ok()
        .and_then(|o| String::from_utf8(o.stdout).ok())
        .map(|s| s.chars().take(7).collect::<String>())
        .unwrap_or_else(|| "UNKNW".into());
    println!("cargo:rustc-env=GIT_HASH={}", git_hash);
}
