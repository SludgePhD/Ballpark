use std::{env, process::Command};

fn invoke(program: &str, args: &[&str]) {
    let status = Command::new(program).args(args).status().unwrap();
    if !status.success() {
        panic!("`{program}` exited with an error");
    }
}

fn invoke_cargo(args: &[&str]) {
    let cargo = env::var("CARGO").unwrap();
    invoke(&cargo, args);
}

const NIGHTLY: &str = "nightly";

const NO_STD_TARGET: &str = "thumbv7em-none-eabihf";

#[test]
fn build_with_nightly_features() {
    invoke(
        "rustup",
        &["run", NIGHTLY, "cargo", "build", "--features", "f16,f128"],
    );
}

#[test]
fn build_no_std() {
    invoke("rustup", &["target", "install", NO_STD_TARGET]);
    invoke_cargo(&[
        "build",
        "-p",
        "no-std",
        "--target",
        NO_STD_TARGET,
        "--no-default-features",
    ]);
    invoke_cargo(&[
        "build",
        "-p",
        "no-std",
        "--target",
        NO_STD_TARGET,
        "--no-default-features",
        "--features",
        "alloc",
    ]);
}

#[test]
fn debug_assertions() {
    // Build the unit tests in release mode, without `debug_assertions`.
    // The unit tests will then test that `debug_assert_approx_X` macros get turned off properly.
    invoke_cargo(&["test", "--release", "--lib"]);
}
