use std::env;
use std::fs;
use std::path::Path;
use std::process::Command;

fn main() {
    let output = Command::new("cpp")
        .arg("src/c1core-t32.S")
        .output()
        .expect("could not execute C pre-processor");

    /* TODO: Should show stderr at this point */
    assert!(output.status.success());

    let asmsrc = String::from_utf8_lossy(&output.stdout);

    // We must "escape" curly braces (which Arm assembler uses for load/store
    // multiple instructions) otherwise lobal_asm!(include_str!(...)) will go
    // horribly wrong (curly brace has special meaning within macro arguments)
    let asmsrc = asmsrc.replace("{", "{{");
    let asmsrc = asmsrc.replace("}", "}}");

    let out_dir = env::var_os("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join("c1core-t32.s");
    fs::write(dest_path, asmsrc).expect("Unable to write file");
}
