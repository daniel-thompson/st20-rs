// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2022 Daniel Thompson

use st20_rs::*;
use std::os::raw::c_void;

fn main() {
    let mut c1 = st20c1_ctx::default();
    let mut code = vec![1_u8];
    c1.mem = code.as_mut_ptr() as *mut c_void;

    let res;
    unsafe {
        res = st20c1_run(&mut c1);
    }

    println!("{c1:#?} -> {res}");
}
