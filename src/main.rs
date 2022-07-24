#![allow(non_snake_case)]

use std::arch::global_asm;
use std::os::raw::{c_int, c_void};

#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct st20c1_ctx {
    pub Areg: u32,
    pub Breg: u32,
    pub Creg: u32,
    pub Iptr: u32,
    pub Status: u32,
    pub Wptr: u32,
    pub Tdesc: u32,
    pub IOreg: u32,
    pub mem: *mut c_void,
}

extern "C" {
    pub fn st20c1_run(ctx: *mut st20c1_ctx) -> c_int;
}

global_asm!(include_str!(concat!(env!("OUT_DIR"), "/c1core-t32.s")));

fn main() {
    let mut ctx = st20c1_ctx {
        Areg: 0xaaaaaaaa,
        Breg: 0xbbbbbbbb,
        Creg: 0xcccccccc,
        Iptr: 0,
        Status: 0,
        Wptr: 0,
        Tdesc: 0,
        IOreg: 0,
        mem: std::ptr::null_mut(),
    };

    let mut code = vec![1_u8];
    ctx.mem = code.as_mut_ptr() as *mut c_void;

    let res;
    unsafe {
        res = st20c1_run(&mut ctx);
    }

    println!("{ctx:#?} -> {res}");
}
