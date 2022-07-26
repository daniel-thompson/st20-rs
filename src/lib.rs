// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2022 Daniel Thompson

#![allow(non_snake_case)]
#![allow(unused_macros)] // see TODO in asm.rs

pub mod asm;

use std::arch::global_asm;
use std::os::raw::{c_int, c_void};
use std::ptr;

#[repr(C)]
#[derive(Debug, Default, Copy, Clone)]
pub struct st20c1_regs {
    pub Areg: u32,
    pub Breg: u32,
    pub Creg: u32,
    pub Iptr: u32,
    pub Status: u32,
    pub Wptr: u32,
    pub Tdesc: u32,
    pub IOreg: u32,
}

#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct st20c1_ctx {
    pub regs: st20c1_regs,
    pub mem: *mut c_void,
}

impl Default for st20c1_ctx {
    fn default() -> Self {
        Self {
            regs: st20c1_regs::default(),
            mem: ptr::null_mut(),
        }
    }
}

extern "C" {
    pub fn st20c1_run(ctx: *mut st20c1_ctx) -> c_int;
}

global_asm!(include_str!(concat!(env!("OUT_DIR"), "/c1core-t32.s")));

#[macro_export]
macro_rules! assert_regs {
    ($core:expr, $a:expr) => {
        assert_eq!($core.regs.Areg, $a);
    };
    ($core:expr, $a:expr, $b:expr) => {
        assert_eq!($core.regs.Areg, $a);
        assert_eq!($core.regs.Breg, $b);
    };
    ($core:expr, $a:expr, $b:expr, $c:expr) => {
        assert_eq!($core.regs.Areg, $a);
        assert_eq!($core.regs.Breg, $b);
        assert_eq!($core.regs.Creg, $c);
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    fn run_fragment(code: &mut [u8]) -> st20c1_ctx {
        let mut c1 = st20c1_ctx::default();

        // mark the register stack to simplify test fragments
        c1.regs.Areg = 1000001;
        c1.regs.Breg = 2000020;
        c1.regs.Creg = 3000300;

        c1.mem = code.as_mut_ptr() as *mut c_void;

        let res;
        unsafe {
            res = st20c1_run(&mut c1);
        }

        println!("{c1:#?}");

        // a "fragment" exists from the final instruction in the memory,
        // which must be a breakpoint instruction
        assert_eq!(res, 0);
        assert_eq!(c1.regs.Iptr, code.len() as u32);

        c1
    }

    #[test]
    fn test_adc() {
        let mut code = assembleST20C1!(
            ldc     #1
            adc     #1
            adc4    #1
            adc8    #0xe1
            adc16   #0xbe01
            adc32   #0xdead000a_u32
            breakpoint
        );
        let c1 = run_fragment(&mut code);
        assert_regs!(c1, 0xdeadbeef);
    }

    #[test]
    fn test_add() {
        let mut code = assembleST20C1!(
            add
            breakpoint
        );
        let c1 = run_fragment(&mut code);
        assert_regs!(c1, 3000021, 3000300, 1000001);
    }

    #[test]
    fn test_and() {
        let mut code = assembleST20C1!(
            and
            breakpoint
        );
        let c1 = run_fragment(&mut code);
        assert_regs!(c1, 917504, 3000300, 1000001);
    }

    #[test]
    fn test_arot() {
        let mut code = assembleST20C1!(
            arot
            breakpoint
        );
        let c1 = run_fragment(&mut code);
        assert_regs!(c1, 3000300, 1000001, 2000020);
    }

    #[test]
    fn test_dup() {
        let mut code = assembleST20C1!(
            dup
            breakpoint
        );
        let c1 = run_fragment(&mut code);
        assert_regs!(c1, 1000001, 1000001, 2000020);
    }

    #[test]
    fn test_ldc4bit() {
        let mut code = assembleST20C1!(
            ldc     #0xc
            ldc4    #0xb
            ldc     #0xa
            breakpoint
        );
        let c1 = run_fragment(&mut code);
        assert_regs!(c1, 0x0a, 0x0b, 0x0c);

        let mut code = assembleST20C1!(
            ldc     #-1
            ldc4    #-8
            ldc     #-15
            breakpoint
        );
        let c1 = run_fragment(&mut code);
        assert_regs!(c1, -15_i32 as u32, -8_i32 as u32, -1_i32 as u32);
    }

    #[test]
    fn test_ldc8bit() {
        let mut code = assembleST20C1!(
            ldc8    #0xc0
            ldc8    #0xb1
            ldc8    #0xa2
            breakpoint
        );
        let c1 = run_fragment(&mut code);
        assert_regs!(c1, 0xa2, 0xb1, 0xc0);

        let mut code = assembleST20C1!(
            ldc8    #-1
            ldc8    #-88
            ldc8    #-252
            breakpoint
        );
        let c1 = run_fragment(&mut code);
        assert_regs!(c1, -252_i32 as u32, -88_i32 as u32, -1_i32 as u32);
    }

    #[test]
    fn test_ldc16bit() {
        let mut code = assembleST20C1!(
            ldc16   #0xc012
            ldc16   #0xb345
            ldc16   #0xa678
            breakpoint
        );
        let c1 = run_fragment(&mut code);
        assert_regs!(c1, 0xa678, 0xb345, 0xc012);

        let mut code = assembleST20C1!(
            ldc16   #-1
            ldc16   #-256
            ldc16   #-30000
            breakpoint
        );
        let c1 = run_fragment(&mut code);
        assert_regs!(c1, -30000_i32 as u32, -256_i32 as u32, -1_i32 as u32);
    }

    #[test]
    fn test_ldc32() {
        let mut code = assembleST20C1!(
            ldc32   #0xc0123456_u32
            ldc32   #0xb789abcd_u32
            ldc32   #0xaef01234_u32
            breakpoint
        );
        let c1 = run_fragment(&mut code);
        assert_regs!(c1, 0xaef01234, 0xb789abcd, 0xc0123456);

        let mut code = assembleST20C1!(
            ldc32   #-1
            ldc32   #-30000
            ldc32   #-800000000
            breakpoint
        );
        let c1 = run_fragment(&mut code);
        assert_regs!(c1, -800000000_i32 as u32, -30000_i32 as u32, -1_i32 as u32);
    }

    #[test]
    fn test_mul() {
        let mut code = assembleST20C1!(
            ldc     #9
            ldc8    #81
            mul
            breakpoint
        );
        let c1 = run_fragment(&mut code);
        assert_regs!(c1, 729, 1000001, 81);
    }

    #[test]
    fn test_nfix() {
        let mut code: Vec<u8> = vec![0x6f, 0x4f, 0xff];
        let c1 = run_fragment(&mut code);
        assert_regs!(c1, 0xffffffff, 1000001, 2000020);
    }

    #[test]
    fn test_not() {
        let mut code = assembleST20C1!(
            ldc     #0
            not
            ldc16   #0xffff
            not
            ldc32   #0x55aa33cc
            not
            breakpoint
        );
        let c1 = run_fragment(&mut code);
        assert_regs!(c1, 0xaa55cc33, 0xffff0000, 0xffffffff);
    }

    #[test]
    fn test_or() {
        let mut code = assembleST20C1!(
            or
            breakpoint
        );
        let c1 = run_fragment(&mut code);
        assert_regs!(c1, 2082517, 3000300, 1000001);
    }

    #[test]
    fn test_rev() {
        let mut code = assembleST20C1!(
            rev
            breakpoint
        );
        let c1 = run_fragment(&mut code);
        assert_regs!(c1, 2000020, 1000001, 3000300);
    }

    #[test]
    fn test_rot() {
        let mut code = assembleST20C1!(
            rot
            breakpoint
        );
        let c1 = run_fragment(&mut code);
        assert_regs!(c1, 2000020, 3000300, 1000001);
    }

    #[test]
    fn test_shl() {
        let mut code = assembleST20C1!(
            ldc     #4
            shl
            breakpoint
        );
        let c1 = run_fragment(&mut code);
        assert_regs!(c1, 16000016, 2000020, 1000001);
    }

    #[test]
    fn test_shr() {
        let mut code = assembleST20C1!(
            ldc     #4
            shr
            breakpoint
        );
        let c1 = run_fragment(&mut code);
        assert_regs!(c1, 62500, 2000020, 1000001);
    }

    #[test]
    fn test_sub() {
        let mut code = assembleST20C1!(
            adc8     #20
            sub
            breakpoint
        );
        let c1 = run_fragment(&mut code);
        assert_regs!(c1, 999999, 3000300, 1000021);
    }

    #[test]
    fn test_wsub() {
        let mut code = assembleST20C1!(
            ldc8    #250
            rev
            wsub
            breakpoint
        );
        let c1 = run_fragment(&mut code);
        assert_regs!(c1, 1001001, 2000020, 1000001);
    }
}
