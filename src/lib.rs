// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2022 Daniel Thompson

#![allow(non_snake_case)]
#![allow(unused_macros)] // see TODO in asm.rs
#![recursion_limit = "256"]

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
    fn test_ajw() {
        let mut code = assembleST20C1!(
            ajw32   #0x3c000000
            ajw32   #-0x1c000000
            ajw16   #0x3c00
            ajw16   #-0x1c00
            ajw8    #0x3c
            ajw8    #-0x1c
            ajw     #2
            ajw     #-1
            breakpoint
        );
        let c1 = run_fragment(&mut code);
        assert_eq!(c1.regs.Wptr, 0x80008084);
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
    fn test_cj() {
        let mut code = assembleST20C1!(
        start:
            cj      start
            breakpoint
        );
        let c1 = run_fragment(&mut code);
        assert_regs!(c1, 2000020, 3000300, 1000001);

        let mut code = assembleST20C1!(
            ldc     #0
            cj      end
            ldc16   #0xdead
        end:
            breakpoint
        );
        let c1 = run_fragment(&mut code);
        assert_regs!(c1, 0, 1000001, 2000020);

        let mut code = assembleST20C1!(
        start:
            adc     #0
            cj      start
            breakpoint
        );
        let c1 = run_fragment(&mut code);
        assert_regs!(c1, 2000020, 3000300, 1000001);

        let mut code = assembleST20C1!(
            ldc     #0
            cj      end
            ldc16   #0xdead
            adc     #0
        end:
            breakpoint
        );
        let c1 = run_fragment(&mut code);
        assert_regs!(c1, 0, 1000001, 2000020);
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
    fn test_eqc() {
        let mut code = assembleST20C1!(
            eqc32   #1000001
            ldc8    #188
            dup
            eqc8    #189
            rev
            eqc8    #188
            breakpoint
        );
        let c1 = run_fragment(&mut code);
        assert_regs!(c1, 1, 0, 1);
    }

    #[test]
    fn test_fcall() {
        let mut code = assembleST20C1!(
            ajw     #2          // @0
            j       skip_wptr   // @1
            nop                 // @6
            nop                 // @8
            nop                 // @10
        skip_wptr:
            fcall   end         // @12
            breakpoint          // @17
        end:
            ldl     #0          // @18
            breakpoint          // @19
        );
        let c1 = run_fragment(&mut code);
        assert_regs!(c1, 17, 1000001, 2000020);
        assert_eq!(c1.regs.Wptr, 8);
        assert_eq!(code[8], 17);
        assert_eq!(code[9], 0);
        assert_eq!(code[10], 0);
        assert_eq!(code[11], 0);
    }

    #[test]
    fn test_j() {
        let mut code = assembleST20C1!(
            j       give_me
        an_f:
            ldc     #0xf
            j       an_a
        a_c:
            ldc     #4
            shl
            adc     #0xc
            j       an_e
        an_e:
            ldc     #4
            shl
            adc     #0xe
            j       what_have_you_got
        give_me:
            j       an_f
        an_a:
            ldc     #4
            shl
            adc     #0xa
            j       a_c
        what_have_you_got:
            breakpoint
        );
        let c1 = run_fragment(&mut code);
        assert_regs!(c1, 0xface);
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
    fn test_ldl() {
        let mut code = assembleST20C1!(
            ldl32   #1
            ldl16   #2
            ajw     #15
            ldl8    #-12
            breakpoint
        );
        let c1 = run_fragment(&mut code);
        assert_regs!(
            c1,         // This will only work on little-endian hosts
            0xff746fbf, // ajw #15, nfix #f, ldl #4, breakpoint
            0x72202020, // pfix #0, pfix #0, pfix #0, ldl #2
            0x71202020  // pfix #0, pfix #0, pfix #0, ldl #1
        );
    }

    #[test]
    fn test_ldlp() {
        let mut code = assembleST20C1!(
            ldlp    #15
            ajw32   #0x04000000
            ldlp32  #0x008d159e
            breakpoint
        );
        let c1 = run_fragment(&mut code);
        assert_regs!(c1, 0x12345678, 60, 1000001);
    }

    #[test]
    fn test_ldnl() {
        let mut code = assembleST20C1!(
            ldlp16  #1
            ldnl16  #0
            ldlp8   #0
            ldnl8   #2
            ajw     #15
            ldlp    #0
            ldnl    #-12
            breakpoint
        );
        let c1 = run_fragment(&mut code);
        assert_regs!(
            c1,         // This will only work on little-endian hosts
            0x346f10bf, // ajw #15, ldlp #0, nfix #f, ldnl #4
            0x32201020, // pfix #0, ldlp #0, pfix #0, ldnl #2
            0x30202020  // pfix #0, pfix #0, pfix #0, ldnl #0
        );
    }

    #[test]
    fn test_ldnlp() {
        let mut code = assembleST20C1!(
            ldc     #3 // 0b0011
            ldnlp   #1 // 0b0111
            ldnlp4  #2 // 0b1111
            ldnlp8  #0x38 // 0xef
            ldnlp16 #0x2f80 // 0xbeef
            ldnlp32 #0x37ab4000 // 0xdeadbeef
            breakpoint
        );
        let c1 = run_fragment(&mut code);
        assert_regs!(c1, 0xdeadbeef);
    }

    #[test]
    fn test_ldpi() {
        let mut code = assembleST20C1!(
            ldc8    #100    // @0  100
            ldpi            // @2  104
            dup             // @4  104, 104
            ldpi            // @5  111, 104
            dup             // @7  111, 111, 104
            ldpi            // @8  121, 111, 104
            breakpoint      // @10 121, 111, 104
        );
        let c1 = run_fragment(&mut code);
        assert_regs!(c1, 121, 111, 104);
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
    fn test_stl() {
        let mut code = assembleST20C1!(
            nop
            adc     #0
            stl     #0
            nop
            rev
            stl     #1
            ajw     #1
            nop
            stl     #1
            breakpoint
        );
        let c1 = run_fragment(&mut code);
        assert_regs!(c1, 1000001, 3000300, 2000020);
        assert_eq!(code[0], (1000001 & 0xff) as u8);
        assert_eq!(code[1], ((1000001 >> 8) & 0xff) as u8);
        assert_eq!(code[2], ((1000001 >> 16) & 0xff) as u8);
        assert_eq!(code[3], ((1000001 >> 24) & 0xff) as u8);
        assert_eq!(code[4], (3000300 & 0xff) as u8);
        assert_eq!(code[5], ((3000300 >> 8) & 0xff) as u8);
        assert_eq!(code[6], ((3000300 >> 16) & 0xff) as u8);
        assert_eq!(code[7], ((3000300 >> 24) & 0xff) as u8);
        assert_eq!(code[8], (2000020 & 0xff) as u8);
        assert_eq!(code[9], ((2000020 >> 8) & 0xff) as u8);
        assert_eq!(code[10], ((2000020 >> 16) & 0xff) as u8);
        assert_eq!(code[11], ((2000020 >> 24) & 0xff) as u8);
    }

    #[test]
    fn test_stnl() {
        let mut code = assembleST20C1!(
            nop         // @0
            ldlp    #1  // @2
            stnl    #2  // @3
            j       end // @4
            adc     #0  // @9
            nop         // @10
            nop         // @12
            nop         // @14
        end:
            breakpoint
        );
        let c1 = run_fragment(&mut code);
        assert_regs!(c1, 2000020, 4, 1000001);
        assert_eq!(code[12], (1000001 & 0xff) as u8);
        assert_eq!(code[13], ((1000001 >> 8) & 0xff) as u8);
        assert_eq!(code[14], ((1000001 >> 16) & 0xff) as u8);
        assert_eq!(code[15], ((1000001 >> 24) & 0xff) as u8);
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
