// SPDX-License-Identifier: Apache-2.0 OR MIT

// TODO: I haven't yet figured out why this warning appears... so for now we
//       will just shut it down!
#![allow(unused_macros)]

//! An ST20-C1 assembler implemented as a macro.
//!
//! This assembler is based on rustasm6502 by Jonas Schievink. This is
//! assembler is a complete rewrite of the ops table. However most of the
//! 'clever' bits of the code come unmodified from Jonas. For that reason
//! I have preserved the original Apache-2.0 or MIT license used by the
//! original author (hence this assembler has a different, but compatible,
//! license to the rest of the crate).
//!
//! # Example
//!
//! ```
//! #[macro_use] #[no_link]
//! extern crate st20-rs;
//!
//! fn main() {
//!     let machine_code = asm::assembleST20C1! {
//!         lda #0x00
//!         ldx #0x00
//!         ldy #0x00
//!         txs
//!
//!     main:
//!         adc 0xff        // Zero-Page
//!         adc abs 0x1234  // Absolute address 0x1234
//!         ror a           // Rotate the accumulator
//!         beq main
//!     end:
//!         jmp end
//!     };
//!
//!     assert_eq!(machine_code, [
//!         0xA9, 0x00,
//!         0xA2, 0x00,
//!         0xA0, 0x00,
//!         0x9A,
//!
//!         0x65, 0xff,
//!         0x6D, 0x34, 0x12,   // Little-endian
//!         0x6A,
//!         0xF0, (-8i8) as u8,
//!
//!         0x4C, 15, 0,
//!     ]);
//! }
//! ```

/// A compile-time map from identifiers to arbitrary (heterogeneous) expressions
#[macro_export]
#[doc(hidden)]
macro_rules! ident_map {
    ( $name:ident = { $($key:ident => $e:expr),* $(,)* } ) => {
        macro_rules! $name {
            $(
                ( $key ) => { $e };
            )*
            // Empty invocation expands to nothing. Needed when the map is empty.
            () => {};
        }
    };
}

/// Returns the number of comma-separated expressions passed to it
#[macro_export]
#[doc(hidden)]
macro_rules! codelen {
    () => { 0 };
    ( $one:expr ) => { 1 };
    ( $first:expr, $($rest:expr),+ ) => { 1 + codelen!($($rest),+) };
}

/// Replace elements of "arrays" of expressions with sorted replacements by
/// seeking along a "positional array" containing n expressions, then replacing
/// elements in the source array.
///
/// Expands to the first array with replacements applied. The length doesn't
/// change.
#[macro_export]
#[doc(hidden)]
macro_rules! lockstep_replace {
    ( [ $($result:expr),* ], [], ) => {
        // `src` is empty, no relocations. We're done!
        [ $($result),* ]
    };
    ( [ $($result:expr),* ], [ $($src:expr,)+ ], ) => {
        // Empty list of replacements, but still `src` to go
        [ $($result,)* $($src),+ ]
    };
    ( [ $($result:expr),* ], [ $($src:expr,)* ], [], [], $( [ $($pos:expr,)* ], [ $($rep:expr,)* ], )* ) => {
        // All replacements applied. Pop the current replacement and continue.
        lockstep_replace!(
            [ $($result),* ],
            [ $($src,)* ],
            $(
                [ $($pos,)* ],
                [ $($rep,)* ],
            )*
        )
    };
    ( [ $($result:expr),* ], [ $src1_replaced:expr, $($src:expr,)* ], [], [ $rep1:expr, $($rep_rest:expr,)* ], $( [ $pos1:expr, $($pos:expr,)* ], [ $($rep:expr,)* ], )* ) => {
        // Position of a replacement reached (or: inside a replacement)
        // Coupled with a seek step
        lockstep_replace!(
            [ $($result,)* $rep1 ],
            [ $($src,)* ],
            [],
            [ $($rep_rest,)* ],
            $(
                [ $($pos,)* ],
                [ $($rep,)* ],
            )*
        )
    };
    ( [ $($result:expr),* ], [ $src1:expr, $($src:expr,)* ], $( [ $pos1:expr, $($pos:expr,)* ], [ $($rep:expr,)* ], )+ ) => {
        // Seek to next replacement position (simultaneously for all
        // replacements)
        lockstep_replace!(
            [ $($result,)* $src1 ],
            [ $($src,)* ],
            $(
                [ $($pos,)* ],
                [ $($rep,)* ],
            )+
        )
    };
}

/// Performs relocation of machine code based on given labels and relocations.
/// Looks up label names in an `ident_map`. Expands to the relocated machine
/// code.
///
/// Relocation formats:
/// { $label as ABS16 @ [$lockstepmcpos] }
#[macro_export]
#[doc(hidden)]
macro_rules! reloc {
    ( { $($attr:tt)* }  [ $( [ $($pos:expr),* ], [ $($rep:expr),* ] ),* ], $lblmap:ident, [ $($mcode:expr),* ], [/* empty relocation list */] ) => {
        lockstep_replace!([], [ $($mcode,)* ], $( [ $($pos,)* ], [ $($rep,)* ], )*)
    };
    ( { start: $start:expr }  [ $( [ $($pos:expr),* ], [ $($rep:expr),* ] ),* ], $lblmap:ident, [ $($mcode:expr),* ], [ { $lbl:ident as ABS16 @ [$($lockstepmcpos:expr),*] } $(,$reloc:tt)* ] ) => {
        // Replace 2 Bytes with the absolute address
        // Relocation position is given as "lock-step MC pos", an expression
        // list that's as long as all mcode before the relocation should happen.
        reloc!(
            { start: $start }
            [ $( [ $($pos),* ], [ $($rep),* ] ,)*
            [ $($lockstepmcpos),* ], [ ($lblmap!($lbl) + $start) as u8, (($lblmap!($lbl) + $start) >> 8) as u8 ] ],
            $lblmap, [ $($mcode),* ], [ $($reloc),* ])
    };
    ( { $($attr:tt)* }  [ $( [ $($pos:expr),* ], [ $($rep:expr),* ] ),* ], $lblmap:ident, [ $($mcode:expr),* ], [ { $lbl:ident as PCREL @ [$($lockstepmcpos:expr),*] } $(,$reloc:tt)* ] ) => {
        // Replace 1 Byte with the PC relative address
        // PC is the program counter *after* the relocated offset (the length of the
        // `$lockstepmcpos` array + 1), so we need to subtract 1 additional byte.
        reloc!(
            { $($attr)* }
            [ $( [ $($pos),* ], [ $($rep),* ] ,)*
            [ $($lockstepmcpos),* ], [ ( $lblmap!($lbl) as i32 - codelen!($($lockstepmcpos),*) as i32 - 1 ) as u8 ] ],
            $lblmap, [ $($mcode),* ], [ $($reloc),* ])
    };
}

#[macro_export]
#[doc(hidden)]
macro_rules! asm_ {
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        // EOF
    ) => {{
        ident_map!(labelmap = {
            $($lbl => $lblval),*
        });
        reloc!({ $($attr)* } [], labelmap, [ $($mcode),* ], [ $($reloc),* ])
    }};

    // ==================================================================================
    // ==================================================================================
    // ==================================================================================

    // Opcode assembly table.
    // See ST20-C1 Core Instruction Set Reference Manual for more details of the
    // instruction set.

    // ADC
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        adc # $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x80 + $imm) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        adc4 # $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x80 + $imm) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        adc8 # $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x20 | (($imm >> 4) & 0xf)), (0x80 | ($imm & 0xf)) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        adc16 # $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x20 | (($imm >> 12) & 0xf)) as u8, (0x20 | (($imm >> 8) & 0xf)) as u8, (0x20 | (($imm >> 4) & 0xf)) as u8, (0x80 | ($imm & 0xf) as u8) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        adc32 # $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x20 | (($imm >> 28) & 0xf)) as u8, (0x20 | (($imm >> 24) & 0xf)) as u8, (0x20 | (($imm >> 20) & 0xf)) as u8, (0x20 | (($imm >> 16) & 0xf)) as u8, (0x20 | (($imm >> 12) & 0xf)) as u8, (0x20 | (($imm >> 8) & 0xf)) as u8, (0x20 | (($imm >> 4) & 0xf)) as u8, (0x80 | ($imm & 0xf)) as u8 ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };

    // ADD
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        add
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* 0xF4 ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };

    // AROT
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        arot
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* 0xF3 ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };

    // BCC
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        bcc $label:ident
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* 0x90, 0x00 ],
            [ $($lbl => $lblval),* ], [ $($reloc,)* { $label as PCREL @ [$($mcode,)* 0x90] } ], $($rest)*)
    };
    // BCS
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        bcs $label:ident
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* 0xB0, 0x00 ],
            [ $($lbl => $lblval),* ], [ $($reloc,)* { $label as PCREL @ [$($mcode,)* 0xB0] } ], $($rest)*)
    };

    // BREAKPOINT
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        breakpoint
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* 0xFF ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };

    // DUP
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        dup
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* 0xF1 ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };

    // JMP
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        jmp $label:ident    // jmp abs
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* 0x4C, 0x00, 0x00 ],
            [ $($lbl => $lblval),* ], [ $($reloc,)* { $label as ABS16 @ [$($mcode,)* 0x4C] } ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        jmp ($ind:tt)
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* 0x6C, ($ind as u16) as u8, (($ind as u16) >> 8) as u8 ],
            [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };

    // LDA
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        lda # $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* 0xA9, $imm ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        lda ($ind:tt, x)
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* 0xA1, $ind ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        lda ($ind:tt), y
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* 0xB1, $ind ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        lda $zp:tt, x
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* 0xB5, $zp ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        lda abs $abs:tt, x
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* 0xBD, ($abs as u16) as u8, (($abs as u16) >> 8) as u8 ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        lda abs $abs:tt, y
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* 0xB9, ($abs as u16) as u8, (($abs as u16) >> 8) as u8 ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        lda abs $abs:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* 0xAD, ($abs as u16) as u8, (($abs as u16) >> 8) as u8 ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        lda $zp:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* 0xA5, $zp ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };

    // LDC
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ldc # - $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x6f), (0x40 + (0x10 - $imm)) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ldc # $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x40 + $imm) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ldc4 # - $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x6f), (0x40 + (0x10 - $imm)) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ldc4 # $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x40 + $imm) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ldc8 # - $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x60 | (((0x100 - $imm) >> 4) & 0xf)) as u8, (0x40 | ((0x100 - $imm) & 0xf)) as u8 ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ldc8 # $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x20 | (($imm >> 4) & 0xf)), (0x40 | ($imm & 0xf)) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ldc16 # - $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x60 | (((0x10000 - $imm) >> 12) & 0xf)) as u8, (0x20 | (((0x10000 - $imm) >> 8) & 0xf)) as u8, (0x20 | (((0x10000 - $imm) >> 4) & 0xf)) as u8, (0x40 | ((0x10000 - $imm) & 0xf) as u8) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ldc16 # $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x20 | (($imm >> 12) & 0xf)) as u8, (0x20 | (($imm >> 8) & 0xf)) as u8, (0x20 | (($imm >> 4) & 0xf)) as u8, (0x40 | ($imm & 0xf) as u8) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ldc32 # - $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x68 | (((0x80000000_u32 - $imm) >> 28) & 0xf)) as u8, (0x20 | (((0x80000000_u32 - $imm) >> 24) & 0xf)) as u8, (0x20 | (((0x80000000_u32 - $imm) >> 20) & 0xf)) as u8, (0x20 | (((0x80000000_u32 - $imm) >> 16) & 0xf)) as u8, (0x20 | (((0x80000000_u32 - $imm) >> 12) & 0xf)) as u8, (0x20 | (((0x80000000_u32 - $imm) >> 8) & 0xf)) as u8, (0x20 | (((0x80000000_u32 - $imm) >> 4) & 0xf)) as u8, (0x40 | ((0x80000000_u32 - $imm) & 0xf)) as u8 ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ldc32 # $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x20 | (($imm >> 28) & 0xf)) as u8, (0x20 | (($imm >> 24) & 0xf)) as u8, (0x20 | (($imm >> 20) & 0xf)) as u8, (0x20 | (($imm >> 16) & 0xf)) as u8, (0x20 | (($imm >> 12) & 0xf)) as u8, (0x20 | (($imm >> 8) & 0xf)) as u8, (0x20 | (($imm >> 4) & 0xf)) as u8, (0x40 | ($imm & 0xf)) as u8 ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    // LDX
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ldx # $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* 0xA2, $imm ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ldx $zp:tt, y
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* 0xB6, $zp ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ldx abs $abs:tt, y
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* 0xBE, ($abs as u16) as u8, (($abs as u16) >> 8) as u8 ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ldx abs $abs:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* 0xAE, ($abs as u16) as u8, (($abs as u16) >> 8) as u8 ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ldx $zp:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* 0xA6, $zp ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };

    // LSR - Logical Shift Right
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        lsr a     // Accumulator
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* 0x4A ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        lsr $zp:tt, x
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* 0x56, $zp ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        lsr abs $abs:tt, x
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* 0x5E, ($abs as u16) as u8, (($abs as u16) >> 8) as u8 ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        lsr abs $abs:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* 0x4E, ($abs as u16) as u8, (($abs as u16) >> 8) as u8 ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        lsr $zp:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* 0x46, $zp ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };

    // NOT
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        not
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* 0xF8 ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };

    // REV
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        rev
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* 0xF0 ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };

    // ROL - Rotate Left
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        rol a     // Accumulator
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* 0x2A ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        rol $zp:tt, x
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* 0x36, $zp ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        rol abs $abs:tt, x
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* 0x3E, ($abs as u16) as u8, (($abs as u16) >> 8) as u8 ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        rol abs $abs:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* 0x2E, ($abs as u16) as u8, (($abs as u16) >> 8) as u8 ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        rol $zp:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* 0x26, $zp ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };

    // ROR - Rotate Right
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ror a     // Accumulator
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* 0x6A ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ror $zp:tt, x
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* 0x76, $zp ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ror abs $abs:tt, x
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* 0x7E, ($abs as u16) as u8, (($abs as u16) >> 8) as u8 ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ror abs $abs:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* 0x6E, ($abs as u16) as u8, (($abs as u16) >> 8) as u8 ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ror $zp:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* 0x66, $zp ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };

    // ROT
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        rot
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* 0xF2 ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };

    // TXA
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        txa
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* 0x8A ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };

    // ==================================================================================
    // ==================================================================================
    // ==================================================================================

    // Check for labels
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        $label:ident :
    $($rest:tt)* ) => {
        asm_!(
            { $($attr)* }
            [ $($mcode),* ],
            [ $($lbl => $lblval,)* $label => codelen!($($mcode),*) ],
            [ $($reloc),* ],
            $($rest)*
        )
    };
}

/// Entry point for the macro-based ST20-C1 assembler.
///
/// Expands to a fixed-size `u8` array containing the assembled machine code.
///
/// **Note**: Any errors in the assembly will be reported as inscrutable expansion errors. This is
/// a limitation in Rust's current macro implementation.
#[macro_export]
macro_rules! assembleST20C1 {
    ( {
        start: $start:expr,
        code: {
            $($tokens:tt)*
        }
    } ) => {
        asm_!({ start: $start } [], [], [], $($tokens)*)
    };
    ( $($tokens:tt)* ) => {
        assembleST20C1!({
            start: 0,
            code: {
                $($tokens)*
            }
        })
    };
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_ident_map() {
        ident_map!(my_map = {
            main => 0,
            end => 0x45,
        });
        assert_eq!(my_map!(main), 0);
        assert_eq!(my_map!(end), 0x45);
        // Unknown keys cause syntax errors (can't test that, but believe me :P)
    }

    /// Test simple label relocation
    #[test]
    fn simple_jmp() {
        let mcode = assembleST20C1!(
            start: jmp start
        );
        assert_eq!(mcode, [0x4C, 0x00, 0x00]);
    }

    #[test]
    fn indirect_jmp() {
        let mcode = assembleST20C1!(jmp(0x5432));
        assert_eq!(mcode, [0x6C, 0x32, 0x54]);
    }

    /// Tests multiple labels and relocated jumps, `lbl1` is unused
    #[test]
    fn labels() {
        let mcode = assembleST20C1!(
            start:
                lda #0x0f
            lbl1:
                lda #0x0f
                jmp main
            main:
                jmp start
        );
        assert_eq!(
            mcode,
            [0xA9, 0x0F, 0xA9, 0x0F, 0x4C, 0x07, 0x00, 0x4C, 0x00, 0x00]
        );
    }

    /// Tests the pc-relative relocation.
    #[test]
    fn pcrel() {
        let mcode = assembleST20C1!(
            start:
                bcc start
                bcc main
            main:
                bcc start
        );
        assert_eq!(mcode, [0x90, 0xfe, 0x90, 0x00, 0x90, (-6i8) as u8,]);
    }

    /// We should assemble to true constant expressions that can be stored in `static`s or `const`s.
    #[test]
    fn const_expr() {
        const MCODE: &'static [u8] = &assembleST20C1!(
            ldx #0
            txa

        lbl:
            bcs lbl
        );

        assert_eq!(MCODE, [0xA2, 0x00, 0x8A, 0xB0, (-2i8) as u8,]);
    }

    #[test]
    fn code_start_attr() {
        let mcode = assembleST20C1! {{
            start: 0x8000,
            code: {
                start:
                    jmp start
                bla:
                    lda #0
                    jmp bla
            }
        }};
        assert_eq!(mcode, [0x4C, 0x00, 0x80, 0xA9, 0x00, 0x4C, 0x03, 0x80,]);
    }

    #[test]
    fn accumulator_addressing() {
        let mcode = assembleST20C1!(
            lsr a
            rol a
            ror a
        );
        assert_eq!(mcode, [0x4A, 0x2A, 0x6A]);
    }

    /// Has to work without any relocations (label references)
    #[test]
    fn no_reloc() {
        let mcode = assembleST20C1!(
            start:
                ldc8    #0xff
        );
        assert_eq!(mcode, [0x2f, 0x4f]);
    }

    #[test]
    fn adc() {
        let mcode = assembleST20C1!(
            adc     #0
            adc4    #1
            adc     #0xa
            adc     #15
            adc8    #0x5c
            adc16   #0x1234
            adc32   #0x12345678
        );
        assert_eq!(
            mcode,
            [
                0x80, // adc
                0x81, // adc4
                0x8a, // adc
                0x8f, // adc
                0x25, 0x8c, // adc8
                0x21, 0x22, 0x23, 0x84, // adc16
                0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x88, // adc32
            ]
        );
    }

    #[test]
    fn ldc() {
        let mcode = assembleST20C1!(
            ldc     #-1
            ldc     #0
            ldc4    #1
            ldc     #0xa
            ldc     #15
            ldc8    #0x5c
            ldc16   #0x1234
            ldc32   #0x12345678
        );
        assert_eq!(
            mcode,
            [
                0x6f, 0x4f, // ldc -1
                0x40, // ldc
                0x41, // ldc4
                0x4a, // ldc
                0x4f, // ldc
                0x25, 0x4c, // ldc8
                0x21, 0x22, 0x23, 0x44, // ldc16
                0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x48, // ldc32
            ]
        );
    }

    #[test]
    fn opr_primary() {
        let mcode = assembleST20C1!(
            rev
            dup
            rot
            arot
            add
            not
            breakpoint
        );
        assert_eq!(
            mcode,
            [
                0xf0, // rev
                0xf1, // dup
                0xf2, // rot
                0xf3, // arot
                0xf4, // add
                0xf8, // not
                0xff, // breakpoint
            ]
        );
    }
}
