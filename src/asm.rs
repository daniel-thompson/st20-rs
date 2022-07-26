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
    ( { $($attr:tt)* }  [ $( [ $($pos:expr),* ], [ $($rep:expr),* ] ),* ], $lblmap:ident, [ $($mcode:expr),* ], [ { $lbl:ident as PCREL0 @ [$($lockstepmcpos:expr),*] } $(,$reloc:tt)* ] ) => {
        // Replace 4 Byte with the PC relative address and jump instruction
        // Iptr is the program counter *after* the relocated offset (the length of the
        // `$lockstepmcpos` array + 1), so we need to subtract 1 additional byte.
        reloc!(
            { $($attr)* }
            [ $( [ $($pos),* ], [ $($rep),* ] ,)*
            [ $($lockstepmcpos),* ], [
                // Masking against 0x4f is essentially reading bit 18 as the sign bit and using it
                // to choose between nfix and pfix...
                ( 0x20 | ((( $lblmap!($lbl) as i32 - codelen!($($lockstepmcpos),*) as i32 - 4 ) >> 12) & 0x4f)) as u8,
                ( 0x20 | ((( $lblmap!($lbl) as i32 - codelen!($($lockstepmcpos),*) as i32 - 4 ) >> 8) & 0xf)) as u8,
                ( 0x20 | ((( $lblmap!($lbl) as i32 - codelen!($($lockstepmcpos),*) as i32 - 4 ) >> 4) & 0xf)) as u8,
                ( 0x00 | (( $lblmap!($lbl) as i32 - codelen!($($lockstepmcpos),*) as i32 - 4 ) & 0xf)) as u8
            ] ],
            $lblmap, [ $($mcode),* ], [ $($reloc),* ])
    };
    ( { $($attr:tt)* }  [ $( [ $($pos:expr),* ], [ $($rep:expr),* ] ),* ], $lblmap:ident, [ $($mcode:expr),* ], [ { $lbl:ident as PCREL9 @ [$($lockstepmcpos:expr),*] } $(,$reloc:tt)* ] ) => {
        // Replace 4 Byte with the PC relative address and jump instruction
        // Iptr is the program counter *after* the relocated offset (the length of the
        // `$lockstepmcpos` array + 1), so we need to subtract 1 additional byte.
        reloc!(
            { $($attr)* }
            [ $( [ $($pos),* ], [ $($rep),* ] ,)*
            [ $($lockstepmcpos),* ], [
                // Masking against 0x4f is essentially reading bit 18 as the sign bit and using it
                // to choose between nfix and pfix...
                ( 0x20 | ((( $lblmap!($lbl) as i32 - codelen!($($lockstepmcpos),*) as i32 - 4 ) >> 12) & 0x4f)) as u8,
                ( 0x20 | ((( $lblmap!($lbl) as i32 - codelen!($($lockstepmcpos),*) as i32 - 4 ) >> 8) & 0xf)) as u8,
                ( 0x20 | ((( $lblmap!($lbl) as i32 - codelen!($($lockstepmcpos),*) as i32 - 4 ) >> 4) & 0xf)) as u8,
                ( 0x90 | (( $lblmap!($lbl) as i32 - codelen!($($lockstepmcpos),*) as i32 - 4 ) & 0xf)) as u8
            ] ],
            $lblmap, [ $($mcode),* ], [ $($reloc),* ])
    };
    ( { $($attr:tt)* }  [ $( [ $($pos:expr),* ], [ $($rep:expr),* ] ),* ], $lblmap:ident, [ $($mcode:expr),* ], [ { $lbl:ident as PCRELA @ [$($lockstepmcpos:expr),*] } $(,$reloc:tt)* ] ) => {
        // Replace 4 Byte with the PC relative address and jump instruction
        // Iptr is the program counter *after* the relocated offset (the length of the
        // `$lockstepmcpos` array + 1), so we need to subtract 1 additional byte.
        reloc!(
            { $($attr)* }
            [ $( [ $($pos),* ], [ $($rep),* ] ,)*
            [ $($lockstepmcpos),* ], [
                // Masking against 0x4f is essentially reading bit 18 as the sign bit and using it
                // to choose between nfix and pfix...
                ( 0x20 | ((( $lblmap!($lbl) as i32 - codelen!($($lockstepmcpos),*) as i32 - 4 ) >> 12) & 0x4f)) as u8,
                ( 0x20 | ((( $lblmap!($lbl) as i32 - codelen!($($lockstepmcpos),*) as i32 - 4 ) >> 8) & 0xf)) as u8,
                ( 0x20 | ((( $lblmap!($lbl) as i32 - codelen!($($lockstepmcpos),*) as i32 - 4 ) >> 4) & 0xf)) as u8,
                ( 0xa0 | (( $lblmap!($lbl) as i32 - codelen!($($lockstepmcpos),*) as i32 - 4 ) & 0xf)) as u8
            ] ],
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
        adc # - $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x6f), (0x80 + (0x10 - $imm)) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        adc # $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x80 + $imm) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        adc4 # - $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x6f), (0x80 + (0x10 - $imm)) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        adc4 # $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x80 + $imm) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        adc8 # - $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x60 | (((0x100 - $imm) >> 4) & 0xf)) as u8, (0x80 | ((0x100 - $imm) & 0xf)) as u8 ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        adc8 # $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x20 | (($imm >> 4) & 0xf)), (0x80 | ($imm & 0xf)) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        adc16 # - $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x60 | (((0x10000 - $imm) >> 12) & 0xf)) as u8, (0x20 | (((0x10000 - $imm) >> 8) & 0xf)) as u8, (0x20 | (((0x10000 - $imm) >> 4) & 0xf)) as u8, (0x80 | ((0x10000 - $imm) & 0xf) as u8) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        adc16 # $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x20 | (($imm >> 12) & 0xf)) as u8, (0x20 | (($imm >> 8) & 0xf)) as u8, (0x20 | (($imm >> 4) & 0xf)) as u8, (0x80 | ($imm & 0xf) as u8) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        adc32 # - $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x68 | (((0x80000000_u32 - $imm) >> 28) & 0xf)) as u8, (0x20 | (((0x80000000_u32 - $imm) >> 24) & 0xf)) as u8, (0x20 | (((0x80000000_u32 - $imm) >> 20) & 0xf)) as u8, (0x20 | (((0x80000000_u32 - $imm) >> 16) & 0xf)) as u8, (0x20 | (((0x80000000_u32 - $imm) >> 12) & 0xf)) as u8, (0x20 | (((0x80000000_u32 - $imm) >> 8) & 0xf)) as u8, (0x20 | (((0x80000000_u32 - $imm) >> 4) & 0xf)) as u8, (0x80 | ((0x80000000_u32 - $imm) & 0xf)) as u8 ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
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

    // AJW
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ajw # - $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x6f), (0xb0 + (0x10 - $imm)) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ajw # $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0xb0 + $imm) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ajw4 # - $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x6f), (0xb0 + (0x10 - $imm)) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ajw4 # $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0xb0 + $imm) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ajw8 # - $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x60 | (((0x100 - $imm) >> 4) & 0xf)) as u8, (0xb0 | ((0x100 - $imm) & 0xf)) as u8 ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ajw8 # $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x20 | (($imm >> 4) & 0xf)), (0xb0 | ($imm & 0xf)) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ajw16 # - $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x60 | (((0x10000 - $imm) >> 12) & 0xf)) as u8, (0x20 | (((0x10000 - $imm) >> 8) & 0xf)) as u8, (0x20 | (((0x10000 - $imm) >> 4) & 0xf)) as u8, (0xb0 | ((0x10000 - $imm) & 0xf) as u8) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ajw16 # $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x20 | (($imm >> 12) & 0xf)) as u8, (0x20 | (($imm >> 8) & 0xf)) as u8, (0x20 | (($imm >> 4) & 0xf)) as u8, (0xb0 | ($imm & 0xf) as u8) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ajw32 # - $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x68 | (((0x80000000_u32 - $imm) >> 28) & 0xf)) as u8, (0x20 | (((0x80000000_u32 - $imm) >> 24) & 0xf)) as u8, (0x20 | (((0x80000000_u32 - $imm) >> 20) & 0xf)) as u8, (0x20 | (((0x80000000_u32 - $imm) >> 16) & 0xf)) as u8, (0x20 | (((0x80000000_u32 - $imm) >> 12) & 0xf)) as u8, (0x20 | (((0x80000000_u32 - $imm) >> 8) & 0xf)) as u8, (0x20 | (((0x80000000_u32 - $imm) >> 4) & 0xf)) as u8, (0xb0 | ((0x80000000_u32 - $imm) & 0xf)) as u8 ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ajw32 # $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x20 | (($imm >> 28) & 0xf)) as u8, (0x20 | (($imm >> 24) & 0xf)) as u8, (0x20 | (($imm >> 20) & 0xf)) as u8, (0x20 | (($imm >> 16) & 0xf)) as u8, (0x20 | (($imm >> 12) & 0xf)) as u8, (0x20 | (($imm >> 8) & 0xf)) as u8, (0x20 | (($imm >> 4) & 0xf)) as u8, (0xb0 | ($imm & 0xf)) as u8 ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };

    // AND
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        and
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* 0xF9 ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
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

    // CJ
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        cj $label:ident
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* 0x20, 0x00, 0x00, 0x00, 0x00 ],
            [ $($lbl => $lblval),* ], [ $($reloc,)* { $label as PCRELA @ [$($mcode,)* 0x20 ] } ], $($rest)*)
    };

    // DUP
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        dup
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* 0xF1 ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };

    // EQC
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        eqc # - $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x6f), (0xc0 + (0x10 - $imm)) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        eqc # $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0xc0 + $imm) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        eqc4 # - $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x6f), (0xc0 + (0x10 - $imm)) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        eqc4 # $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0xc0 + $imm) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        eqc8 # - $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x60 | (((0x100 - $imm) >> 4) & 0xf)) as u8, (0xc0 | ((0x100 - $imm) & 0xf)) as u8 ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        eqc8 # $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x20 | (($imm >> 4) & 0xf)), (0xc0 | ($imm & 0xf)) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        eqc16 # - $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x60 | (((0x10000 - $imm) >> 12) & 0xf)) as u8, (0x20 | (((0x10000 - $imm) >> 8) & 0xf)) as u8, (0x20 | (((0x10000 - $imm) >> 4) & 0xf)) as u8, (0xc0 | ((0x10000 - $imm) & 0xf) as u8) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        eqc16 # $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x20 | (($imm >> 12) & 0xf)) as u8, (0x20 | (($imm >> 8) & 0xf)) as u8, (0x20 | (($imm >> 4) & 0xf)) as u8, (0xc0 | ($imm & 0xf) as u8) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        eqc32 # - $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x68 | (((0x80000000_u32 - $imm) >> 28) & 0xf)) as u8, (0x20 | (((0x80000000_u32 - $imm) >> 24) & 0xf)) as u8, (0x20 | (((0x80000000_u32 - $imm) >> 20) & 0xf)) as u8, (0x20 | (((0x80000000_u32 - $imm) >> 16) & 0xf)) as u8, (0x20 | (((0x80000000_u32 - $imm) >> 12) & 0xf)) as u8, (0x20 | (((0x80000000_u32 - $imm) >> 8) & 0xf)) as u8, (0x20 | (((0x80000000_u32 - $imm) >> 4) & 0xf)) as u8, (0xc0 | ((0x80000000_u32 - $imm) & 0xf)) as u8 ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        eqc32 # $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x20 | (($imm >> 28) & 0xf)) as u8, (0x20 | (($imm >> 24) & 0xf)) as u8, (0x20 | (($imm >> 20) & 0xf)) as u8, (0x20 | (($imm >> 16) & 0xf)) as u8, (0x20 | (($imm >> 12) & 0xf)) as u8, (0x20 | (($imm >> 8) & 0xf)) as u8, (0x20 | (($imm >> 4) & 0xf)) as u8, (0xc0 | ($imm & 0xf)) as u8 ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };

    // FCALL
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        fcall $label:ident
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* 0x20, 0x00, 0x00, 0x00, 0x00 ],
            [ $($lbl => $lblval),* ], [ $($reloc,)* { $label as PCREL9 @ [$($mcode,)* 0x20 ] } ], $($rest)*)
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

    // JAB
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        jab
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* 0xFD ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };

    // J
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        j $label:ident
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* 0x20, 0x00, 0x00, 0x00, 0x00 ],
            [ $($lbl => $lblval),* ], [ $($reloc,)* { $label as PCREL0 @ [$($mcode,)* 0x20 ] } ], $($rest)*)
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

    // LDL
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ldl # - $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x6f), (0x70 + (0x10 - $imm)) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ldl # $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x70 + $imm) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ldl4 # - $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x6f), (0x70 + (0x10 - $imm)) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ldl4 # $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x70 + $imm) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ldl8 # - $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x60 | (((0x100 - $imm) >> 4) & 0xf)) as u8, (0x70 | ((0x100 - $imm) & 0xf)) as u8 ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ldl8 # $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x20 | (($imm >> 4) & 0xf)), (0x70 | ($imm & 0xf)) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ldl16 # - $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x60 | (((0x10000 - $imm) >> 12) & 0xf)) as u8, (0x20 | (((0x10000 - $imm) >> 8) & 0xf)) as u8, (0x20 | (((0x10000 - $imm) >> 4) & 0xf)) as u8, (0x70 | ((0x10000 - $imm) & 0xf) as u8) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ldl16 # $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x20 | (($imm >> 12) & 0xf)) as u8, (0x20 | (($imm >> 8) & 0xf)) as u8, (0x20 | (($imm >> 4) & 0xf)) as u8, (0x70 | ($imm & 0xf) as u8) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ldl32 # - $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x68 | (((0x80000000_u32 - $imm) >> 28) & 0xf)) as u8, (0x20 | (((0x80000000_u32 - $imm) >> 24) & 0xf)) as u8, (0x20 | (((0x80000000_u32 - $imm) >> 20) & 0xf)) as u8, (0x20 | (((0x80000000_u32 - $imm) >> 16) & 0xf)) as u8, (0x20 | (((0x80000000_u32 - $imm) >> 12) & 0xf)) as u8, (0x20 | (((0x80000000_u32 - $imm) >> 8) & 0xf)) as u8, (0x20 | (((0x80000000_u32 - $imm) >> 4) & 0xf)) as u8, (0x70 | ((0x80000000_u32 - $imm) & 0xf)) as u8 ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ldl32 # $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x20 | (($imm >> 28) & 0xf)) as u8, (0x20 | (($imm >> 24) & 0xf)) as u8, (0x20 | (($imm >> 20) & 0xf)) as u8, (0x20 | (($imm >> 16) & 0xf)) as u8, (0x20 | (($imm >> 12) & 0xf)) as u8, (0x20 | (($imm >> 8) & 0xf)) as u8, (0x20 | (($imm >> 4) & 0xf)) as u8, (0x70 | ($imm & 0xf)) as u8 ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };

    // LDLP
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ldlp # - $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x6f), (0x10 + (0x10 - $imm)) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ldlp # $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x10 + $imm) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ldlp4 # - $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x6f), (0x10 + (0x10 - $imm)) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ldlp4 # $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x10 + $imm) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ldlp8 # - $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x60 | (((0x100 - $imm) >> 4) & 0xf)) as u8, (0x10 | ((0x100 - $imm) & 0xf)) as u8 ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ldlp8 # $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x20 | (($imm >> 4) & 0xf)), (0x10 | ($imm & 0xf)) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ldlp16 # - $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x60 | (((0x10000 - $imm) >> 12) & 0xf)) as u8, (0x20 | (((0x10000 - $imm) >> 8) & 0xf)) as u8, (0x20 | (((0x10000 - $imm) >> 4) & 0xf)) as u8, (0x10 | ((0x10000 - $imm) & 0xf) as u8) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ldlp16 # $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x20 | (($imm >> 12) & 0xf)) as u8, (0x20 | (($imm >> 8) & 0xf)) as u8, (0x20 | (($imm >> 4) & 0xf)) as u8, (0x10 | ($imm & 0xf) as u8) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ldlp32 # - $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x68 | (((0x80000000_u32 - $imm) >> 28) & 0xf)) as u8, (0x20 | (((0x80000000_u32 - $imm) >> 24) & 0xf)) as u8, (0x20 | (((0x80000000_u32 - $imm) >> 20) & 0xf)) as u8, (0x20 | (((0x80000000_u32 - $imm) >> 16) & 0xf)) as u8, (0x20 | (((0x80000000_u32 - $imm) >> 12) & 0xf)) as u8, (0x20 | (((0x80000000_u32 - $imm) >> 8) & 0xf)) as u8, (0x20 | (((0x80000000_u32 - $imm) >> 4) & 0xf)) as u8, (0x10 | ((0x80000000_u32 - $imm) & 0xf)) as u8 ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ldlp32 # $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x20 | (($imm >> 28) & 0xf)) as u8, (0x20 | (($imm >> 24) & 0xf)) as u8, (0x20 | (($imm >> 20) & 0xf)) as u8, (0x20 | (($imm >> 16) & 0xf)) as u8, (0x20 | (($imm >> 12) & 0xf)) as u8, (0x20 | (($imm >> 8) & 0xf)) as u8, (0x20 | (($imm >> 4) & 0xf)) as u8, (0x10 | ($imm & 0xf)) as u8 ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };

    // LDNL
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ldnl # - $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x6f), (0x30 + (0x10 - $imm)) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ldnl # $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x30 + $imm) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ldnl4 # - $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x6f), (0x30 + (0x10 - $imm)) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ldnl4 # $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x30 + $imm) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ldnl8 # - $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x60 | (((0x100 - $imm) >> 4) & 0xf)) as u8, (0x30 | ((0x100 - $imm) & 0xf)) as u8 ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ldnl8 # $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x20 | (($imm >> 4) & 0xf)), (0x30 | ($imm & 0xf)) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ldnl16 # - $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x60 | (((0x10000 - $imm) >> 12) & 0xf)) as u8, (0x20 | (((0x10000 - $imm) >> 8) & 0xf)) as u8, (0x20 | (((0x10000 - $imm) >> 4) & 0xf)) as u8, (0x30 | ((0x10000 - $imm) & 0xf) as u8) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ldnl16 # $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x20 | (($imm >> 12) & 0xf)) as u8, (0x20 | (($imm >> 8) & 0xf)) as u8, (0x20 | (($imm >> 4) & 0xf)) as u8, (0x30 | ($imm & 0xf) as u8) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ldnl32 # - $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x68 | (((0x80000000_u32 - $imm) >> 28) & 0xf)) as u8, (0x20 | (((0x80000000_u32 - $imm) >> 24) & 0xf)) as u8, (0x20 | (((0x80000000_u32 - $imm) >> 20) & 0xf)) as u8, (0x20 | (((0x80000000_u32 - $imm) >> 16) & 0xf)) as u8, (0x20 | (((0x80000000_u32 - $imm) >> 12) & 0xf)) as u8, (0x20 | (((0x80000000_u32 - $imm) >> 8) & 0xf)) as u8, (0x20 | (((0x80000000_u32 - $imm) >> 4) & 0xf)) as u8, (0x30 | ((0x80000000_u32 - $imm) & 0xf)) as u8 ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ldnl32 # $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x20 | (($imm >> 28) & 0xf)) as u8, (0x20 | (($imm >> 24) & 0xf)) as u8, (0x20 | (($imm >> 20) & 0xf)) as u8, (0x20 | (($imm >> 16) & 0xf)) as u8, (0x20 | (($imm >> 12) & 0xf)) as u8, (0x20 | (($imm >> 8) & 0xf)) as u8, (0x20 | (($imm >> 4) & 0xf)) as u8, (0x30 | ($imm & 0xf)) as u8 ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };

    // LDNLP
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ldnlp # - $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x6f), (0x50 + (0x10 - $imm)) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ldnlp # $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x50 + $imm) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ldnlp4 # - $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x6f), (0x50 + (0x10 - $imm)) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ldnlp4 # $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x50 + $imm) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ldnlp8 # - $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x60 | (((0x100 - $imm) >> 4) & 0xf)) as u8, (0x50 | ((0x100 - $imm) & 0xf)) as u8 ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ldnlp8 # $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x20 | (($imm >> 4) & 0xf)), (0x50 | ($imm & 0xf)) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ldnlp16 # - $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x60 | (((0x10000 - $imm) >> 12) & 0xf)) as u8, (0x20 | (((0x10000 - $imm) >> 8) & 0xf)) as u8, (0x20 | (((0x10000 - $imm) >> 4) & 0xf)) as u8, (0x50 | ((0x10000 - $imm) & 0xf) as u8) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ldnlp16 # $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x20 | (($imm >> 12) & 0xf)) as u8, (0x20 | (($imm >> 8) & 0xf)) as u8, (0x20 | (($imm >> 4) & 0xf)) as u8, (0x50 | ($imm & 0xf) as u8) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ldnlp32 # - $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x68 | (((0x80000000_u32 - $imm) >> 28) & 0xf)) as u8, (0x20 | (((0x80000000_u32 - $imm) >> 24) & 0xf)) as u8, (0x20 | (((0x80000000_u32 - $imm) >> 20) & 0xf)) as u8, (0x20 | (((0x80000000_u32 - $imm) >> 16) & 0xf)) as u8, (0x20 | (((0x80000000_u32 - $imm) >> 12) & 0xf)) as u8, (0x20 | (((0x80000000_u32 - $imm) >> 8) & 0xf)) as u8, (0x20 | (((0x80000000_u32 - $imm) >> 4) & 0xf)) as u8, (0x50 | ((0x80000000_u32 - $imm) & 0xf)) as u8 ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ldnlp32 # $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x20 | (($imm >> 28) & 0xf)) as u8, (0x20 | (($imm >> 24) & 0xf)) as u8, (0x20 | (($imm >> 20) & 0xf)) as u8, (0x20 | (($imm >> 16) & 0xf)) as u8, (0x20 | (($imm >> 12) & 0xf)) as u8, (0x20 | (($imm >> 8) & 0xf)) as u8, (0x20 | (($imm >> 4) & 0xf)) as u8, (0x50 | ($imm & 0xf)) as u8 ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };

    // LDPI
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        ldpi
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* 0x23, 0xFA ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
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

    // MUL
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        mul
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* 0xF6 ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };

    // NOP
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        nop
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* 0x23, 0xFF ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };

    // NOT
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        not
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* 0xF8 ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };

    // OR
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        or
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* 0xFA ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
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

    // SHL
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        shl
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* 0xFB ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };

    // SHR
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        shr
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* 0xFC ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };

    // STL
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        stl # - $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x6f), (0xd0 + (0x10 - $imm)) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        stl # $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0xd0 + $imm) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        stl4 # - $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x6f), (0xd0 + (0x10 - $imm)) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        stl4 # $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0xd0 + $imm) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        stl8 # - $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x60 | (((0x100 - $imm) >> 4) & 0xf)) as u8, (0xd0 | ((0x100 - $imm) & 0xf)) as u8 ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        stl8 # $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x20 | (($imm >> 4) & 0xf)), (0xd0 | ($imm & 0xf)) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        stl16 # - $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x60 | (((0x10000 - $imm) >> 12) & 0xf)) as u8, (0x20 | (((0x10000 - $imm) >> 8) & 0xf)) as u8, (0x20 | (((0x10000 - $imm) >> 4) & 0xf)) as u8, (0xd0 | ((0x10000 - $imm) & 0xf) as u8) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        stl16 # $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x20 | (($imm >> 12) & 0xf)) as u8, (0x20 | (($imm >> 8) & 0xf)) as u8, (0x20 | (($imm >> 4) & 0xf)) as u8, (0xd0 | ($imm & 0xf) as u8) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        stl32 # - $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x68 | (((0x80000000_u32 - $imm) >> 28) & 0xf)) as u8, (0x20 | (((0x80000000_u32 - $imm) >> 24) & 0xf)) as u8, (0x20 | (((0x80000000_u32 - $imm) >> 20) & 0xf)) as u8, (0x20 | (((0x80000000_u32 - $imm) >> 16) & 0xf)) as u8, (0x20 | (((0x80000000_u32 - $imm) >> 12) & 0xf)) as u8, (0x20 | (((0x80000000_u32 - $imm) >> 8) & 0xf)) as u8, (0x20 | (((0x80000000_u32 - $imm) >> 4) & 0xf)) as u8, (0xd0 | ((0x80000000_u32 - $imm) & 0xf)) as u8 ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        stl32 # $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x20 | (($imm >> 28) & 0xf)) as u8, (0x20 | (($imm >> 24) & 0xf)) as u8, (0x20 | (($imm >> 20) & 0xf)) as u8, (0x20 | (($imm >> 16) & 0xf)) as u8, (0x20 | (($imm >> 12) & 0xf)) as u8, (0x20 | (($imm >> 8) & 0xf)) as u8, (0x20 | (($imm >> 4) & 0xf)) as u8, (0xd0 | ($imm & 0xf)) as u8 ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };

    // STNL
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        stnl # - $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x6f), (0xe0 + (0x10 - $imm)) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        stnl # $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0xe0 + $imm) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        stnl4 # - $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x6f), (0xe0 + (0x10 - $imm)) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        stnl4 # $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0xe0 + $imm) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        stnl8 # - $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x60 | (((0x100 - $imm) >> 4) & 0xf)) as u8, (0xe0 | ((0x100 - $imm) & 0xf)) as u8 ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        stnl8 # $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x20 | (($imm >> 4) & 0xf)), (0xe0 | ($imm & 0xf)) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        stnl16 # - $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x60 | (((0x10000 - $imm) >> 12) & 0xf)) as u8, (0x20 | (((0x10000 - $imm) >> 8) & 0xf)) as u8, (0x20 | (((0x10000 - $imm) >> 4) & 0xf)) as u8, (0xe0 | ((0x10000 - $imm) & 0xf) as u8) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        stnl16 # $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x20 | (($imm >> 12) & 0xf)) as u8, (0x20 | (($imm >> 8) & 0xf)) as u8, (0x20 | (($imm >> 4) & 0xf)) as u8, (0xe0 | ($imm & 0xf) as u8) ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        stnl32 # - $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x68 | (((0x80000000_u32 - $imm) >> 28) & 0xf)) as u8, (0x20 | (((0x80000000_u32 - $imm) >> 24) & 0xf)) as u8, (0x20 | (((0x80000000_u32 - $imm) >> 20) & 0xf)) as u8, (0x20 | (((0x80000000_u32 - $imm) >> 16) & 0xf)) as u8, (0x20 | (((0x80000000_u32 - $imm) >> 12) & 0xf)) as u8, (0x20 | (((0x80000000_u32 - $imm) >> 8) & 0xf)) as u8, (0x20 | (((0x80000000_u32 - $imm) >> 4) & 0xf)) as u8, (0xe0 | ((0x80000000_u32 - $imm) & 0xf)) as u8 ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        stnl32 # $imm:tt
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* (0x20 | (($imm >> 28) & 0xf)) as u8, (0x20 | (($imm >> 24) & 0xf)) as u8, (0x20 | (($imm >> 20) & 0xf)) as u8, (0x20 | (($imm >> 16) & 0xf)) as u8, (0x20 | (($imm >> 12) & 0xf)) as u8, (0x20 | (($imm >> 8) & 0xf)) as u8, (0x20 | (($imm >> 4) & 0xf)) as u8, (0xe0 | ($imm & 0xf)) as u8 ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };

    // SUB
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        sub
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* 0xF5 ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };

    // TIMESLICE
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        timeslice
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* 0xFE ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };

    // TXA
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        txa
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* 0x8A ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
    };

    // WSUB
    ( { $($attr:tt)* } [ $($mcode:expr),* ], [ $($lbl:ident => $lblval:expr),* ], [ $($reloc:tt),* ],
        wsub
    $($rest:tt)* ) => {
        asm_!({ $($attr)* } [ $($mcode,)* 0xF7 ], [ $($lbl => $lblval),* ], [ $($reloc),* ], $($rest)*)
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
            adc     #-1
            adc4    #-1
            adc8    #-1
            adc16    #-1
            adc32    #-1
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
                0x6f, 0x8f, // adc
                0x6f, 0x8f, // adc4
                0x6f, 0x8f, // adc8
                0x6f, 0x2f, 0x2f, 0x8f, // adc16
                0x6f, 0x2f, 0x2f, 0x2f, 0x2f, 0x2f, 0x2f, 0x8f, // adc32
            ]
        );
    }

    #[test]
    fn ajw() {
        let mcode = assembleST20C1!(
            ajw     #0
            ajw4    #1
            ajw     #0xa
            ajw     #15
            ajw8    #0x5c
            ajw16   #0x1234
            ajw32   #0x12345678
            ajw     #-1
            ajw4    #-1
            ajw8    #-1
            ajw16    #-1
            ajw32    #-1
        );
        assert_eq!(
            mcode,
            [
                0xb0, // ajw
                0xb1, // ajw4
                0xba, // ajw
                0xbf, // ajw
                0x25, 0xbc, // ajw8
                0x21, 0x22, 0x23, 0xb4, // ajw16
                0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0xb8, // ajw32
                0x6f, 0xbf, // ajw
                0x6f, 0xbf, // ajw4
                0x6f, 0xbf, // ajw8
                0x6f, 0x2f, 0x2f, 0xbf, // ajw16
                0x6f, 0x2f, 0x2f, 0x2f, 0x2f, 0x2f, 0x2f, 0xbf, // ajw32
            ]
        );
    }

    #[test]
    fn cj() {
        let mcode = assembleST20C1!(
        start:
            cj start
            cj main
            ldpi
        main:
            cj start
        );
        assert_eq!(
            mcode,
            [
                0x20, 0x6f, 0x2f, 0x2f, 0xab, // cj start
                0x20, 0x20, 0x20, 0x20, 0xa2, // cj main
                0x23, 0xfa, // ldpi
                0x20, 0x6f, 0x2f, 0x2e, 0xaf, // cj start
            ]
        );
    }

    #[test]
    fn eqc() {
        let mcode = assembleST20C1!(
            eqc     #0
            eqc4    #1
            eqc     #0xa
            eqc     #15
            eqc8    #0x5c
            eqc16   #0x1234
            eqc32   #0x12345678
            eqc     #-1
            eqc4    #-1
            eqc8    #-1
            eqc16   #-1
            eqc32   #-1
        );
        assert_eq!(
            mcode,
            [
                0xc0, // eqc
                0xc1, // eqc4
                0xca, // eqc
                0xcf, // eqc
                0x25, 0xcc, // eqc8
                0x21, 0x22, 0x23, 0xc4, // eqc16
                0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0xc8, // eqc32
                0x6f, 0xcf, // eqc
                0x6f, 0xcf, // eqc4
                0x6f, 0xcf, // eqc8
                0x6f, 0x2f, 0x2f, 0xcf, // eqc16
                0x6f, 0x2f, 0x2f, 0x2f, 0x2f, 0x2f, 0x2f, 0xcf, // eqc32
            ]
        );
    }

    #[test]
    fn fcall() {
        let mcode = assembleST20C1!(
        start:
            fcall start
            fcall main
            ldpi
        main:
            fcall start
        );
        assert_eq!(
            mcode,
            [
                0x20, 0x6f, 0x2f, 0x2f, 0x9b, // fcall start
                0x20, 0x20, 0x20, 0x20, 0x92, // fcall main
                0x23, 0xfa, // ldpi
                0x20, 0x6f, 0x2f, 0x2e, 0x9f, // fcall start
            ]
        );
    }

    #[test]
    fn ldc() {
        let mcode = assembleST20C1!(
            ldc32   #-1
            ldc16   #-1
            ldc8    #-1
            ldc4    #-1
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
                0x6f, 0x2f, 0x2f, 0x2f, 0x2f, 0x2f, 0x2f, 0x4f, // ldc32 -1
                0x6f, 0x2f, 0x2f, 0x4f, // ldc16 -1
                0x6f, 0x4f, // ldc8 -1
                0x6f, 0x4f, // ldc4 -1
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
    fn ldl() {
        let mcode = assembleST20C1!(
            ldl     #0
            ldl4    #1
            ldl     #0xa
            ldl     #15
            ldl8    #0x5c
            ldl16   #0x1234
            ldl32   #0x12345678
            ldl     #-1
            ldl4    #-1
            ldl8    #-1
            ldl16   #-1
            ldl32   #-1
        );
        assert_eq!(
            mcode,
            [
                0x70, // ldl
                0x71, // ldl4
                0x7a, // ldl
                0x7f, // ldl
                0x25, 0x7c, // ldl8
                0x21, 0x22, 0x23, 0x74, // ldl16
                0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x78, // ldl32
                0x6f, 0x7f, // ldl
                0x6f, 0x7f, // ldl4
                0x6f, 0x7f, // ldl8
                0x6f, 0x2f, 0x2f, 0x7f, // ldl16
                0x6f, 0x2f, 0x2f, 0x2f, 0x2f, 0x2f, 0x2f, 0x7f, // ldl32
            ]
        );
    }

    #[test]
    fn ldlp() {
        let mcode = assembleST20C1!(
            ldlp    #0
            ldlp4   #1
            ldlp    #0xa
            ldlp    #15
            ldlp8   #0x5c
            ldlp16  #0x1234
            ldlp32  #0x12345678
            ldlp    #-1
            ldlp4   #-1
            ldlp8   #-1
            ldlp16  #-1
            ldlp32  #-1
        );
        assert_eq!(
            mcode,
            [
                0x10, // ldlp
                0x11, // ldlp4
                0x1a, // ldlp
                0x1f, // ldlp
                0x25, 0x1c, // ldlp8
                0x21, 0x22, 0x23, 0x14, // ldlp16
                0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x18, // ldlp32
                0x6f, 0x1f, // ldlp
                0x6f, 0x1f, // ldlp4
                0x6f, 0x1f, // ldlp8
                0x6f, 0x2f, 0x2f, 0x1f, // ldlp16
                0x6f, 0x2f, 0x2f, 0x2f, 0x2f, 0x2f, 0x2f, 0x1f, // ldlp32
            ]
        );
    }

    #[test]
    fn ldnl() {
        let mcode = assembleST20C1!(
            ldnl    #0
            ldnl4   #1
            ldnl    #0xa
            ldnl    #15
            ldnl8   #0x5c
            ldnl16  #0x1234
            ldnl32  #0x12345678
            ldnl    #-1
            ldnl4   #-1
            ldnl8   #-1
            ldnl16  #-1
            ldnl32  #-1
        );
        assert_eq!(
            mcode,
            [
                0x30, // ldnl
                0x31, // ldnl4
                0x3a, // ldnl
                0x3f, // ldnl
                0x25, 0x3c, // ldnl8
                0x21, 0x22, 0x23, 0x34, // ldnl16
                0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x38, // ldnl32
                0x6f, 0x3f, // ldnl
                0x6f, 0x3f, // ldnl4
                0x6f, 0x3f, // ldnl8
                0x6f, 0x2f, 0x2f, 0x3f, // ldnl16
                0x6f, 0x2f, 0x2f, 0x2f, 0x2f, 0x2f, 0x2f, 0x3f, // ldnl32
            ]
        );
    }

    #[test]
    fn ldnlp() {
        let mcode = assembleST20C1!(
            ldnlp   #0
            ldnlp4   #1
            ldnlp    #0xa
            ldnlp    #15
            ldnlp8   #0x5c
            ldnlp16  #0x1234
            ldnlp32  #0x12345678
            ldnlp    #-1
            ldnlp4   #-1
            ldnlp8   #-1
            ldnlp16  #-1
            ldnlp32  #-1
        );
        assert_eq!(
            mcode,
            [
                0x50, // ldnlp
                0x51, // ldnlp4
                0x5a, // ldnlp
                0x5f, // ldnlp
                0x25, 0x5c, // ldnlp8
                0x21, 0x22, 0x23, 0x54, // ldnlp16
                0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x58, // ldnlp32
                0x6f, 0x5f, // ldnlp
                0x6f, 0x5f, // ldnlp4
                0x6f, 0x5f, // ldnlp8
                0x6f, 0x2f, 0x2f, 0x5f, // ldnlp16
                0x6f, 0x2f, 0x2f, 0x2f, 0x2f, 0x2f, 0x2f, 0x5f, // ldnlp32
            ]
        );
    }

    #[test]
    fn j() {
        let mcode = assembleST20C1!(
        start:
            j start
            j main
            ldpi
        main:
            j start
        );
        assert_eq!(
            mcode,
            [
                0x20, 0x6f, 0x2f, 0x2f, 0x0b, // j start
                0x20, 0x20, 0x20, 0x20, 0x02, // j main
                0x23, 0xfa, // ldpi
                0x20, 0x6f, 0x2f, 0x2e, 0x0f, // j start
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
            sub
            mul
            wsub
            not
            and
            or
            shl
            shr
            jab
            timeslice
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
                0xf5, // sub
                0xf6, // mul
                0xf7, // wsub
                0xf8, // not
                0xf9, // and
                0xfa, // or
                0xfb, // shl
                0xfc, // shr
                0xfd, // jab
                0xfe, // timeslice
                0xff, // breakpoint
            ]
        );
    }

    #[test]
    fn opr_secondary() {
        let mcode = assembleST20C1!(
            ldpi
            nop
        );
        assert_eq!(
            mcode,
            [
                0x23, 0xfa, // ldpi
                0x23, 0xff, // nop
            ]
        );
    }

    #[test]
    fn stl() {
        let mcode = assembleST20C1!(
            stl     #0
            stl4    #1
            stl     #0xa
            stl     #15
            stl8    #0x5c
            stl16   #0x1234
            stl32   #0x12345678
            stl     #-1
            stl4    #-1
            stl8    #-1
            stl16   #-1
            stl32   #-1
        );
        assert_eq!(
            mcode,
            [
                0xd0, // stl
                0xd1, // stl4
                0xda, // stl
                0xdf, // stl
                0x25, 0xdc, // stl8
                0x21, 0x22, 0x23, 0xd4, // stl16
                0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0xd8, // stl32
                0x6f, 0xdf, // stl
                0x6f, 0xdf, // stl4
                0x6f, 0xdf, // stl8
                0x6f, 0x2f, 0x2f, 0xdf, // stl16
                0x6f, 0x2f, 0x2f, 0x2f, 0x2f, 0x2f, 0x2f, 0xdf, // stl32
            ]
        );
    }

    #[test]
    fn stnl() {
        let mcode = assembleST20C1!(
            stnl     #0
            stnl4    #1
            stnl     #0xa
            stnl     #15
            stnl8    #0x5c
            stnl16   #0x1234
            stnl32   #0x12345678
            stnl     #-1
            stnl4    #-1
            stnl8    #-1
            stnl16   #-1
            stnl32   #-1
        );
        assert_eq!(
            mcode,
            [
                0xe0, // stnl
                0xe1, // stnl4
                0xea, // stnl
                0xef, // stnl
                0x25, 0xec, // stnl8
                0x21, 0x22, 0x23, 0xe4, // stnl16
                0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0xe8, // stnl32
                0x6f, 0xef, // stnl
                0x6f, 0xef, // stnl4
                0x6f, 0xef, // stnl8
                0x6f, 0x2f, 0x2f, 0xef, // stnl16
                0x6f, 0x2f, 0x2f, 0x2f, 0x2f, 0x2f, 0x2f, 0xef, // stnl32
            ]
        );
    }
}
