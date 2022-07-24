st20-rs
=======

Using a modern language to implement an obsolete instruction set!

st20-rs is an incomplete emulation of an ST20-C1. The ST20-C1 can be
described as a transputer with the instruction set re-encoded and with
the os-links removed. Of course anyone who knows anything about the
transputer knows that a transputer without an os-link isn't a
transputer anymore.

Having invoked the spirit of the transputer I should add that, st20-rs
doesn't actually have any lofty computing history ideals. This was just
a weekend project so I could benchmark some bytecode interpreters;
reusing the ST20-C1 instruction set spared me the effort of designing my
own bytecode and allowed me to get the job done.

Note that assemblers and C compilers do exist for the ST20-C1 but I've
done everything by hand because I no longer have access to them. That is
a shame since I would be interested in gathering dhrystone so if anyone
can help me build a binary for that then drop me a line!

Quickstart
----------

    sudo apt install gcc-arm-linux-gnueabihf
    rustup target add armv7-unknown-linux-gnueabihf
    cargo build
    cargo test
    cargo bench
    cargo run
