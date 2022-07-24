	.arch armv7
	.fpu neon-vfpv4
	.syntax unified
	.thumb
	.text

/* ST20C1 context */
#define rAreg	r1
#define rBreg	r2
#define rCreg	r3
#define rIptr	r4
#define rStatus	r5
#define rWptr	sp
//#define rTdesc	r10
//#define rIOreg	r11

/* Misc */
#define rScratch r0
#define rDispatch r6
#define rAcc	r7
#define rMemBase r10

/* Saved context */
#define rArmSP	r8
#define rC1Ctx	r9

	.align	2
	.global	st20c1_run
	.type	st20c1_run, %function
	.thumb_func
st20c1_run:
	push	{r4-r11, lr}
	mov	rC1Ctx, rScratch
	mov	rArmSP, sp

	/*
	 * load ST20C1 state
	 *
	 * TODO:can use ldmia one we have finalize register allocation
	 */
	ldr	rAreg, [rScratch]
	ldr	rBreg, [rScratch, #4]
	ldr	rCreg, [rScratch, #8]
	ldr	rIptr, [rScratch, #12]
	ldr	rStatus, [rScratch, #16]
	ldr	rWptr, [rScratch, #20]
	//ldr	rTdesc, [rScratch, #24]
	//ldr	rIOreg, [rScratch, #28]
	ldr	rMemBase, [rScratch, #32]
	add	rIptr, rIptr, rMemBase
	adr	rDispatch, dispatch+1	/* +1 to stay in thumb mode */
	mov	rAcc, #0

.macro	fetchexec
	ldrb	rScratch, [rIptr], #1	/* 32-bit opcode */
	lsl	rScratch, #5
	add	rScratch, rDispatch
	bx	rScratch
	.align	5
.endm

	fetchexec

st20c1_restore:
	/*
	 * save ST20C1 state
	 */
	mov	rScratch, rC1Ctx
	str	rAreg, [rScratch]
	str	rBreg, [rScratch, #4]
	str	rCreg, [rScratch, #8]
	sub	rIptr, rIptr, rMemBase
	str	rIptr, [rScratch, #12]
	str	rStatus, [rScratch, #16]
	str	rWptr, [rScratch, #20]
	//str	rTdesc, [rScratch, #24]
	//str	rIOreg, [rScratch, #28]

	mov	r0, #0			/* success! */
	mov	sp, rArmSP
	pop	{r4-r11, pc}

	.align	5
dispatch:
.macro	j	operand
	b	panic
	fetchexec
.endm
j:
	j	0
	j	1
	j	2
	j	3
	j	4
	j	5
	j	6
	j	7
	j	8
	j	9
	j	10
	j	11
	j	12
	j	13
	j	14
	j	15

.macro	ldlp	operand
	b	panic
	fetchexec
.endm
ldlp:
	ldlp	0
	ldlp	1
	ldlp	2
	ldlp	3
	ldlp	4
	ldlp	5
	ldlp	6
	ldlp	7
	ldlp	8
	ldlp	9
	ldlp	10
	ldlp	11
	ldlp	12
	ldlp	13
	ldlp	14
	ldlp	15

.macro	pfix operand
	lsl	rAcc, rAcc, #4
	orr	rAcc, rAcc, #\operand << 4
	fetchexec
.endm
pfix:
	pfix	0
	pfix	1
	pfix	2
	pfix	3
	pfix	4
	pfix	5
	pfix	6
	pfix	7
	pfix	8
	pfix	9
	pfix	10
	pfix	11
	pfix	12
	pfix	13
	pfix	14
	pfix	15

.macro	ldnl	operand
	b	panic
	fetchexec
.endm
ldnl:
	ldnl	0
	ldnl	1
	ldnl	2
	ldnl	3
	ldnl	4
	ldnl	5
	ldnl	6
	ldnl	7
	ldnl	8
	ldnl	9
	ldnl	10
	ldnl	11
	ldnl	12
	ldnl	13
	ldnl	14
	ldnl	15

.macro	ldc	operand
	mov	rCreg, rBreg
	mov	rBreg, rAreg
	add	rAreg, rAcc, #\operand
	mov	rAcc, #0
	fetchexec
.endm
ldc:
	ldc	0
	ldc	1
	ldc	2
	ldc	3
	ldc	4
	ldc	5
	ldc	6
	ldc	7
	ldc	8
	ldc	9
	ldc	10
	ldc	11
	ldc	12
	ldc	13
	ldc	14
	ldc	15

.macro	ldnlp	operand
	b	panic
	fetchexec
.endm
ldnlp:
	ldnlp	0
	ldnlp	1
	ldnlp	2
	ldnlp	3
	ldnlp	4
	ldnlp	5
	ldnlp	6
	ldnlp	7
	ldnlp	8
	ldnlp	9
	ldnlp	10
	ldnlp	11
	ldnlp	12
	ldnlp	13
	ldnlp	14
	ldnlp	15

.macro	nfix	operand
	lsl	rAcc, rAcc, #4
	mov	rScratch, #0xffffff00
	eor	rAcc, rScratch
	add	rAcc, rAcc, #\operand << 4
	fetchexec
.endm
nfix:
	nfix	0
	nfix	1
	nfix	2
	nfix	3
	nfix	4
	nfix	5
	nfix	6
	nfix	7
	nfix	8
	nfix	9
	nfix	10
	nfix	11
	nfix	12
	nfix	13
	nfix	14
	nfix	15

.macro	ldl	operand
	b	panic
	fetchexec
.endm
ldl:
	ldl	0
	ldl	1
	ldl	2
	ldl	3
	ldl	4
	ldl	5
	ldl	6
	ldl	7
	ldl	8
	ldl	9
	ldl	10
	ldl	11
	ldl	12
	ldl	13
	ldl	14
	ldl	15

.macro	adc	operand
	add	rAcc, rAcc, #\operand
	add	rAreg, rAreg, rAcc
	mov	rAcc, #0
	fetchexec
.endm
adc:
	adc	0
	adc	1
	adc	2
	adc	3
	adc	4
	adc	5
	adc	6
	adc	7
	adc	8
	adc	9
	adc	10
	adc	11
	adc	12
	adc	13
	adc	14
	adc	15

.macro	fcall	operand
	b	panic
	fetchexec
.endm
fcall:
	fcall	0
	fcall	1
	fcall	2
	fcall	3
	fcall	4
	fcall	5
	fcall	6
	fcall	7
	fcall	8
	fcall	9
	fcall	10
	fcall	11
	fcall	12
	fcall	13
	fcall	14
	fcall	15

.macro	cj	operand
	b	panic
	fetchexec
.endm
cj:
	cj	0
	cj	1
	cj	2
	cj	3
	cj	4
	cj	5
	cj	6
	cj	7
	cj	8
	cj	9
	cj	10
	cj	11
	cj	12
	cj	13
	cj	14
	cj	15

.macro	ajw	operand
	b	panic
	fetchexec
.endm
ajw:
	ajw	0
	ajw	1
	ajw	2
	ajw	3
	ajw	4
	ajw	5
	ajw	6
	ajw	7
	ajw	8
	ajw	9
	ajw	10
	ajw	11
	ajw	12
	ajw	13
	ajw	14
	ajw	15

.macro	eqc	operand
	b	panic
	fetchexec
.endm
eqc:
	eqc	0
	eqc	1
	eqc	2
	eqc	3
	eqc	4
	eqc	5
	eqc	6
	eqc	7
	eqc	8
	eqc	9
	eqc	10
	eqc	11
	eqc	12
	eqc	13
	eqc	14
	eqc	15

.macro	stl	operand
	b	panic
	fetchexec
.endm
stl:
	stl	0
	stl	1
	stl	2
	stl	3
	stl	4
	stl	5
	stl	6
	stl	7
	stl	8
	stl	9
	stl	10
	stl	11
	stl	12
	stl	13
	stl	14
	stl	15

.macro	stnl	operand
	b	panic
	fetchexec
.endm
stnl:
	stnl	0
	stnl	1
	stnl	2
	stnl	3
	stnl	4
	stnl	5
	stnl	6
	stnl	7
	stnl	8
	stnl	9
	stnl	10
	stnl	11
	stnl	12
	stnl	13
	stnl	14
	stnl	15

.macro	opr	operand
	b	panic
	fetchexec
.endm
opr:
	opr	0		// rev, addc, xor, swinc
	opr	1		// dup, subc,
	opr	2		// rot,
	opr	3		// arot,

add:				// (opr4) or ...
	cbnz	rAcc, 1f
	// rAcc is already zero so we don't need to consume or zero it
	mov	rScratch, rBreg
	mov	rBreg, rCreg
	mov	rCreg, rAreg
	add	rAreg, rAreg, rScratch
	fetchexec
1:
	b	panic

	opr	5		// sub,
	opr	6		// mul,
	opr	7		// wsub,

not:				// (opr 8) or ...
	cbnz	rAcc, 1f
	mvn	rAreg, rAreg
	fetchexec
1:
	b	panic

	opr	9		// and,
	opr	10		// or,
	opr	11		// shl,
	opr	12		// shr,
	opr	13		// jab,
	opr	14		// timeslice,

breakpoint:			// (opr F) or ashr, lwinc, nop
	cbnz	rAcc, panic
	b	st20c1_restore
	fetchexec

panic:
	mov	rScratch, rC1Ctx
	str	rAreg, [rScratch]
	str	rBreg, [rScratch, #4]
	str	rCreg, [rScratch, #8]
	sub	rIptr, rIptr, rMemBase
	str	rIptr, [rScratch, #12]
	str	rStatus, [rScratch, #16]
	str	rWptr, [rScratch, #20]
	//str	rTdesc, [rScratch, #24]
	//str	rIOreg, [rScratch, #28]

	mov	r0, #-1			/* ouch! */
	mov	sp, rArmSP
	pop	{r4-r11, pc}
