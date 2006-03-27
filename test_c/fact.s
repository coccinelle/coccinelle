	.data
	.globl	Fact_data_begin
Fact_data_begin:
	.text
	.globl	Fact_code_begin
Fact_code_begin:
	.data
	.long	1024
	.globl	Fact
Fact:
	.space	4
	.data
	.long	2298
	.globl	Fact_1
Fact_1:
	.long	Fact_fact_49
	.long	3
	.text
	.align	16
	.globl	Fact_fact_49
Fact_fact_49:
	subl	$4, %esp
.L101:
	cmpl	$1, %eax
	jne	.L100
	movl	$3, %eax
	addl	$4, %esp
	ret
	.align	16
.L100:
	movl	%eax, 0(%esp)
	addl	$-2, %eax
	call	Fact_fact_49
.L102:
	movl	%eax, %ebx
	sarl	$1, %ebx
	movl	0(%esp), %eax
	decl	%eax
	imull	%ebx, %eax
	incl	%eax
	addl	$4, %esp
	ret
	.text
	.align	16
	.globl	Fact_entry
Fact_entry:
.L103:
	movl	$Fact_1, %eax
	movl	%eax, Fact
	movl	$1, %eax
	ret
	.text
	.globl	Fact_code_end
Fact_code_end:
	.data
	.globl	Fact_data_end
Fact_data_end:
	.long	0
	.globl	Fact_frametable
Fact_frametable:
	.long	1
	.long	.L102
	.word	8
	.word	1
	.word	0
	.align	4
