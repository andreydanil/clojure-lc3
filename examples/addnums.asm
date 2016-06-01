	.ORIG	x3000
	AND	R1, R1, x0	;clear R1, to be used for the running sum
	AND	R4, R4, x0	;clear R4, to be used as a counter
	ADD	R4, R4, xA	;load R4 with 10(hex:A), the number of times to add
LOOP	LDR	R2, R2, x0
	ADD	R2, R2, x1	;increment the pointer
	ADD	R1, R1, R3	;add the next number to the running sum
	ADD	R4, R4, x-1	;decrement the counter
	BRp	LOOP		;do it again if the counter is still positive
	HALT
	.END
