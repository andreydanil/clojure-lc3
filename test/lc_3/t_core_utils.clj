(ns lc_3.t-core-utils
  (:use midje.sweet)
  (:use [lc_3.core]))

(fact "about expt"
	; TODO: it seems to hang with negative exponents..not that we care that much
	; TODO: Should I even check for negative numbers?
	(fact "works generally"
		(expt 64 3) => 262144
		(expt 3 3) => 27
		(expt 2 0) => 1
		(expt 2 1) => 2
		(expt 1 0) => 1
		(expt 1 1) => 1
		(expt 1 2) => 1
		(expt 1 10) => 1
	)
	(fact "works with zero"
		(expt 0 2) => 0
		(expt 0 0) => 1 ;TODO: Are we sure it is supposed to be like that?
	)
)

(fact "about get-bits"
	(get-bits 2r0010100011000010 15 0) => 2r0010100011000010
	(get-bits 2r0010100011000010 16 0) => 2r0010100011000010
	(get-bits 2r0010100011000010 2 0) => 2r010
	(get-bits 2r0010100011000010 7 2) => 2r110000
	(get-bits 2r0010100011000010 1 1) => 2r1
	(get-bits 2r0010100011000010 0 0) => 2r0
	(get-bits 2r0010100011000010 17 16) => 2r0
)

(fact "about get-bits-2c"
	(fact "general 2's complement"
		(get-bits-2c 2r0001 0 0) => (- 1) ; TODO: Is it supposed to work that way?
		(get-bits-2c 2r0000 3 0) => 0
		(get-bits-2c 2r1000 3 0) => (- 8)
		(get-bits-2c 2r1111 3 0) => (- 1)
		(get-bits-2c 2r0010100011000010 15 0) => 2r0010100011000010
		(get-bits-2c 2r1010100011000010 15 0) => (- 2r0101011100111110)
	)

	(fact "2's complement from mask"
		(get-bits-2c 2r0001 1 0) => 1
		(get-bits-2c 2r0101 2 0) => (- 3)
		(get-bits-2c 2r0101 3 0) => 5
		(get-bits-2c 2r0010100011000010 6 2) => (- 16)
	)
)
