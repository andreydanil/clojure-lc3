(ns lc_3.t-core-instr
  (:use midje.sweet)
  (:use [lc_3.core]))

; = Instructions for testing
(defn addi-instr  [] { :op :add-i :dr 1 :sr1 1 :imm5 5 })
(defn add-instr   [] { :op :add :dr 1 :sr1 1 :sr2 1 })
(defn and-instr   [] { :op :and :dr 2 :sr1 1 :sr2 0 })
(defn andi-instr  [] { :op :and-i :dr 3 :sr1 1 :imm5 7 })
(defn br-instr    [] { :op :br :pcoffset9 0x30 })
(defn brn-instr   [] { :op :brn :pcoffset9 0x30 })
(defn brz-instr   [] { :op :brz :pcoffset9 0x30 })
(defn brp-instr   [] { :op :brp :pcoffset9 0x30 })
(defn brzp-instr  [] { :op :brzp :pcoffset9 0x30 })
(defn brnp-instr  [] { :op :brnp :pcoffset9 0x30 })
(defn brnz-instr  [] { :op :brnz :pcoffset9 0x30 })
(defn brnzp-instr [] { :op :brnzp :pcoffset9 0x30 })
(defn jmp-instr   [] { :op :jmp :baser 1 })
(defn ret-instr   [] { :op :ret })
(defn jsr-instr   [] { :op :jsr :pcoffset11 0x30 })
(defn jsrr-instr  [] { :op :jsrr :baser 1 })
(defn ld-instr    [] { :op :ld :dr 0 :pcoffset9 0x2 })
(defn ldi-instr   [] { :op :ldi :dr 0 :pcoffset9 0x2 })
(defn ldr-instr   [] { :op :ldr :dr 0 :baser 3 :offset6 0x2 })
(defn lea-instr   [] { :op :lea :dr 0 :pcoffset9 0x2 })
(defn not-instr   [] { :op :not :dr 5 :sr 0})
(defn rti-instr   [] { :op :rti })
(defn st-instr    [] { :op :st :sr 0 :pcoffset9 0x2 })
(defn sti-instr   [] { :op :sti :sr 0 :pcoffset9 0x2 })
(defn str-instr   [] { :op :str :sr 0 :baser 3 :offset6 0x1 })
; (defn trap-instr [] {})

(fact "test encode-word"
  (encode-word (addi-instr))  => 2r0001001001100101
  (encode-word (add-instr))   => 2r0001001001000001
  (encode-word (and-instr))   => 2r0101010001000000
  (encode-word (andi-instr))  => 2r0101011001100111
  (encode-word (br-instr))    => 2r0000000000110000
  (encode-word (brn-instr))   => 2r0000100000110000
  (encode-word (brz-instr))   => 2r0000010000110000
  (encode-word (brp-instr))   => 2r0000001000110000
  (encode-word (brzp-instr))  => 2r0000011000110000
  (encode-word (brnp-instr))  => 2r0000101000110000
  (encode-word (brnz-instr))  => 2r0000110000110000
  (encode-word (brnzp-instr)) => 2r0000111000110000
  (encode-word (jmp-instr))   => 2r1100000001000000
  (encode-word (ret-instr))   => 2r1100000111000000
  (encode-word (jsr-instr))   => 2r0100100000110000
  (encode-word (jsrr-instr))  => 2r0100000001000000
  (encode-word (ld-instr))    => 2r0010000000000010
  (encode-word (ldi-instr))   => 2r1010000000000010
  (encode-word (ldr-instr))   => 2r0110000011000010
  (encode-word (lea-instr))   => 2r1110000000000010
  (encode-word (not-instr))   => 2r1001101000111111
  (encode-word (rti-instr))   => 2r1000000000000000
  (encode-word (st-instr))    => 2r0011000000000010
  (encode-word (sti-instr))   => 2r1011000000000010
  (encode-word (str-instr))   => 2r0111000011000001
)

(fact "test decode-word"
  (decode-word 2r0001001001100101) => (addi-instr)
  (decode-word 2r0100100000110000) => (jsr-instr)
  (decode-word 2r1001101000111111) => (not-instr)
)

(fact "test program counter and branching"
	(let [x {:pc 0}]
	  (fact "increment pc"
			(:pc x) => 0x0
			(:pc (inc-pc x)) => 0x1
		)
		(fact "branch instructions"
		  (fact "test RET instruction"
				(let [y (merge x {:mem [(encode-word (ret-instr))]
				                  :registers [0, 0, 0, 0, 0, 0, 0, 0x3000]})]
				  (:pc (exec y)) => 0x3000
				)
			)
		  (fact "test BR instruction"
				(let [y (merge x {:mem [(encode-word (br-instr))]
				                  :registers [0, 0, 0, 0, 0, 0, 0, 0]})]
				  (:pc (exec y)) => 0x31
				)
			)
		  (fact "test BRN instruction"
				(let [y (merge x {:mem [(encode-word (brn-instr))]
				                  :registers [0, 0, 0, 0, 0, 0, 0, 0]})]
           (let [z (merge y {:psr 2r100})]
				      (:pc (exec z)) => 0x31
           )
           (let [z (merge y {:psr 2r000})]
				      (:pc (exec z)) => 0x1
           )
				)
			)
		  (fact "test BRZ instruction"
				(let [y (merge x {:mem [(encode-word (brz-instr))]
				                  :registers [0, 0, 0, 0, 0, 0, 0, 0]})]
           (let [z (merge y {:psr 2r010})]
				      (:pc (exec z)) => 0x31
           )
           (let [z (merge y {:psr 2r000})]
				      (:pc (exec z)) => 0x1
           )
				)
			)
		  (fact "test BRP instruction"
				(let [y (merge x {:mem [(encode-word (brp-instr))]
				                  :registers [0, 0, 0, 0, 0, 0, 0, 0]})]
           (let [z (merge y {:psr 2r001})]
				      (:pc (exec z)) => 0x31
           )
           (let [z (merge y {:psr 2r000})]
				      (:pc (exec z)) => 0x1
           )
				)
			)
		  (fact "test BRNP instruction"
				(let [y (merge x {:mem [(encode-word (brnp-instr))]
				                  :registers [0, 0, 0, 0, 0, 0, 0, 0]})]
           (let [z (merge y {:psr 2r001})]
				      (:pc (exec z)) => 0x31
           )
           (let [z (merge y {:psr 2r100})]
				      (:pc (exec z)) => 0x31
           )
           (let [z (merge y {:psr 2r010})]
				      (:pc (exec z)) => 0x1
           )
				)
			)
		  (fact "test BRZP instruction"
				(let [y (merge x {:mem [(encode-word (brzp-instr))]
				                  :registers [0, 0, 0, 0, 0, 0, 0, 0]})]
           (let [z (merge y {:psr 2r001})]
				      (:pc (exec z)) => 0x31
           )
           (let [z (merge y {:psr 2r010})]
				      (:pc (exec z)) => 0x31
           )
           (let [z (merge y {:psr 2r100})]
				      (:pc (exec z)) => 0x1
           )
				)
			)
		  (fact "test BRNZ instruction"
				(let [y (merge x {:mem [(encode-word (brnz-instr))]
				                  :registers [0, 0, 0, 0, 0, 0, 0, 0]})]
           (let [z (merge y {:psr 2r100})]
				      (:pc (exec z)) => 0x31
           )
           (let [z (merge y {:psr 2r010})]
				      (:pc (exec z)) => 0x31
           )
           (let [z (merge y {:psr 2r001})]
				      (:pc (exec z)) => 0x1
           )
				)
			)
		  (fact "test BRNZP instruction"
				(let [y (merge x {:mem [(encode-word (brnzp-instr))]
				                  :registers [0, 0, 0, 0, 0, 0, 0, 0]})]
           (let [z (merge y {:psr 2r100})]
				      (:pc (exec z)) => 0x31
           )
           (let [z (merge y {:psr 2r010})]
				      (:pc (exec z)) => 0x31
           )
           (let [z (merge y {:psr 2r001})]
				      (:pc (exec z)) => 0x31
           )
           (let [z (merge y {:psr 2r000})]
				      (:pc (exec z)) => 0x31 ; Is it supposed to work this way?
           )
				)
			)
		  (fact "test JMP instruction"
				(let [y (merge x {:mem [(encode-word (jmp-instr))]
				                  :registers [0, 0x3000]})]
				  (:pc (exec y)) => 0x3000
				)
			)
		)
    (fact "JSR and JSRR instructions"
      (fact "test JSR instruction"
        (let [y (merge x {:mem [(encode-word (jsr-instr))]
                          :registers [0, 0, 0, 0, 0, 0, 0, 0]
                          :psr 0x0})]
           (:pc (exec y)) => 0x31
           (get-register (exec y) 7) => 0x1
           (:psr (exec y)) => 0x1 ; TODO: Need to research this psr situation
        )
      )
      (fact "test JSRR instruction"
        (let [y (merge x {:mem [(encode-word (jsrr-instr))]
                          :registers [0, 0x30, 0, 0, 0, 0, 0, 0]
                          :psr 0x0})]
           (:pc (exec y)) => 0x30
           (get-register (exec y) 7) => 0x1
           (:psr (exec y)) => 0x1 ; TODO: Need to research this psr situation
        )
      )
    )
	)
)

(fact "test memory operations"
	(let [x {:pc 0 :psr 0}]
	  (fact "test LD instruction"
		  (let [y (merge x {:mem [(encode-word (ld-instr)), 0x0, 0x1, 0xFFFF, 0x0]
			                  :registers [0]})]
			  (get-register (exec y) 0) => 0xFFFF
        (:pc (exec y)) => 1
        (:psr (exec y)) => 2r100
			)
		)
	  (fact "test LDI instruction"
		  (let [y (merge x {:mem [(encode-word (ldi-instr)), 0x0, 0xFF, 0x2, 0x0]
			                  :registers [0]})]
			  (get-register (exec y) 0) => 0xFF
        (:pc (exec y)) => 1
        (:psr (exec y)) => 2r100
			)
		)
	  (fact "test LDR instruction"
		  (let [y (merge x {:mem [(encode-word (ldr-instr)), 0x1, 0x2, 0x3, 0x12, 0x4]
			                  :registers [0, 0, 0, 0x2]})]
			  (get-register (exec y) 0) => 0x12
        (:pc (exec y)) => 1
        (:psr (exec y)) => 2r001
			)
		)
	  (fact "test LEA instruction"
		  (let [y (merge x {:mem [(encode-word (lea-instr)), 0x0]
			                  :registers [0, 0, 0, 0, 0, 0, 0, 0, 0]})]
			  (get-register (exec y) 0) => 0x3
        (:pc (exec y)) => 1
        (:psr (exec y)) => 2r001
			)
		)
	  (fact "test ST instruction"
		  (let [y (merge x {:mem [(encode-word (st-instr)), 0x0A, 0xB, 0xC, 0xD]
			                  :registers [0xFF]})]
			  (get (:mem (exec y)) 3) => 0xFF
        (:pc (exec y)) => 1
        (:psr (exec y)) => 2r000
			)
		)
	  (fact "test STI instruction"
		  (let [y (merge x {:mem [(encode-word (sti-instr)), 0x1, 0x1, 0x2, 0x1]
			                  :registers [0x1F]})]
			  (get (:mem (exec y)) 2) => 0x1F
        (:pc (exec y)) => 1
        (:psr (exec y)) => 2r000
			)
		)
	  (fact "test STR instruction"
		  (let [y (merge x {:mem [(encode-word (str-instr)), 0x0, 0x0, 0x0, 0x0]
			                  :registers [0xFF, 0, 0, 0x1]})]
			  (get (:mem (exec y)) 2) => 0xFF
        (:pc (exec y)) => 1
        (:psr (exec y)) => 2r000
			)
		)
  )
)

(fact "test other operations"
  (let [x {:pc 0 :psr 0}]
	  (fact "test ADD instruction"
		  (let [y (merge x {:mem [(encode-word (add-instr))]
			                  :registers [0, 0x5]})]
			  (get-register (exec y) 1) => 0xA
        (:pc (exec y)) => 1
        (:psr (exec y)) => 2r001
			)
		)
	  (fact "test ADDi instruction"
		  (let [y (merge x {:mem [(encode-word (addi-instr))]
			                  :registers [0, 0x5]})]
			  (get-register (exec y) 1) => 0xA
        (:pc (exec y)) => 1
        (:psr (exec y)) => 2r001
			)
		)
	  (fact "test AND instruction"
		  (let [y (merge x {:mem [(encode-word (and-instr))]
			                  :registers [0x0FF0, 0xFF00, 0]})]
			  (get-register (exec y) 2) => 0xF00
        (:pc (exec y)) => 1
        (:psr (exec y)) => 2r001
			)
		)
	  (fact "test ANDi instruction"
		  (let [y (merge x {:mem [(encode-word (andi-instr))]
			                  :registers [0, 0x6, 0, 0]})]
			  (get-register (exec y) 3) => 0x6
        (:pc (exec y)) => 1
        (:psr (exec y)) => 2r001
			)
		)
	  (fact "test NOT instruction"
		  (let [y (merge x {:mem [(encode-word (not-instr))]
			                  :registers [0x2, 0, 0, 0, 0, 0]})]
			  (get-register (exec y) 5) => (- 3) ; Need to change that with a two-complement func
        (:pc (exec y)) => 1
        (:psr (exec y)) => 2r100
			)
		)
	)
)
