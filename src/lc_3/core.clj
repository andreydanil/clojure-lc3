(ns lc_3.core
  (:require [clojure.string :as str]
            [clojure.pprint :refer [cl-format]]))

(def memory (vec  (repeat 1024 0)))

;; Found this from Karl Rosaen's post on Stack Overflow
;;
(defn expt [b n]
  (let [inner (fn [a b n]
                (cond
                  (= n 0) a
                  (even? n) (recur a (* b b) (/ n 2))
                  :else (recur (* a b) b (- n 1))))
        ]
    (inner 1 b n)))

(defn get-bits [w hi lo]
  (let [size (- hi lo -1)
        sw   (bit-shift-right w lo)
        mask (- (bit-shift-left 1 size) 1)]
    (bit-and sw mask)))

(def get-bit bit-test)

(defn get-bits-2c [w hi lo]
  (let [size (- hi lo)
        sw   (bit-shift-right w lo)
        mask (- (bit-shift-left 1 size) 1)
        bits (bit-and sw mask)
        neg? (bit-test w hi)]
    (if neg?
      (- bits (expt 2 size))
      bits)) )

(defn make-bits [w lo size]
  (let [mask (- (bit-shift-left 1 size) 1)
        masked-word (bit-and mask w)]
    (bit-shift-left masked-word lo)))

(defn make-bit [w lo]
  (make-bits w lo 1))

(defn get-opcode [w]
  (get-bits w 15 12))
(defn get-dr [w]
  (get-bits w 11 9))
(defn get-sr1 [w]
  (get-bits w 8 6))
(defn get-sr2 [w]
  (get-bits w 2 0))
(defn get-immediate [w]
  (get-bits w 5 5))
(defn get-imm5 [w]
  (get-bits-2c w 4 0))
(defn get-pcoffset9 [w]
  (get-bits-2c w 8 0))
(defn get-pcoffset11 [w]
  (get-bits-2c w 10 0))
(defn get-offset6 [w]
  (get-bits-2c w 5 0))
(defn get-n [w]
  (get-bits w 11 11))
(defn get-z [w]
  (get-bits w 10 10))
(defn get-p [w]
  (get-bits w  9  9))

(defn make-dr [w]
  (make-bits w 9 3))
(defn make-sr1 [w]
  (make-bits w 6 3))
(defn make-sr2 [w]
  (make-bits w 0 3))
(defn make-imm5 [w]
  (make-bits w 0 5))
(defn make-immediate [w]
  (make-bits w 5 1))
(defn make-opcode [w]
  (make-bits w 12 4))
(defn make-pcoffset9 [w]
  (make-bits w 0 9))
(defn make-pcoffset11 [w]
  (make-bits w 0 11))
(defn make-offset6 [w]
  (make-bits w 0 6))
(defn make-n [w]
  (make-bits w 11 1))
(defn make-z [w]
  (make-bits w 10 1))
(defn make-p [w]
  (make-bits w  9 1))
(defn make-trapvect8 [w]
  (make-bits w 0 8))


(defn encode-word [op]
  (case (:op op)
    :add-i  (bit-or (make-opcode 2r0001)
                    (make-immediate 1)
                    (make-dr (:dr op))
                    (make-sr1 (:sr1 op))
                    (make-imm5 (:imm5 op)))
    :add    (bit-or (make-opcode 2r0001)
                    (make-dr (:dr op))
                    (make-sr1 (:sr1 op))
                    (make-sr2 (:sr2 op)))
    :and-i  (bit-or (make-opcode 2r0101)
                    (make-immediate 1)
                    (make-dr (:dr op))
                    (make-sr1 (:sr1 op))
                    (make-imm5 (:imm5 op)))
    :and    (bit-or (make-opcode 2r0101)
                    (make-dr (:dr op))
                    (make-sr1 (:sr1 op))
                    (make-sr2 (:sr2 op)))
    :br     (bit-or (make-opcode 2r0000)
                    (make-pcoffset9 (:pcoffset9 op)))
    :brn    (bit-or (make-opcode 2r0000)
                    (make-n 1)
                    (make-pcoffset9 (:pcoffset9 op)))
    :brz    (bit-or (make-opcode 2r0000)
                    (make-z 1)
                    (make-pcoffset9 (:pcoffset9 op)))
    :brp    (bit-or (make-opcode 2r0000)
                    (make-p 1)
                    (make-pcoffset9 (:pcoffset9 op)))
    :brzp   (bit-or (make-opcode 2r0000)
                    (make-z 1)
                    (make-p 1)
                    (make-pcoffset9 (:pcoffset9 op)))
    :brnp   (bit-or (make-opcode 2r0000)
                    (make-n 1)
                    (make-p 1)
                    (make-pcoffset9 (:pcoffset9 op)))
    :brnz   (bit-or (make-opcode 2r0000)
                    (make-n 1)
                    (make-z 1)
                    (make-pcoffset9 (:pcoffset9 op)))
    :brnzp  (bit-or (make-opcode 2r0000)
                    (make-n 1)
                    (make-z 1)
                    (make-p 1)
                    (make-pcoffset9 (:pcoffset9 op)))
    :jmp    (bit-or (make-opcode 2r1100)
                    (make-sr1 (:baser op)))
    :jsr    (bit-or (make-opcode 2r0100)
                    (make-bits 1 11 1)
                    (make-pcoffset11 (:pcoffset11 op)))
    :jsrr   (bit-or (make-opcode 2r0100)
                    (make-sr1 (:baser op)))
    :ld     (bit-or (make-opcode 2r0010)
                    (make-dr (:dr op))
                    (make-pcoffset9 (:pcoffset9 op)))
    :ldi    (bit-or (make-opcode 2r1010)
                    (make-dr (:dr op))
                    (make-pcoffset9 (:pcoffset9 op)))
    :ldr    (bit-or (make-opcode 2r0110)
                    (make-dr (:dr op))
                    (make-sr1 (:baser op))
                    (make-offset6 (:offset6 op)))
    :lea    (bit-or (make-opcode 2r1110)
                    (make-dr (:dr op))
                    (make-pcoffset9 (:pcoffset9 op)))
    :not    (bit-or (make-opcode 2r1001)
                    (make-dr (:dr op))
                    (make-sr1 (:sr op))
                    (make-offset6 2r111111))
    :ret    (bit-or (make-opcode 2r1100)
                    (make-sr1 7))
    :rti    (bit-or (make-opcode 2r1000)
		                (make-bits 0 11 1)
										(make-pcoffset11 2r00000000000))
    :st     (bit-or (make-opcode 2r0011)
                    (make-dr (:sr op))  ;; SR is stored in the DR bits
                    (make-pcoffset9 (:pcoffset9 op)))
    :sti    (bit-or (make-opcode 2r1011)
                    (make-dr (:sr op))
                    (make-pcoffset9 (:pcoffset9 op)))
    :str    (bit-or (make-opcode 2r0111)
                    (make-dr (:sr op))
                    (make-sr1 (:baser op))
                    (make-offset6 (:offset6 op)))
    :trap   (bit-or (make-opcode 2r1111)
                    (make-trapvect8 (:trapvect8 op)))))

(def br-map
  {2r000 [:br    "BR"]
   2r001 [:brp   "BRp"]
   2r010 [:brz   "BRz"]
   2r011 [:brzp  "BRzp"]
   2r100 [:brn   "BRn"]
   2r101 [:brnp  "BRnp"]
   2r110 [:brnz  "BRnz"]
   2r111 [:brnzp "BRnzp"] })

(defn decode-word [w]
  (case (get-bits w 15 12)
    2r0001 (if (zero? (get-immediate w))
             {:op  :add
              :dr  (get-dr w)
              :sr1 (get-sr1 w)
              :sr2 (get-sr2 w)}
             {:op  :add-i
              :dr  (get-dr w)
              :sr1 (get-sr1 w)
              :imm5 (get-imm5 w)})
    2r0101 (if (zero? (get-immediate w))
             {:op  :and
              :dr  (get-dr w)
              :sr1 (get-sr1 w)
              :sr2 (get-sr2 w)}
             {:op  :and-i
              :dr  (get-dr w)
              :sr1 (get-sr1 w)
              :imm5 (get-imm5 w)})
    2r0000 (let [[br-op br-opcode] (br-map (get-bits w 11 9))]
             {:op br-op
              :pcoffset9 (get-pcoffset9 w)} )
    2r1100 (let [baser (get-sr1 w)]
             (if (= baser 7)
               {:op :ret }
               {:op :jmp
                :baser baser}))
    2r0100 (if (false? (get-bit w 11))
             ;; JSRR
             {:op :jsrr
              :baser (get-sr1 w)}
             {:op :jsr
              :pcoffset11 (get-pcoffset11 w)})
    2r0010 {:op :ld
            :dr (get-dr w)
						:pcoffset9 (get-pcoffset9 w)
            }
    2r1010 {:op :ldi
            :dr (get-dr w)
            :pcoffset9 (get-pcoffset9 w)
            }
    2r0110 {:op :ldr
            :dr (get-dr w)
            :baser (get-sr1 w)
            :offset6 (get-offset6 w)
            }
    2r1110 {:op :lea
            :dr (get-dr w)
            :pcoffset9 (get-pcoffset9 w)
            }
    2r1001 {:op :not
            :dr (get-dr w)
            :sr (get-sr1 w)
            }
		2r1000 {:op :rti
					 	}
    2r0011 {:op :st
            :sr (get-dr w)
            :pcoffset9 (get-pcoffset9 w)
            }
    2r1011 {:op :sti
            :sr (get-dr w)
            :pcoffset9 (get-pcoffset9 w)
            }
    2r0111 {:op :str
            :sr (get-dr w)
            :baser (get-sr1 w)
            :offset6 (get-offset6 w)
            }
    2r1111 {:op :trap
            :trapvect8 (get-bits w 7 0)
            }
    ))

(defn show-record
  "Show a canonical textual representation for a given record."
  [w]
  (case (:op w)
    :add   (str "ADD r"  (:dr w) ", r" (:sr1 w) ", r" (:sr2 w))
    :add-i (str "ADD r"  (:dr w) ", r" (:sr1 w) ", " (:imm5 w))
    :and   (str "AND r"  (:dr w) ", r" (:sr1 w) ", r" (:sr2 w))
    :and-i (str "AND r"  (:dr w) ", r" (:sr1 w) ", " (:imm5 w))
    :br    (str "BR "    (:pcoffset9 w))
    :brp   (str "BRp "   (:pcoffset9 w))
    :brz   (str "BRz "   (:pcoffset9 w))
    :brzp  (str "BRzp "  (:pcoffset9 w))
    :brn   (str "BRn "   (:pcoffset9 w))
    :brnp  (str "BRnp "  (:pcoffset9 w))
    :brnz  (str "BRnz "  (:pcoffset9 w))
    :brnzp (str "BRnzp " (:pcoffset9 w))
    :jmp   (str "JMP "   (:baser w))
		:jsr   (str "JSR "   (:pcoffset11 w))
		:jsrr  (str "JSRR "  (:baser w))
		:ld    (str "LD r"   (:dr w) ", " (:pcoffset9 w))
		:ldi   (str "LDI r"  (:dr w) ", " (:pcoffset9 w))
		:ldr   (str "LDR r"  (:dr w) ", " (:baser w) ", " (:pcoffset6 w))
		:lea   (str "LEA r"  (:dr w) ", " (:pcoffset9 w))
		:not   (str "NOT r"  (:dr w) ", r" (:sr w))
    :ret   "RET"
    :rti   "RTI"
		:st    (str "ST r"   (:sr w) ", " (:pcoffset9 w))
		:sti   (str "STI r"  (:sr w) ", " (:pcoffset9 w))
		:str   (str "STR r"  (:sr w) ", " (:baser w) ", " (:pcoffset6 w))
		:trap  (str "TRAP " (:trapvect8 w))
    ))

(defn show-word [x]
  (-> x decode-word show-record))

(defmacro acond
  "Like 'cond', but binds the result of each test to 'symbol' in the
  expression body."
  [symbol & clauses]
  (if (not-empty clauses)
    `(if-let [~symbol ~(first clauses)]
       ~(second clauses)
       (acond ~symbol ~@(rest (rest clauses))) )
    nil))

(def add-re  #"add\s+r([0-7]),\s*r([0-7]),\s*r([0-7])")
(def addi-re #"addi\s+r([0-7]),\s*r([0-7]),\s*(-?[0-9]+)")
(def and-re  #"and\s+r([0-7]),\s*r([0-7]),\s*r([0-7])")
(def andi-re #"andi\s+r([0-7]),\s*r([0-7]),\s*([0-9]+)")
(def br-re   #"br(n?)(z?)(p?)\s+(-?[0-9]+)")
(def jmp-re  #"jmp\s+r([0-7])")
(def jsr-re  #"jsr\s+(-?[0-9]+)")
(def jsrr-re #"jsrr\s+r([0-7])")
(def ld-re   #"ld\s+r([0-7]),\s+([0-9]+)")
(def ldi-re  #"ldi\s+r([0-7]),\s+([0-9]+)")
(def ldr-re  #"ldr\s+r([0-7]),\s*r([0-7]),\s*([0-9]+)")
(def lea-re  #"lea\s+r([0-7]),\s+([0-9]+)")
(def not-re  #"not\s+r([0-7]),\s*r([0-7])")
(def ret-re  #"ret")
(def rti-re  #"rti")
(def st-re   #"st\s+r([0-7]),\s+([0-9]+)")
(def sti-re  #"sti\s+r([0-7]),\s+([0-9]+)")
(def trap-re #"trap\s+(0?)x([0-9]+)") ; TODO: Need to think this through and correct this

(declare bool-to-bit)

(defn read-word [s]
  (acond result
         (re-matches add-re s) {:op :add :opcode "ADD" :dr (read-string (result 1))
                                :sr1 (read-string (result 2))
                                :sr2 (read-string (result 3))}
         (re-matches addi-re s) {:op :add-i :opcode "ADD" :dr (read-string (result 1))
                                 :sr1 (read-string (result 2))
                                 :imm5 (read-string (result 3))}
         (re-matches and-re s) {:op :and :opcode "AND" :dr (read-string (result 1))
                                :sr1 (read-string (result 2))
                                :sr2 (read-string (result 3))}
         (re-matches andi-re s) {:op :and-i :opcode "AND" :dr (read-string (result 1))
                                 :sr1 (read-string (result 2))
                                 :imm5 (read-string (result 3))}
         (re-matches br-re s)  (let [[op opcode] (br-map (+ (* 4 (bool-to-bit (= "n" (result 1))))
                                                            (* 2 (bool-to-bit (= "z" (result 2))))
                                                            (bool-to-bit (= "p" (result 3)))))]
                                 {:op op
                                  :opcode opcode
                                  :pcoffset9 (read-string (result 4))})
         (re-matches jmp-re s) (if (= "7" (result 1))
                                      {:op :ret
                                       :opcode "RET"}
                                      {:op :jmp
                                       :opcode "JMP"
                                       :baser (read-string (result 1))})
         (re-matches jsr-re s) {:op :jsr
				 												:opcode "JSR"
																:pcoffset11 (read-string (result 1))}
         (re-matches jsrr-re s) {:op :jsrr
				 												 :opcode "JSRR"
																 :baser (read-string (result 1))}
         (re-matches ld-re s) {:op :ld
				 											 :opcode "LD"
															 :dr (read-string (result 1))
															 :pcoffset9 (read-string (result 2))}
				 (re-matches ldi-re s) {:op :ldi
				 												:opcode "LDI"
										  				  :dr (read-string (result 1))
															  :pcoffset9 (read-string (result 2))}
				 (re-matches ldr-re s) {:op :ldr
				 												:opcode "LDR"
																:dr (read-string (result 1))
																:baser (read-string (result 2))
																:offset6 (read-string (result 3))}
				 (re-matches lea-re s) {:op :lea
				 												:opcode "LEA"
																:dr (read-string (result 1))
															  :pcoffset9 (read-string (result 2))}
         (re-matches not-re s) {:op :not
				 											 :opcode "NOT"
															 :dr (read-string (result 1))
															 :sr (read-string (result 2))}
         (re-matches ret-re s) {:op :ret
                                :opcode "RET"} ; TODO: twice?
         (re-matches rti-re s) {:op :rti
                                :opcode "RTI"}
         (re-matches st-re s) {:op :st
				 											 :opcode "ST"
															 :sr (read-string (result 1))
															 :pcoffset9 (read-string (result 2))}
         (re-matches sti-re s) {:op :sti
				 											 :opcode "STI"
															 :sr (read-string (result 1))
															 :pcoffset9 (read-string (result 2))}
         (re-matches trap-re s) {:op :trap
				 											   :opcode "TRAP"
																 :trapvect8 (read-string (result 1))}
         ))

(defn mk-init-state []
  {:mar 0 ;; The Memory Address Register
   :mdr 0 ;; The Memory Data Register
   :kbdr 0 ;; Keyboard Data
   :kbsr 0 ;; Keyboard Status
   :ddr 0 ;; Monitor Data
   :dsr 0 ;; Monitor Status
   :registers [0 0 0 0 0 0 0 0]
   :pc 0x3000 ;; Program Counter
   :ir 0 ;; Instruction Register
   :psr 0 ;; Process Status Register
   :mem (vec (repeat 0xffff 0))
   :usp 0 ;; User Stack Pointer
   :ssp 0 ;; Supervisor Stack Pointer
   })

(defn load-program
  ([state program] (load-program state program 0x3000))
  ([state program start]
   (assoc state :mem
          (loop [mem (transient (:mem state))
                 pidx 0
                 midx start]
            (if (< pidx (count program))
              (recur (assoc! mem midx (program pidx))
                     (inc pidx)
                     (inc midx))
              (persistent! mem))) ) ))

(defn get-psr-n [content]
  (bit-test content 2))

(defn get-psr-z [content]
  (bit-test content 1))

(defn get-psr-p [content]
  (bit-test content 0))

(defn set-psr-n [content bit]
  (if (zero? bit)
    (bit-clear content 2)
    (bit-set content 2)))

(defn set-psr-z [content bit]
  (if (zero? bit)
    (bit-clear content 1)
    (bit-set content 1)))

(defn set-psr-p [content bit]
  (if (zero? bit)
    (bit-clear content 0)
    (bit-set content 0)))


(defn get-register [state r]
  ((state :registers) r))

(defn bool-to-bit [x]
  (if x 1 0))

(defn set-condition-codes
  "Set the condition codes based on content."
  [state register]
  (let [content (get-in state [:registers register])
        psr (state :psr)
        z (bool-to-bit (zero? content))
        n (if (= z 1) 0 (get-bits content 5 5))
        p (if (= z 1) 0 (- 1 n))]
    (assoc state
           :psr
           (-> psr
               (set-psr-z z)
               (set-psr-n n)
               (set-psr-p p)))))

(defn set-register [state register content]
  (set-condition-codes (assoc-in state [:registers register] content) register))

(defn inc-pc [state]
  (update-in state [:pc] inc))

(defn exec [state]
  (let [memory (:mem state)
        op (->> :pc state memory decode-word)]
    (case (:op op)
      :add-i (inc-pc (set-register state
                                   (:dr op)
                                   (+ (get-register state (:sr1 op))
                                      (:imm5 op))))
      :add   (inc-pc (set-register state
                                   (:dr op)
                                   (+ (get-register state (:sr1 op))
                                      (get-register state (:sr2 op)))))

      :and-i (inc-pc (set-register state
                                   (:dr op)
                                   (bit-and (get-register state (:sr1 op))
                                            (:imm5 op))))
      :and   (inc-pc (set-register state
                                   (:dr op)
                                   (bit-and (get-register state (:sr1 op))
                                            (get-register state (:sr2 op)))))
      :br    (assoc state :pc (+ 1 (:pc state) (:pcoffset9 op)))
      :brn   (if (get-psr-n (:psr state))
               (assoc state :pc (+ 1 (:pc state) (:pcoffset9 op)))
               (assoc state :pc (inc (:pc state))))
      :brz   (if (get-psr-z (:psr state))
               (assoc state :pc (+ 1 (:pc state) (:pcoffset9 op)))
               (assoc state :pc (inc (:pc state))))
      :brp   (if (get-psr-p (:psr state))
               (assoc state :pc (+ 1 (:pc state) (:pcoffset9 op)))
               (assoc state :pc (inc (:pc state))))
      :brnp  (if (or (get-psr-n (:psr state)) (get-psr-p (:psr state)))
               (assoc state :pc (+ 1 (:pc state) (:pcoffset9 op)))
               (assoc state :pc (inc (:pc state))))
      :brzp  (if (or (get-psr-z (:psr state)) (get-psr-p (:psr state)))
               (assoc state :pc (+ 1 (:pc state) (:pcoffset9 op)))
               (assoc state :pc (inc (:pc state))))
      :brnz  (if (or (get-psr-n (:psr state)) (get-psr-z (:psr state)))
               (assoc state :pc (+ 1 (:pc state) (:pcoffset9 op)))
               (assoc state :pc (inc (:pc state))))
      :brnzp (assoc state :pc (+ 1 (:pc state) (:pcoffset9 op)))

      :ret   (assoc state :pc ((:registers state) 7) )
      :jmp   (assoc state :pc (get-in state [:registers (:baser op)]))
			:jsr   (assoc (set-register state 7 (+ 1 (:pc state))) :pc (+ 1 (:pc state) (:pcoffset11 op)))
			:jsrr  (assoc (set-register state 7 (+ 1 (:pc state))) :pc (get-in state [:registers (:baser op)]))
			:ld    (inc-pc (set-register state
			                             (:dr op)
																	 (get memory (+ 1 (:pc state) (:pcoffset9 op)))))
			:ldi   (inc-pc (set-register state
			                             (:dr op)
																	 (get memory (get memory (+ 1 (:pc state) (:pcoffset9 op))))))
			:ldr   (inc-pc (set-register state
			                             (:dr op)
																	 (get memory (+ (get-register state (:baser op)) (:offset6 op)))))
			:lea   (inc-pc (set-register state
			                             (:dr op)
                                   (+ 1 (:pc state) (:pcoffset9 op))))
			:not   (inc-pc (set-register state
			                             (:dr op)
																	 (bit-not (get-register state (:sr op))))) ; just flip bits or 2's comp?!?
	;		:rti   ()
			:st    (inc-pc (-> state (assoc-in [:mem (+ 1 (:pc state) (:pcoffset9 op))]
                                         (get-register state (:sr op)))))
			:sti   (inc-pc (-> state (assoc-in [:mem (get memory (+ 1 (:pc state) (:pcoffset9 op)))]
			                                   (get-register state (:sr op)))))
			:str   (inc-pc (-> state (assoc-in [:mem (+ (get-register state (:baser op)) (:offset6 op))]
			                                   (get-register state (:sr op)))))
;			:trap  ()
      :nop state
      )


    ))

(defn runtimes [state n]
  (loop [st0 state
         ct n]
    (cl-format true "0x~x [~a]~%" (:pc st0) (:registers st0))
    (if (zero? ct)
      st0
      (recur (exec st0) (dec ct)))))
