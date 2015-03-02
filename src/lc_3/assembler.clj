(ns lc_3.assembler
  (:require [clojure.string :as str]))

;;Note: All functions with a string value as one of their parameters expect
;;that the string value has been trimmed.

;;customized control flow for head-tail structure processing
(defn doht [ht peek proceed]
  (let [peekresult (peek (:head ht))]
    (if (:bool peekresult)
      (proceed (:tail ht) (:value peekresult))
      peekresult)))

;;head-tail destruction for a token
(defn htail [s] {:head (.charAt s 0) :tail (.substring s 1)})

;;head-tail destruction for a line
(defn HTail [line]
  (let [v (str/split line #"\s+" 2)]
    {:head (v 0)
     :tail (if (= (count v) 2) (v 1))}))

(defn rangeChecker [low high]
  (fn [x] (and (>= x low) (<= x high))))

;;range of register sequence number
(def regSpace? (rangeChecker 0 7))

;;range of a word
(def wordSpace?
  (let [limit (bit-shift-left 1 15)]
    (rangeChecker (- limit) (dec limit))))

;;range of memory address
(def memSpace? (rangeChecker 0 (dec (bit-shift-left 1 16))))

;;range of an imm5 value
(def imm5Space?
  (let [limit (bit-shift-left 1 4)]
    (rangeChecker (- limit) (dec limit))))

;;range of an offset6 value
(def offset6Space?
  (let [limit (bit-shift-left 1 5)]
    (rangeChecker (- limit) (dec limit))))

;;range of a trapvect8 value
(def trapSpace? (rangeChecker 0 (dec (bit-shift-left 1 8))))

;;all token recognizers return {:bool true/false :value ...}
;;Note: the value of :value should not be used if the value of :bool is false.

(defn registerToken [s]
  (doht (htail s)
        (fn [h] {:bool (= h \R) :value h})
        (fn [t r]
          (let [n (try (Integer/valueOf t)
                       (catch NumberFormatException e -1))]
            {:bool (regSpace? n) :value n}))))

(defn directiveToken [s]
  (doht (htail s)
        (fn [h] {:bool (= h \.) :value h})
        (fn [t r]
          {:bool  (not (nil? (re-find #"^(ORIG|FILL|BLKW|STRINGZ|END)$" t)))
           :value t})))

(defn opcodeToken [s]
  {:bool (not (nil? (re-find #"^(AND|ADD|NOT|LD|LDR|LDI|ST|STR|STI|LEA|BRN?Z?P?|JMP|JSRR?|RET|RTI|TRAP|GETC|OUT|PUTS|IN|PUTSP|HALT)$" s)))
   :value s})

(defn immediateTokenHelper [firstcall? s radix range]
  (try (let [x (Integer/parseInt s radix)]
         {:bool (range x) :value x})
       (catch NumberFormatException e
         (if firstcall?
           (doht (htail s)
                 (fn [h] {:bool (or (= h \#) (= h \X) (= h \B)) :value h})
                 (fn [t r]
                   (let [radix (case r
                                 \# 10 \X 16 \B 2)]
                     (immediateTokenHelper false t radix range))))
           {:bool false :value nil}))))

(defn immediateToken [s] (immediateTokenHelper true s 10 wordSpace?))
(defn addressToken   [s] (immediateTokenHelper true s 10 memSpace?))
(defn imm5Token      [s] (immediateTokenHelper true s 10 imm5Space?))
(defn offset6Token   [s] (immediateTokenHelper true s 10 offset6Space?))
(defn trapvect8Token [s] (immediateTokenHelper true s 10 trapSpace?))

;;s: a first element (separated by whitespaces) of a line;
;;   not an operator token; not a directive token.
;;Returns true iff s is neither a register token, nor an immediate token,
;;nor an invalid token, nor nil.
(defn labelToken [s]
  {:bool (cond
           (:bool (opcodeToken s)) false
           (:bool (directiveToken s)) false
           (:bool (registerToken s)) (println "Error: a register token is found at the place of a label, an operator, or a directive.")
           (:bool (immediateToken s)) (println "Error: an immediate token is found at the place of a label, an operator, or a directive.")
           (nil? (re-find #"^[A-Z]([A-Z]|\d)*$" s)) (println "Error: an invalid token is found at the place of a label, an operator, or a directive.")
           :else s)
   :value s})

(defn uniqueLabel? [map label]
  (if (contains? map label) (println "Error: " label "is defined twice.")
      label))

;;an Environment consists of:
;;- an origin address
;;- a current pc
;;- a mapping from label to (absolute) address
;;- a vector that contains lines of program read
(defrecord Environment [orig pc labelMap instVec encodedVec])

(defn update-map [map key func]
  (assoc map key (func (get map key))))

(defn inc-pc [env] (update-map env :pc inc))

(defn add-inst [env line]
  (update-map env :instVec (fn [x] (conj x line))))

(defn add-label [env label addr]
  (update-map env :labelMap (fn [x] (assoc x label addr))))

(defn splitAtSemicolon [line] (str/split line #"\s*;\s*" 2))
(defn splitAtComma [line] (str/split line #"\s*,\s*" 4))

(defn instruction [line]
  (let [h (:head (HTail line))]
    (if (or (:bool (opcodeToken h)) (:bool (directiveToken h)))
      line
      (println "Error:" line "doesn't begin with opcode or directive."))))

(defn line-to-inst [line]
  (let [ht (HTail line)
        h  (:head ht)
        t  (:tail ht)]
    (if (:bool (labelToken h))
      [(instruction t) h]
      [(instruction line) nil])))

;;return nil if error occurs, or updated env if succeed.
(defn append-inst [env line]
  (let [[inst label] (line-to-inst line)]
    (cond (and label inst) (inc-pc (add-inst (add-label env label (:pc env)) inst))
          inst             (inc-pc (add-inst env inst)))))

;;return nil iff the 1st element in the line (separated by whitespaces) isn't
;;".ORIG", or the 2nd element is not a valid address token.
(defn set-orig [line]
  (let [ht (HTail line)]
    (if (.equals (:head ht) ".ORIG")
      (let [a (addressToken (:tail ht))]
        (if (:bool a)
          (Environment. (:value a) (:value a) {} [] [])
          (println "Error: .ORIG isn't followed by a valid memory address.")))
      nil)))

(defn line-reducer [env line]
  (let [line (-> line
                 (str/trim)
                 (splitAtSemicolon)
                 (nth 0)
                 (str/upper-case))]
    (if (or (.equals line "") (.equals (:head (HTail line)) ".END"))
      env
      (if (nil? env)
        (set-orig line)
        (append-inst env line)))))

(use 'clojure.java.io)
(defn first-pass [path-to-file]
  (with-open [r (reader path-to-file)]
    (reduce line-reducer nil (line-seq r))))

;;After building the environment upon the input program, first pass is done.
;;In second pass, assembler should process elements of (:instVec environment)
;;one by one: validate and encode.

;;in 1st pass, each line of input that is recorded in (:instVec environment)
;;goes through the following process:
;;
;; 1. trim preceding and succeeding whitespaces;
;; 2. drop succeeding comment part, and whitespaces in between as well;
;; 3. ensure that there is content left after previous steps;
;; 4. include iff the line is enterred after the latest valid .ORIG line and
;;    before the valid .END line.
;; 5. drop the label part if a valid one presents;
;; 6. ensure that the 1st element (separated by whitespaces) is either an opcode,
;;    or a directive.

(defn add-encoded [env encoded]
  (assoc env :encodedVec (conj (:encodedVec env) encoded)))

;;validity of a label in 2nd-pass is only checked against (:labelMap env)
(defn definedLabel [env token]
  (let [v (get (:labelMap env) token)]
    (if (nil? v)
      {:bool false}
      {:bool true :value (- v (:pc env) 1)}))) ;label -> pcoffset9(or pcoffset11)

;;used by encodeHelper
(def instructionSpec
  {"AND"   {:n 3 :ops [{:func registerToken :label "a register"}
                       {:func registerToken :label "a register"}
                       {:func registerToken :label "a register"}]}
   ;ANDI is an intermediate opcode
   "ANDI"  {:n 3 :ops [{:func registerToken :label "a register"}
                       {:func registerToken :label "a register"}
                       {:func imm5Token     :label "an imm5"}]}
   "ADD"   {:n 3 :ops [{:func registerToken :label "a register"}
                       {:func registerToken :label "a register"}
                       {:func registerToken :label "a register"}]}
   ;ADDI is an intermediate opcode
   "ADDI"  {:n 3 :ops [{:func registerToken :label "a register"}
                       {:func registerToken :label "a register"}
                       {:func imm5Token     :label "an imm5"}]}
   "NOT"   {:n 2 :ops [{:func registerToken :label "a register"}
                       {:func registerToken :label "a register"}]}
   "LD"    {:n 2 :ops [{:func registerToken :label "a register"}
                       {:func definedLabel  :label "a defined label"}]}
   "LDR"   {:n 3 :ops [{:func registerToken :label "a register"}
                       {:func registerToken :label "a register"}
                       {:func offset6Token  :label "an offset6"}]}
   "LDI"   {:n 2 :ops [{:func registerToken :label "a register"}
                       {:func definedLabel  :label "a defined label"}]}
   "ST"    {:n 2 :ops [{:func registerToken :label "a register"}
                       {:func definedLabel  :label "a defined label"}]}
   "STR"   {:n 3 :ops [{:func registerToken :label "a register"}
                       {:func registerToken :label "a register"}
                       {:func offset6Token  :label "a register"}]}
   "STI"   {:n 2 :ops [{:func registerToken :label "a register"}
                       {:func definedLabel  :label "a defined label"}]}
   "LEA"   {:n 2 :ops [{:func registerToken :label "a register"}
                       {:func definedLabel  :label "a defined label"}]}
   "BR"    {:n 1 :ops [{:func definedLabel  :label "a defined label"}]}
   "BRN"   {:n 1 :ops [{:func definedLabel  :label "a defined label"}]}
   "BRNZ"  {:n 1 :ops [{:func definedLabel  :label "a defined label"}]}
   "BRNP"  {:n 1 :ops [{:func definedLabel  :label "a defined label"}]}
   "BRNZP" {:n 1 :ops [{:func definedLabel  :label "a defined label"}]}
   "BRZ"   {:n 1 :ops [{:func definedLabel  :label "a defined label"}]}
   "BRZP"  {:n 1 :ops [{:func definedLabel  :label "a defined label"}]}
   "BRP"   {:n 1 :ops [{:func definedLabel  :label "a defined label"}]}
   "JMP"   {:n 1 :ops [{:func definedLabel  :label "a defined label"}]}
   "JSR"   {:n 1 :ops [{:func definedLabel  :label "a defined label"}]}
   "JSRR"  {:n 1 :ops [{:func registerToken :label "a register"}]}
   "RET"   {:n 0}
   "RTI"   {:n 0}
   "TRAP"  {:n 1 :ops [{:func trapvect8Token :label "a trapvect8"}]}
   "GETC"  {:n 0}
   "OUT"   {:n 0}
   "PUTS"  {:n 0}
   "IN"    {:n 0}
   "PUTSP" {:n 0}
   "HALT"  {:n 0}})

;;return a corresponding vector of values for the given vector of tokens
;;of operands, according to the rule for operator op, under the current
;;environment.
(defn encodeHelper [env op n tokens]
  (let [spec  (get instructionSpec op)
        nops  (:n spec)
        ops   (:ops spec)]
    (if (= n nops)
      (loop [[x & xs] ops
             [y & ys] tokens
             z []]
        (if (nil? y)
          z ;end successfully
          (let [f (:func x)
                v (if (= f definedLabel)
                    (f env y) ;definedLabel needs env as one of its parameters
                    (f y))] ;while other token recognizers don't.
            (if (:bool v)
              (recur xs ys (conj z (:value v)))
              (throw (Throwable.
                      (println-str "Error:" y "is not" (:label x))))))))
      (throw (Throwable.
              (println-str "Error:" op "expects" nops "operands instead of" n))))))

(defn encode [env opcode ops]
  (let [nops (count ops)]
    (try (let [r (encodeHelper env opcode nops ops)]
           (case opcode
             "AND"   (let [[a b c] r] {:op :and   :dr a :sr1 b :sr2 c})
             "ANDI"  (let [[a b c] r] {:op :and-i :dr a :sr1 b :imm5 c})
             "ADD"   (let [[a b c] r] {:op :add   :dr a :sr1 b :sr2 c})
             "ADDI"  (let [[a b c] r] {:op :add-i :dr a :sr1 b :imm5 c})
             "NOT"   (let [[a b]   r] {:op :not   :dr a :sr b})
             "LD"    (let [[a b]   r] {:op :ld    :dr a :pcoffset9 b})
             "LDR"   (let [[a b c] r] {:op :ldr   :dr a :baser b :offset6 c})
             "LDI"   (let [[a b]   r] {:op :ldi   :dr a :pcoffset9 b})
             "ST"    (let [[a b]   r] {:op :st    :dr a :pcoffset9 b})
             "STR"   (let [[a b c] r] {:op :str   :dr a :baser b :offset6 c})
             "STI"   (let [[a b]   r] {:op :sti   :dr a :pcoffset9 b})
             "LEA"   (let [[a b]   r] {:op :lea   :dr a :pcoffset9 b})
             "BR"    (let [[a]     r] {:op :br    :pcoffset9 a})
             "BRN"   (let [[a]     r] {:op :brn   :pcoffset9 a})
             "BRNZ"  (let [[a]     r] {:op :brnz  :pcoffset9 a})
             "BRNP"  (let [[a]     r] {:op :brnp  :pcoffset9 a})
             "BRNZP" (let [[a]     r] {:op :brnzp :pcoffset9 a})
             "BRZ"   (let [[a]     r] {:op :brz   :pcoffset9 a})
             "BRZP"  (let [[a]     r] {:op :brzp  :pcoffset9 a})
             "BRP"   (let [[a]     r] {:op :brp   :pcoffset9 a})
             "JMP"   (let [[a]     r] {:op :jmp   :baser a})
             "JSR"   (let [[a]     r] {:op :jsr   :pcoffset11 a})
             "JSRR"  (let [[a]     r] {:op :jsrr  :baser a})
             "TRAP"  (let [[a]     r] {:op :trap  :trapvect8 a})
             "GETC"  {:op :trap :trapvect8 32}
             "OUT"   {:op :trap :trapvect8 33}
             "PUTS"  {:op :trap :trapvect8 34}
             "IN"    {:op :trap :trapvect8 35}
             "PUTSP" {:op :trap :trapvect8 36}
             "HALT"  {:op :trap :trapvect8 37}
             "RET"   {:op :ret}
             "RTI"   {:op :rti}
             nil))
         (catch Throwable t
           (case opcode                       ;second chance
             "AND" (encode env "ANDI" ops)
             "ADD" (encode env "ADDI" ops)
             (println (.getMessage t)))))))

(defn encode-and-update [env inst]
  (if env
    (let [ht     (HTail inst)
          opcode (:head ht)
          ops    (if (:tail ht) (splitAtComma (:tail ht)))
          encoded (encode env opcode ops)]
      (if encoded
        (do (println encoded)
          (inc-pc (add-encoded env encoded)))))))

(defn assemble [path-to-file]
  (let [env (first-pass path-to-file)]
    (if env
      (let [env (-> (assoc env :pc (:orig env))
                    (assoc :encodedVec []))]
        (reduce encode-and-update env (:instVec env)))
      (println "First-pass assemble failed."))))

