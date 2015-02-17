(ns lc_3.t-core
  (:use midje.sweet)
  (:use [lc_3.core]))

(facts "about decode-word"
       (fact "register and works"
             (decode-word 2r0010100011000010) => {:op :ld :dr 4 :pcoffset9 194}
             (decode-word 2r0010101001000110) => {:op :ld :dr 5 :pcoffset9 70}
             (decode-word 2r0010000111000111) => {:op :ld :dr 0 :pcoffset9 (- 57)}
             (decode-word 2r0010111000000000) => {:op :ld :dr 7 :pcoffset9 0})
       (fact "register and ignores bits 3,4"
             (decode-word 2r0010100011011010) => {:op :ld :dr 4 :pcoffset9 218}
             (decode-word 2r0010101001011110) => {:op :ld :dr 5 :pcoffset9 94}
             (decode-word 2r0010000111011111) => {:op :ld :dr 0 :pcoffset9 (- 33)}
             (decode-word 2r0010111000011000) => {:op :ld :dr 7 :pcoffset9 24}))
