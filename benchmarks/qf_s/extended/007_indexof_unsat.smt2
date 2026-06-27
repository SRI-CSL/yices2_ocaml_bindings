; EXPECT: unsat
(set-logic QF_S)
(declare-const x String)
(assert (= x "abcabc"))
(assert (= (str.indexof x "bc" 0) 2))
(check-sat)
