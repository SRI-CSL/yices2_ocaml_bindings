; EXPECT: unsat
(set-logic QF_SLIA)
(declare-const x String)
(assert (not (= (str.indexof x "a" (- 1)) (- 1))))
(check-sat)
