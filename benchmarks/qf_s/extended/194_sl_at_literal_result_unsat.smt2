; EXPECT: unsat
(set-logic QF_SLIA)
(declare-const x String)
(declare-const c String)
(assert (= c "b"))
(assert (= c (str.at x 1)))
(assert (not (= (str.at x 1) "b")))
(check-sat)
