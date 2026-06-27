; EXPECT: unsat
(set-logic QF_SLIA)
(declare-const i Int)
(assert (str.contains (str.at "abc" i) "z"))
(check-sat)
