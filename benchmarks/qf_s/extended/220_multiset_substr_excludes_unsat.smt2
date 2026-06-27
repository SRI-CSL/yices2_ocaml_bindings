; EXPECT: unsat
(set-logic QF_SLIA)
(declare-const i Int)
(declare-const n Int)
(assert (str.contains (str.substr "ab" i n) "aa"))
(check-sat)
