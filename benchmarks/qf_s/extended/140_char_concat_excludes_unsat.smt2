; EXPECT: unsat
(set-logic QF_SLIA)
(declare-const i Int)
(declare-const n Int)
(declare-const j Int)
(assert (str.contains (str.++ (str.substr "ab" i n) (str.at "cd" j)) "z"))
(check-sat)
