; EXPECT: unsat
(set-logic QF_SLIA)
(declare-const x String)
(declare-const i Int)
(declare-const n Int)
(assert (= x (str.substr "abc" i n)))
(assert (str.contains x "z"))
(check-sat)
