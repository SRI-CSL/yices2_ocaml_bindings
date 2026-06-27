; EXPECT: sat
(set-logic QF_SLIA)
(declare-const i Int)
(declare-const n Int)
(assert (= i 1))
(assert (= n 1))
(assert (str.contains (str.substr "abc" i n) "b"))
(check-sat)
