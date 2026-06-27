; EXPECT: unsat
(set-logic QF_S)
(declare-const x String)
(assert (= x "abc"))
(assert (str.in_re x (str.to_re "abd")))
(check-sat)
