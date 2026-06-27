; EXPECT: unsat
(set-logic QF_S)
(declare-const x String)
(assert (= x "a"))
(assert (not (str.in_re x (str.to_re "a"))))
(check-sat)
