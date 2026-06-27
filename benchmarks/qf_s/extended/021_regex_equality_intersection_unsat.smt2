; EXPECT: unsat
(set-logic QF_S)
(declare-const x String)
(declare-const y String)
(assert (= x y))
(assert (str.in_re x (str.to_re "a")))
(assert (str.in_re y (str.to_re "b")))
(check-sat)
