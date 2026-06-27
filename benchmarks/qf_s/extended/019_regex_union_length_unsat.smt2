; EXPECT: unsat
(set-logic QF_S)
(declare-const x String)
(assert (str.in_re x (re.union (str.to_re "a") (str.to_re "bbb"))))
(assert (= (str.len x) 2))
(check-sat)
