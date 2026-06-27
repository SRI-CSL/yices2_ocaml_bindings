; EXPECT: unsat
(set-logic QF_S)
(declare-const x String)
(assert (str.in_re x (re.union (str.to_re "a") (str.to_re "bb"))))
(assert (not (str.in_re x (re.union (str.to_re "a") (str.to_re "bb")))))
(assert (= (str.len x) 1))
(check-sat)
