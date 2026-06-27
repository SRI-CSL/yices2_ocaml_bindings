; EXPECT: unsat
(set-logic QF_S)
(declare-const x String)
(assert (str.in_re x (re.* (re.union (str.to_re "aaaa") (str.to_re "bbbbbb")))))
(assert (= (str.len x) 5))
(check-sat)
