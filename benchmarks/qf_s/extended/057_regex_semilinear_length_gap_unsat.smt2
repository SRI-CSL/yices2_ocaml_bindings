; EXPECT: unsat
(set-logic QF_S)
(declare-const x String)
(assert (str.in_re x (re.* (re.union (str.to_re "aa") (str.to_re "bbb")))))
(assert (= (str.len x) 1))
(check-sat)
