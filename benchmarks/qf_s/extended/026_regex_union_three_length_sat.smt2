; EXPECT: sat
(set-logic QF_S)
(declare-const x String)
(assert (str.in_re x (re.union (str.to_re "a") (re.union (str.to_re "bb") (str.to_re "ccc")))))
(assert (= (str.len x) 3))
(check-sat)
