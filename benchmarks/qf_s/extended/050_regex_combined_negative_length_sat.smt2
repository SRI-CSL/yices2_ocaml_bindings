; EXPECT: sat
(set-logic QF_S)
(declare-const x String)
(assert (str.in_re x (re.union (str.to_re "a") (str.to_re "bb") (str.to_re "ccc"))))
(assert (not (str.in_re x (str.to_re "bb"))))
(assert (= (str.len x) 3))
(check-sat)
