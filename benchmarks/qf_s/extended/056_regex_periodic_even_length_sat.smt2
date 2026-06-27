; EXPECT: sat
(set-logic QF_S)
(declare-const x String)
(assert (str.in_re x (re.* (str.to_re "aa"))))
(assert (= (str.len x) 4))
(check-sat)
