; EXPECT: sat
(set-logic QF_S)
(declare-const x String)
(assert (str.in_re x (re.* (re.++ (re.* (str.to_re "a")) (re.* (str.to_re "b"))))))
(assert (= (str.len x) 5))
(check-sat)
