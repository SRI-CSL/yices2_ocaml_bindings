; EXPECT: sat
(set-logic QF_S)
(declare-const x String)
(assert (= x "abbb"))
(assert (str.in_re x (re.++ (str.to_re "a") (re.* (str.to_re "b")))))
(check-sat)
