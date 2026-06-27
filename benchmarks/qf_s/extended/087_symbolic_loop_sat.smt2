; EXPECT: sat
(set-logic QF_S)
(declare-const x String)
(declare-const y String)
(assert (str.in_re x ((_ re.loop 2 3) (str.to_re y))))
(assert (= x "aa"))
(assert (= y "a"))
(check-sat)
