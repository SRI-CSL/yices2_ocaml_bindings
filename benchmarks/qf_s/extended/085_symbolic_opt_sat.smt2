; EXPECT: sat
(set-logic QF_S)
(declare-const x String)
(declare-const y String)
(assert (str.in_re x (re.opt (str.to_re y))))
(assert (= x ""))
(assert (= y "aa"))
(check-sat)
