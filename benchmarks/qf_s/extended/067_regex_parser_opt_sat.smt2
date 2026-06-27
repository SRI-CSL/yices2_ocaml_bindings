; EXPECT: sat
(set-logic QF_S)
(declare-const x String)
(assert (= x ""))
(assert (str.in_re x (re.opt (str.to_re "a"))))
(check-sat)
