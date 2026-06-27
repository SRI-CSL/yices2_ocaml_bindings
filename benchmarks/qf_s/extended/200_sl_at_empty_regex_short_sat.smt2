; EXPECT: sat
(set-logic QF_SLIA)
(declare-const x String)
(assert (= (str.len x) 0))
(assert (str.in_re (str.at x 3) (re.opt (str.to_re "a"))))
(check-sat)
