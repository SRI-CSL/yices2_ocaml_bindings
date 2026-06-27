; EXPECT: unsat
(set-logic QF_S)
(declare-const x String)
(declare-const y String)
(assert (str.in_re x (re.opt (str.to_re y))))
(assert (= x "bb"))
(assert (= y "aa"))
(check-sat)
