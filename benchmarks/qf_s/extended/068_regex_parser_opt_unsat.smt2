; EXPECT: unsat
(set-logic QF_S)
(declare-const x String)
(assert (= x "aa"))
(assert (str.in_re x (re.opt (str.to_re "a"))))
(check-sat)
