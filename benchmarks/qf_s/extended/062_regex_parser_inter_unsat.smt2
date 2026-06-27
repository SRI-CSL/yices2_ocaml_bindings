; EXPECT: unsat
(set-logic QF_S)
(declare-const x String)
(assert (str.in_re x (re.inter (str.to_re "a") (str.to_re "b"))))
(check-sat)
