; EXPECT: unsat
(set-logic QF_S)
(declare-const x String)
(declare-const y String)
(assert (str.in_re x (re.* (str.to_re y))))
(assert (= x "bbb"))
(assert (= y "a"))
(check-sat)
