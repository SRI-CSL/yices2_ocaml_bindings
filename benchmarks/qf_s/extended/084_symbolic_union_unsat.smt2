; EXPECT: unsat
(set-logic QF_S)
(declare-const x String)
(declare-const y String)
(assert (str.in_re x (re.union (str.to_re y) (str.to_re "zz"))))
(assert (= x "bb"))
(assert (= y "aa"))
(check-sat)
