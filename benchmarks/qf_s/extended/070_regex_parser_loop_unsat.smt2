; EXPECT: unsat
(set-logic QF_S)
(declare-const x String)
(assert (= x "aaaaa"))
(assert (str.in_re x ((_ re.loop 2 4) (str.to_re "a"))))
(check-sat)
