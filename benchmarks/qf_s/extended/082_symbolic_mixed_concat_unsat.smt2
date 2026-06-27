; EXPECT: unsat
(set-logic QF_S)
(declare-const x String)
(declare-const y String)
(assert (str.in_re x (re.++ (str.to_re "a") (str.to_re y) (str.to_re "c"))))
(assert (= x "adc"))
(assert (= y "bb"))
(check-sat)
