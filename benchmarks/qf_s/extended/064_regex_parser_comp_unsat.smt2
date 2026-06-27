; EXPECT: unsat
(set-logic QF_S)
(declare-const x String)
(assert (= x "a"))
(assert (str.in_re x (re.comp (str.to_re "a"))))
(check-sat)
