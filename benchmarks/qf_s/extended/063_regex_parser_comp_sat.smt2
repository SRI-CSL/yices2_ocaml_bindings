; EXPECT: sat
(set-logic QF_S)
(declare-const x String)
(assert (= x "b"))
(assert (str.in_re x (re.comp (str.to_re "a"))))
(check-sat)
