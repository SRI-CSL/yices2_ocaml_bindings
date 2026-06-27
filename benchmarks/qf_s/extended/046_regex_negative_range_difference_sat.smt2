; EXPECT: sat
(set-logic QF_S)
(declare-const x String)
(assert (str.in_re x (re.range "a" "d")))
(assert (not (str.in_re x (re.range "b" "c"))))
(check-sat)
