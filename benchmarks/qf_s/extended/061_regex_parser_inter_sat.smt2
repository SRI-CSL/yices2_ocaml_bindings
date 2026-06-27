; EXPECT: sat
(set-logic QF_S)
(declare-const x String)
(assert (= x "b"))
(assert (str.in_re x (re.inter (re.range "a" "c") (re.range "b" "d"))))
(check-sat)
