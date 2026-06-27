; EXPECT: sat
(set-logic QF_S)
(declare-const x String)
(assert (str.in_re x (re.* (re.range "a" "c"))))
(assert (= (str.len x) 6))
(check-sat)
