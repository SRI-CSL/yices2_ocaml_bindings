; EXPECT: sat
(set-logic QF_S)
(declare-const x String)
(assert (str.in_re x (re.* (re.range "a" "c"))))
(assert (not (str.contains x "z")))
(assert (= (str.len x) 2))
(check-sat)
