; EXPECT: sat
(set-logic QF_S)
(declare-const x String)
(declare-const y String)
(assert (= x (str.++ y "bc")))
(assert (str.in_re x (re.* (re.range "a" "c"))))
(assert (= (str.len x) 4))
(check-sat)
