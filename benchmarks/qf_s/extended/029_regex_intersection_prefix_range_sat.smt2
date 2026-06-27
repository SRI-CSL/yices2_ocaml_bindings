; EXPECT: sat
(set-logic QF_S)
(declare-const x String)
(assert (str.in_re x (re.* (re.range "a" "z"))))
(assert (str.in_re x (re.++ (str.to_re "ab") re.all)))
(assert (= (str.len x) 4))
(check-sat)
