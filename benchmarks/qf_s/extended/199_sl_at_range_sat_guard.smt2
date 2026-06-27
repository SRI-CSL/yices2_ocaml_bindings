; EXPECT: sat
(set-logic QF_S)
(declare-const x String)
(declare-const c String)
(assert (= x "abc"))
(assert (= c (str.at x 1)))
(assert (str.in_re c (re.range "a" "z")))
(assert (not (= (str.at x 0) "z")))
(check-sat)
