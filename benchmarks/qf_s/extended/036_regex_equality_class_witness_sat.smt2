; EXPECT: sat
(set-logic QF_S)
(declare-const x String)
(declare-const y String)
(assert (= x y))
(assert (str.in_re x (re.* (str.to_re "aa"))))
(assert (= (str.len y) 4))
(check-sat)
