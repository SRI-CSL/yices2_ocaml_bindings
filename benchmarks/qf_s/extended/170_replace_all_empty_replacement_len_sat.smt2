; EXPECT: sat
(set-logic QF_SLIA)
(declare-const x String)
(declare-const y String)
(assert (= x "banana"))
(assert (= y (str.replace_all x "na" "")))
(assert (= y "ba"))
(assert (= (str.len y) 2))
(check-sat)
