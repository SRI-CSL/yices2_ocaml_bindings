; EXPECT: sat
(set-logic QF_S)
(declare-const x String)
(assert (str.contains x "a"))
(assert (= (str.replace_all x "a" "b") "bb"))
(check-sat)
