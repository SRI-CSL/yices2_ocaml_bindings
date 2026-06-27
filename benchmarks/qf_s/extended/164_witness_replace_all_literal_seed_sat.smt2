; EXPECT: sat
(set-logic QF_S)
(declare-const x String)
(assert (= x "abcb"))
(assert (= (str.replace_all x "b" "B") "aBcB"))
(check-sat)
