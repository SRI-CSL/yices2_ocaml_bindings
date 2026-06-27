; EXPECT: unsat
(set-logic QF_S)
(declare-const x String)
(assert (= x "aaa"))
(assert (= (str.replace_all x "a" "b") "aba"))
(check-sat)
