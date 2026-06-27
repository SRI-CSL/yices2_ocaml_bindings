; EXPECT: sat
(set-logic QF_S)
(declare-const x String)
(assert (= x "abc"))
(assert (str.prefixof "a" x))
(assert (str.suffixof "c" x))
(assert (= (str.at x 1) "b"))
(check-sat)
