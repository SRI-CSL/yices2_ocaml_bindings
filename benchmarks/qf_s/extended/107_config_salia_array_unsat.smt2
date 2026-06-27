; EXPECT: unsat
(set-logic QF_SALIA)
(declare-const a (Array Int String))
(assert (= (select a 0) "hi"))
(assert (= (str.len (select a 0)) 3))
(check-sat)
