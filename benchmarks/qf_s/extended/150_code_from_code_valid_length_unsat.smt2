; EXPECT: unsat
(set-logic QF_SLIA)
(declare-const c Int)
(assert (= c 65))
(assert (= (str.len (str.from_code c)) 0))
(check-sat)
