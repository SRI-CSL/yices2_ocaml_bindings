; EXPECT: unsat
(set-logic QF_SLRA)
(declare-const x String)
(declare-const r Real)
(assert (= x "a"))
(assert (= (str.len x) 1))
(assert (> r 1.5))
(assert (< r 0.5))
(check-sat)
