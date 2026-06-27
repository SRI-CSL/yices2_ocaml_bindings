; EXPECT: unsat
(set-logic QF_SBV)
(declare-const x String)
(declare-const b (_ BitVec 4))
(assert (= x "\u{61}"))
(assert (= (str.len x) 1))
(assert (= b #b0011))
(assert (= b #b0100))
(check-sat)
