; EXPECT: unsat
(set-logic QF_SBV)
(declare-const x String)
(declare-const b (_ BitVec 4))
(assert (= x "abc"))
(assert (= (str.len x) 3))
(assert (= (bvadd b #b0001) #b0100))
(assert (= b #b0100))
(check-sat)
