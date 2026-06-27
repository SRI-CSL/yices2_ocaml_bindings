; EXPECT: unsat
(set-logic QF_S)
(declare-const y String)
(assert (str.contains (str.replace_all "aaaa" y "bb") "c"))
(check-sat)
