(set-logic QF_S)
(assert (= (str.len "abc") 3))
(check-sat)
