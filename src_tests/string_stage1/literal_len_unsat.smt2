(set-logic QF_S)
(assert (= (str.len "abc") 4))
(check-sat)
