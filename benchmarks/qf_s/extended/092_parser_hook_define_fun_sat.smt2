; EXPECT: sat
(set-logic QF_SLIA)
(define-fun bang ((s String)) String (str.++ s "!"))
(assert (= (bang "a") "a!"))
(check-sat)
