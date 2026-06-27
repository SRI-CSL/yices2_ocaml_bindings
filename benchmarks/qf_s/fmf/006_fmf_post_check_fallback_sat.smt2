; EXPECT: sat
; FMF_MAX_TOTAL_LENGTH: 2
; FMF_MAX_ROUNDS: 3
; FMF_EXPECT_TRACE: disabled: commands after check-sat require ordinary processing
(set-logic QF_S)
(declare-const x String)
(assert (= x "a"))
(check-sat)
(get-model)
