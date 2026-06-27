; EXPECT: sat
; FMF_MAX_TOTAL_LENGTH: 2
; FMF_MAX_ROUNDS: 3
; FMF_EXPECT_TRACE: disabled: no root string variables to bound
(set-logic QF_LIA)
(declare-const x Int)
(assert (= x 4))
(check-sat)
