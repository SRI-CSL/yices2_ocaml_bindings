; Example: QF_NRA + tuples (non-standard SMT-LIB2 tuple theory).
; This should trigger MCSAT (via QF_NRA) and, in `src_smt2/yices_smt2.ml`,
; tuple-blasting will be enabled automatically for MCSAT contexts.
;
; Run:
;   dune exec src_smt2/yices_smt2.exe -- src_smt2/qf_nra_tuples.smt2

(set-logic QF_NRA)

(declare-const p (Tuple Real Real))

; Tuple construction + projections
(assert (= p (tuple 3/2 2)))
(assert (= (* ((_ tuple.select 1) p) ((_ tuple.select 1) p)) 9/4)) ; x^2 = 2.25
(assert (= ((_ tuple.select 2) p) 2))                              ; y = 2

; Non-linear arithmetic over tuple projections
(assert (> (+ (* ((_ tuple.select 1) p) ((_ tuple.select 1) p))
              (* ((_ tuple.select 2) p) ((_ tuple.select 2) p)))
           4))

(check-sat)
(get-model)
