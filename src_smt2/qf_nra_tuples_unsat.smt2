; Example: QF_NRA + tuples (non-standard SMT-LIB2 tuple theory), UNSAT.
;
; Run:
;   dune exec src_smt2/yices_smt2.exe -- src_smt2/qf_nra_tuples_unsat.smt2

(set-logic QF_NRA)

(declare-const p (Tuple Real Real))

; Let x be the first projection of p.
; Contradiction: x^2 = 2 and x^2 = 3.
(assert (= (* ((_ tuple.select 1) p) ((_ tuple.select 1) p)) 2))
(assert (= (* ((_ tuple.select 1) p) ((_ tuple.select 1) p)) 3))

(check-sat)
