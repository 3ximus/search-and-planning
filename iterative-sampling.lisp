
;; Randomizer function
(defun select-random-state (successor-states-lst)
	(let  ((nr-states (length successor-states-lst)))
		(nth (random nr-states) successor-states-lst)))

(defun iterative-sampling (problema)
	0)
  ; (let ((espaco (novo-espaco-a* problema)))
  ;   (junta-nos-gerados espaco
  ;              (list (cria-no-a* (problema-estado-inicial problema)
  ;                    nil  ; O pai do estado inicial nao existe
  ;                    0    ; e o custo e' 0 (zero)
  ;                    (problema-heuristica problema))))
  ;   (procura-com-espaco problema espaco)))