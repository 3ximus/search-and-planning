(load "G027")

;;; FUNCOES DE TESTE!!!!!
(setf init-state (make-state :board (make-array'(20 20)) 
			     :nr-plays-left 20 
			     :queens-placed NIL))

(defun basic-checks-of-20-queens-result (res)
  (cond ((not (arrayp res)) (error "The result must be an array!"))
		((not (equalp '(20 20) (array-dimensions res))) (error "The array dimensions are wrong!"))
		(T "Passed basic tests.")))


;(format T "~A" (gen-pos-validas init-state))

;(format T "~A" (cria-novo-estado init-state 0 17))
#| (setf succ (gen-successors init-state))
(setf succ (gen-successors (first succ))) |#

#| (format T "Posicoes validas [linha 1, pos_Rainha 0] - ~A~%" (gen-pos-validas (first succ)))
(format T "Posicoes validas [linha 1, pos_Rainha 1] - ~A~%" (gen-pos-validas (second succ)))
(format T "Posicoes validas [linha 1, pos_Rainha 2] - ~A~%" (gen-pos-validas (third succ)))
(format T "Posicoes validas [linha 1, pos_Rainha 3] - ~A~%" (gen-pos-validas (fourth succ)))
(format T "Posicoes validas [linha 1, pos_Rainha 4] - ~A~%" (gen-pos-validas (fifth succ))) |#

;(format T "~A~%" (first succ))

;(format T "Posicoes validas [linha 2, pos_Rainha 2] - ~A~%" (gen-pos-validas (first succ)))
#| (format T "Posicoes validas [linha 2, pos_Rainha 1] - ~A~%" (gen-pos-validas (second succ)))
(format T "Posicoes validas [linha 2, pos_Rainha 2] - ~A~%" (gen-pos-validas (third succ)))
(format T "Posicoes validas [linha 2, pos_Rainha 3] - ~A~%" (gen-pos-validas (fourth succ)))
(format T "Posicoes validas [linha 2, pos_Rainha 4] - ~A~%" (gen-pos-validas (fifth succ))) |#


#| (setf succ (gen-successors (first succ)))
(format T "~A" (first succ))
(setf succ (gen-successors (first succ)))
(format T "~A" (first succ))
(setf succ (gen-successors (first succ)))
(format T "~A" (first succ))
(setf succ (gen-successors (first succ)))
(format T "~A" (first succ))
(setf succ (gen-successors (first succ)))
(format T "~A" (first succ))
(setf succ (gen-successors (first succ)))
(format T "~A" (first succ)) |#



(basic-checks-of-20-queens-result (solve-problem (make-array'(20 20)) "profundidade"))
;(format T "~A" (solve-problem (make-array'(20 20)) "profundidade"))
;(format T "~A" (solve-problem (make-array '(20 20)) "profundidade"))