; G27 - Andre Goncalves
; Projeto individual para a cadeira de Procura e Planeamento

(load "procura")

; Estrutura estado
(defstruct state
	board
	nr-plays-left
	queens-placed)

; Verifica se e o estado objetivo
(defun isGoalState? (state)
	(= (state-nr-plays-left state) 0))

; Facilita a criacao de um novo estado a partir de um estado exisente
(defun cria-novo-estado (state line pos)
	(let ((new-st	(make-state 	:board 			(copy-array (state-board state))
									:nr-plays-left	(1- (state-nr-plays-left state))
									:queens-placed  (append (state-queens-placed state) (list pos)))))
		(setf (aref (state-board new-st) line pos) T)
	  	new-st))

;; Gerar lista de posicoes interditas
;; pos 		-- posicao da rainha que ja foi colocada
;; pos_line	-- linha da rainha que ja foi colocada
;; line 	-- linha onde a nova rainha vai ser colocada
(defun gen-bad-pos (pos pos_line line)
	(let* ((offset  (- line (+ pos_line 1))))
	(cond ((> (+ pos offset 1) 19) (list (- pos offset 1) pos))
		  ((< (- pos offset 1) 0)  (list pos (+ pos offset 1)))
		  (T 			   	   	   (list (- pos offset 1) pos (+ pos offset 1))))))
 
;; Gerar as posicoes validas no caso de nao ser a primeira linha
;; TODO: optimizar com base nos resultados da linha anterior
(defun gen-pos-validas (state line)
	(let ((good-pos (list 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19)))
		(if (null (state-queens-placed state))
	  		good-pos
	  		(rem-bad-positions good-pos (state-queens-placed state) line))))

(defun rem-bad-positions (good-pos queens-placed line)
	(let ((pos_line 0))
	  	(dolist (pos queens-placed good-pos) 							
	  		(dolist (bad-pos (gen-bad-pos pos pos_line line) good-pos) ;; 
	  			(setf good-pos (remove bad-pos good-pos)))
	  		(incf pos_line))))

;; Retorna a lista de todos os estados possiveis  
(defun gen-successors (state)
  (let ((next-states NIL)
		(line (length (state-queens-placed state)))) ;; linha em que se vai inserir uma nova rainha
		(dolist (pos (gen-pos-validas state line))
			(setf next-states (append next-states (list (cria-novo-estado state line pos)))))
	next-states))
				 
; Funcao de resolucao do problema
(defun solve-problem (empty-board sch-strategy)
	(let* ((init-state (make-state :board empty-board 
				       			  :nr-plays-left 20 
				       			  :queens-placed NIL))
			(prob (cria-problema  init-state
						  		  (list #'gen-successors)
						  		  :objectivo? #'isGoalState?)))
			(state-board (nth 20 (first (procura prob sch-strategy :espaco-em-arvore? T))))))
