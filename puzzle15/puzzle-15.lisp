;;; Procura e planeamento
;;; 15 Puzzle Problem
;;; Fabio Almeida - 76959

(load "procura")

(setf COMPLETE-BOARD #2A((1 2 3 4)
						 (5 6 7 8)
						 (9 10 11 12)
						 (13 14 15 NIL)))

(defstruct state
	board
	clear-tile	; position of the cleared tile
	previous-play
	number-plays)

; make the inverse play easy to access
(defparameter *invert-table* (make-hash-table :test #'equalp))
(setf (gethash "D" *invert-table*) "U")
(setf (gethash "U" *invert-table*) "D")
(setf (gethash "L" *invert-table*) "R")
(setf (gethash "R" *invert-table*) "L")

;; -----------------------------
;; BASE FUNCTION
;; -----------------------------

(defun swap (board p1 p2)
	"Swap two array elements, returns p2"
	(rotatef (aref board (car p1)(cadr p1)) (aref board (car p2)(cadr p2))) p2)

(defun move-tile (state move)
	"Move tile in a given direction"
	(let ((board (state-board state)) (ct (state-clear-tile state)))
		(cond ((equalp move "L") (setf (state-clear-tile state) (swap board ct (list (car ct) (1- (cadr ct))))))
			  ((equalp move "R") (setf (state-clear-tile state) (swap board ct (list (car ct) (1+ (cadr ct))))))
			  ((equalp move "U") (setf (state-clear-tile state) (swap board ct (list (1- (car ct)) (cadr ct)))))
			  ((equalp move "D") (setf (state-clear-tile state) (swap board ct (list (1+ (car ct)) (cadr ct))))))
	(setf (state-previous-play state) move)
	(incf (state-number-plays state))
	state))

(defun possible-moves (state)
	"Returns all possible moves from a given state"
; for lack of better idea this does the job
	(let ((base (list "L" "R" "U" "D"))(ct (state-clear-tile state)))
		(setf base (remove (gethash (state-previous-play state) *invert-table*) base :test #'equalp))
		(if (equalp (car ct) 0) (setf base (remove "U" base :test #'equalp)))
		(if (equalp (car ct) 3) (setf base (remove "D" base :test #'equalp)))
		(if (equalp (cadr ct) 0) (setf base (remove "L" base :test #'equalp)))
		(if (equalp (cadr ct) 3) (setf base (remove "R" base :test #'equalp)))
	base))

(defun find-empty-tile (tab)
	"Find a tile in a 2d array"
	(let ((dims (array-dimensions tab)))
		(dotimes (i (first dims)) (dotimes (j (second dims)) do
			(if (null (aref tab i j))
				(return-from find-empty-tile (list i j)))))))

(defun create-initial-state (tab)
	"Creates a state"
	(make-state :board tab
				:clear-tile (find-empty-tile tab)
				:previous-play NIL
				:number-plays 0))

;; -----------------------------
;; OPERATOR AND GOAL FUNCTION
;; -----------------------------

(defun gen-successors (state)
	"Generates the successor states of a given state"
	(let ((gen-states NIL))
		(loop for move in (possible-moves state) do
			(push (move-tile (make-state :board (copy-array (state-board state))
										  :clear-tile (state-clear-tile state)
										  :previous-play (state-previous-play state)
										  :number-plays (state-number-plays state))
							  move)
				  gen-states))
	gen-states))

(defun is-goal-state (state)
	"Checks if a given state is the goal state"
	(equalp (state-board state) COMPLETE-BOARD))

;; -----------------------------
;; HEURISTICS AND COST FUNCTIONS
;; -----------------------------

(defun out-place-heuristic (state)
	"This heuristic checks how many tiles are out of place compared to the goal state"
	(let ((dims (array-dimensions (state-board state)))
		  (out 0))
		(dotimes (i (first dims)) (dotimes (j (second dims)) do
			(if (not (equalp (aref COMPLETE-BOARD i j) (aref (state-board state) i j)))
				(incf out))))
	out))

(defun cost-function (state)
	"This function gives the cost of the state"
	(state-number-plays state))

;; -----------------------------
;; SOLVE FUNCTION
;; -----------------------------

(defun solve-problem (table search-type)
	(let ((statelist (car (procura (cria-problema (create-initial-state table)
											(list #'gen-successors)
											:objectivo? #'is-goal-state
											:custo #'cost-function
											:heuristica #'out-place-heuristic)
								search-type)))
		 (boardlist))
		(dolist (s statelist)
			(setf boardlist (append boardlist (list (state-board s)))))
		boardlist))

