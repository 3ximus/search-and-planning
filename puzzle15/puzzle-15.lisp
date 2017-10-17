;;; Procura e planeamento
;;; 15 Puzzle Problem

(load "procura")

(setf COMPLETE-BOARD #2A((1 2 3 4)
						 (5 6 7 8)
						 (9 10 11 12)
						 (13 14 15 NIL)))

(defstruct state
	board
	clear-tile  ; position of the cleared tile
	previous-play)

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

(defun move-piece (state move)
	"Move piece in a given direction"
	(let ((board (state-board state)) (ct (state-clear-tile state)))
		(cond ((equalp move "L") (setf (state-clear-tile state) (swap board ct (list (car ct) (1- (cadr ct))))))
			  ((equalp move "R") (setf (state-clear-tile state) (swap board ct (list (car ct) (1+ (cadr ct))))))
			  ((equalp move "U") (setf (state-clear-tile state) (swap board ct (list (1- (car ct)) (cadr ct)))))
			  ((equalp move "D") (setf (state-clear-tile state) (swap board ct (list (1+ (car ct)) (cadr ct))))))
	(setf (state-previous-play state) move)
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
				:previous-play NIL))

;; -----------------------------
;; OPERATOR AND GOAL FUNCTION
;; -----------------------------

(defun gen-successors (state)
	"Generates the successor states of a given state"
	(let ((gen-states NIL))
		(loop for move in (possible-moves state) do
			(push (move-piece (make-state :board (copy-array (state-board state))
										  :clear-tile (state-clear-tile state)
										  :previous-play (state-previous-play state))
							  move)
				  gen-states))
	gen-states))

(defun is-goal-state (state)
	"Checks if a given state is the goal state"
	(equalp (state-board state) COMPLETE-BOARD))

;; -----------------------------
;; HEURISTICS AND COST FUNCTIONS
;; -----------------------------

;;  -- these are not the funcitons you are looking for


;; -----------------------------
;; SOLVE FUNCTION
;; -----------------------------

(defun solve-problem (table search-type)
	(procura (cria-problema (create-initial-state table)
							(list #'gen-successors)
							:objectivo? #'is-goal-state
							:custo NIL
							:heuristica NIL)
			 search-type))

;;;;;;;;;;; running tests
(solve-problem (make-array '(4 4) :initial-contents '((1 2 3 4) (5 6 7 8) (13 9 10 11) (14 nil 15 12))) "profundidade")

;(find (make-array 3 :initial-contents '(1 2 3)) (list (make-array 3 :initial-contents '(2 4 5)) (make-array 3 :initial-contents '(1 2 3)) (make-array 3 :initial-contents '(1 3 4))) :test #'equalp)

