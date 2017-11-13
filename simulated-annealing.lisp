; SIMULATED ANNEALING
; current ← MAKE-NODE (problem.INITIAL-STATE)
;   for t = 1 to ∞ do
;   T ← schedule(t)
;   if T = 0 then return current
;   next ← a randomly selected successor of current
;   ΔE ← next.VALUE – current.VALUE
;   if ΔE > 0 then current ← next
;   else current ← next only with probability e^ΔE/T

(defstruct problem
	initial-state
	gen-successors
	schedule ; cooling schedule function
	state-value) ; evaluate a state to determine how good it is

(defun create-problem-simulated-annealing (initial-state gen-successors
											&key schedule state-value)
	(make-problem :initial-state initial-state
				  :gen-successors gen-successors
				  :schedule schedule
				  :state-value state-value))

(defun get-random-element (some-list)
	"get a random element from a list"
	(nth (random (length some-list) some-list)))

(defun check-probability (delta-worse temp)
	"returns true with probability e^ΔE/T"
	(<= (/ (random 100) 100) (exp (/ delta-worse temp))))

;; -----------------------------
;; COOLING SCHEDULES
;; -----------------------------

(defun exponential-multiplicative-cooling (delta-t &key initial-temp)
	"Tk = T0 * a^k"
	0)


(defun logarithmic-multiplicative-cooling (delta-t &key initial-temp)
	"Tk = T0 / ( 1+ a * log(1 + k) )"
	0)

;; -----------------------------

(defun simulated-annealing (problem)
	(let ((current (problem-initial-state problem))
		  (time-value 1)
		  (successors nil))
		(loop while (and (incf time) (setf successors (problem-gen-successors problem)))
			(let ((temp (problem-schedule time-value)
				  (next (get-random-element successors))
				  (delta-worse (- (problem-state-value current) (problem-state-value next)))))
				(if (equalp temp 0)
					(return current))
				(if (> delta-worse 0)
					(setf current next)
					(if (check-probability delta-worse temp)
						(setf current next))))))))
