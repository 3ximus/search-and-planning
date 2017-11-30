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
	neighbor
	state-value ; evaluate a state to determine how good it is
	schedule ; cooling schedule function
	initial-schedule-value) ; initial value for the scheduling function

(defun create-problem-simulated-annealing (initial-state neighbor
											&key state-value schedule initial-schedule-value)
	(make-problem :initial-state initial-state
				  :neighbor neighbor
				  :state-value state-value
				  :schedule schedule
				  :initial-schedule-value initial-schedule-value))

(defun get-random-element (some-list)
	"Get a random element from a list"
	(nth (random (length some-list)) some-list))

(defun check-probability (delta-worse temp)
	"Returns true with probability e^ΔE/T"
	(<= (/ (random 100) 100) (exp (/ delta-worse temp))))

(defun assess-path-insertions (id path remaining-length remaining-capacity &key (index 1))
	"Verify all insertions in path and returns the best one, takes remaining length and capacity into account"
	(when (equalp path '(0)) (return-from assess-path-insertions (values most-positive-fixnum nil))) ; returns largest number possible
	(let ((cost (insertion-cost (car path) (cadr path) id)))
		(multiple-value-bind (rest-cost rest-index) (assess-path-insertions id (cdr path) remaining-length remaining-capacity :index (1+ index))
			; verify remaining length and capacity, if not enough return large cost so it doesn't get selected
 			(when (or (> (get-demand id) remaining-capacity) (< remaining-length cost))
				(if (null rest-index) ; if a valid solution hasn't been found
					(return-from assess-path-insertions (values most-positive-fixnum nil))) ; then still wont be it
					(return-from assess-path-insertions (values rest-cost rest-index))) ; else pass previous solution
			; decide if we return the cost gathered by recursion or the current one
			(if (< cost rest-cost) (values cost index) (values rest-cost rest-index)))))

(defun do-sector-insertion (state id)
	"Insert id in the corresponding vehicle acording to slice vectors slice"
	(dotimes (i (vrp-vehicles.number *vrp-data*))
		;(when (is-inside-sector i id) ; NOTE used in equal-slice-sectors
		(when (find id (aref *sweep-sectors* i))
			(multiple-value-bind (cost index) (assess-path-insertions id (get-vehicle-route state i) (get-remaining-length state i) (get-remaining-capacity state i))
				(when (< (get-remaining-length state i) cost) (return ))
				(insert-customer-on-path state id i index cost)
				(return )))))

;; ---------------------------------
;; INITIAL SOLUTION AND NEIGHBORHOOD
;; ---------------------------------

(defun initial-solution (zero-state)
	"get the first solution for the simulated annealing problem"
	(dolist (cid (get-unvisited-customer-ids zero-state))
		(do-sector-insertion zero-state cid))
	(return-from initial-solution zero-state))

(defun delete-position (n list)
	"Delete element at position - destructive!"
	(if (zerop n) (cdr list)
		(let ((cons (nthcdr (1- n) list)))
			(when cons (setf (cdr cons) (cddr cons))) list)))

(defun shift (state v1 i1 v2)
	"Shift position i1 from v1 path to v2 path
	NOTE THIS CHANGES THE GIVEN STATE"
	(let ((p1 (get-vehicle-route state v1))
			(p2 (get-vehicle-route state v2)))
		(setf tmp (nth i1 p1))
		(multiple-value-bind (cost index) (assess-path-insertions tmp p2 (get-remaining-length state v2) (get-remaining-capacity state v2))
			(when (< (get-remaining-length state v2) cost) (return-from shift NIL))
			(insert-customer-on-path state tmp v2 index cost)
			(delete-position i1 p1))
	T))

(defun interchange (state v1 i1 v2 i2)
	"Interchange 2 positions (i1 i2) in 2 vehicle (v1 v2) paths,
	NOTE THIS CHANGES THE GIVEN STATE"
	(let ((p1 (get-vehicle-route state v1))
			(p2 (get-vehicle-route state v2)))
		(let ((ni1 (nth i1 p1)) (ni2 (nth i2 p2))
				(p1a (nth (1- i1) p1)) (p1b (nth (1+ i1) p1))
				(p2a (nth (1- i2) p2)) (p2b (nth (1+ i2) p2)))
		(let ((added-length-v1 (- (insertion-cost p1a p1b ni2) (insertion-cost p1a p1b ni1)))
				(added-length-v2 (- (insertion-cost p2a p2b ni1) (insertion-cost p2a p2b ni2)))
				(added-capacity-v1 (- (get-demand ni2) (get-demand ni1)))
				(added-capacity-v2 (- (get-demand ni1) (get-demand ni2))))
		(when (or (> added-capacity-v1 (get-remaining-capacity state v1)) (> added-capacity-v2 (get-remaining-capacity state v2))
				  (> added-length-v1 (get-remaining-length state v1)) (> added-length-v2 (get-remaining-length state v2)))
			(return-from interchange NIL)) ; then
		(setf (nth i1 p1) (nth i2 p2))
		(setf (nth i2 p2) ni1)
		(set-remaining-capacity state (- (get-remaining-capacity state v1) added-capacity-v1) v1)
		(set-remaining-capacity state (- (get-remaining-capacity state v2) added-capacity-v2) v2)
		(set-remaining-length state (- (get-remaining-length state v1) added-length-v1) v1)
		(set-remaining-length state (- (get-remaining-length state v2) added-length-v2) v2)))
	T))

(defun shift-process ())

(defun interchange-process (state vehicle)
	"Returns all neighbors by means of interchange with other adjacent vehicle paths, exchanging a costumer in the route with another from other route"
	(let ((path (get-vehicle-route state vehicle)) (states NIL)
			(vb (mod (1- vehicle) (vrp-vehicles.number *vrp-data*))) ; vehicle after
			(va (mod (1+ vehicle) (vrp-vehicles.number *vrp-data*)))) ; vehicle before
	(loop for k from 1 to (- (length path) 2) do
		(loop for i from 1 to (- (length (get-vehicle-route state vb)) 2) do
			(let ((new-state (copy-full-state state)))
				(when (interchange new-state vb i vehicle k)
					(setf states (cons new-state states)))))
		(loop for j from 1 to (- (length (get-vehicle-route state va)) 2) do
			(let ((new-state (copy-full-state state)))
				(when (interchange new-state va j vehicle k)
					(setf states (cons new-state states))))))
	states))

(defun closest-interchange-process (state)
	"Returns all neighbors by means of interchange with any other vehicle route that has points adjacent to a given vehicle"
	)

(defun neighbor-states (state)
	"Get all neighbor states"
	(let ((nstates NIL))
	(dotimes (i (vrp-vehicles.number *vrp-data*))
		(setf nstates (nconc nstates (interchange-process state i))))
	nstates))

;; -----------------------------
;; VALUE OF A STATE
;; -----------------------------

(defun state-value (state)
	"The value of the state is the remaining length, goal states in which the vehicles traveled less have higher value"
	(reduce #'+ (state-remaining-tour-length state)))

;; -----------------------------
;; COOLING SCHEDULES
;; -----------------------------

; Good values for this ALPHA range from 0.8 to 0.99 (higher ALPHA => cools slower)
(defconstant ALPHA 0.97) ; used for exponential-multiplicative cooling
(defconstant INITIAL_TEMP 100) ; used for exponential-multiplicative cooling

(defun exponential-multiplicative-cooling (delta-t &key initial-temp)
	"Cooling scheduler Tk = T0 * a^k"
	(if (null initial-temp) (setf initial-temp INITIAL_TEMP))
	(* initial-temp (expt ALPHA delta-t)))


(defun logarithmic-multiplicative-cooling (delta-t &key initial-temp)
	"Cooling scheduler  = T0 / ( 1+ a * log(1 + k) )"
	0)

;; -----------------------------

(defun simulated-annealing (problem)
	(let ((current (problem-initial-state problem))
		  (time-value 0)
		  (successors nil))
		(loop while (and (incf time-value)
						 (setf successors (funcall (problem-neighbor problem) current))
						 (incf *nos-expandidos*)
						 (incf *nos-gerados* (length successors))) do
			(let* ((temp (funcall (problem-schedule problem) time-value :initial-temp (problem-initial-schedule-value problem)))
				  (next (get-random-element successors))
				  (delta-worse (- (funcall (problem-state-value problem) current) (funcall (problem-state-value problem) next))))
				(if (equalp temp 0)
					(return-from simulated-annealing current))
				(if (< delta-worse 0)
					(setf current next)
					(if (check-probability delta-worse temp)
						(setf current next)))
				;(format t "~D  .. ~D xx  ~D~% " temp (state-value current) delta-worse)  ; PLACEHOLDER
				(log-state current)
				(break )
				))
	current))
