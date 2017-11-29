;;; Procura e planeamento
;;; Vehicle Routing Problem

(in-package :user)

(load "procura")  ; default basic searches
(load "iterative-sampling")
(load "genetic")
(load "simulated-annealing")

;; -----------------------------
;; DEFINITIONS
;; -----------------------------

(defstruct vrp
	name               ; string name of the instance
	vehicle.capacity   ; integer maximum vehicle capacity
	vehicles.number    ; integer maximum number of vehicles (vehicle tours)
	max.tour.length    ; float maximum tour length
	customer.locations ; ordered list of locations (each location has 3 elements (id x y)) the first element is the depot
	customer.demand)   ; ordered list of demands (each demand has 2 elements (location-id demand-integer)) first element is the depot with demand value at zero

(defstruct state
	vehicle-routes             ; array of lists containing the location history of each vehicle (current location being the first element)
	current-vehicle            ; index of the current vehicle, nil if no vehicle is active (finished)
	transition-cost            ; cost of transitioning to this state in terms of added distance
	inserted-pair              ; pair inserted (vehicle customer)
	unvisited-locations        ; hash table containing only keys with city ids
	number-unvisited-locations ; number of unvisited cities
	remaining-tour-length      ; array with remaining tour length for each vehicle
	remaining-capacity)        ; array with remaining cargo for each vehicle

;; STATE ACCESSORS -------------------------------------

; --- getters
(defun get-vehicle-route (state &optional vehicle)
	"get vehicle route, if vehicle is omited the current vehicle is used"
	(if (null vehicle) (setf vehicle (get-current-vehicle state)))
	(aref (state-vehicle-routes state) vehicle))

(defun get-remaining-capacity (state &optional vehicle)
	"get remaining capacity of a vehicle, if vehicle is omited the current vehicle is used"
	(if (null vehicle) (setf vehicle (get-current-vehicle state)))
	(aref (state-remaining-capacity state) vehicle))

(defun get-remaining-length (state &optional vehicle)
	"get remaining length of a vehicle route, if vehicle is omited the current vehicle is used"
	(if (null vehicle) (setf vehicle (get-current-vehicle state)))
	(aref (state-remaining-tour-length state) vehicle))

(defun get-current-vehicle (state)
	(state-current-vehicle state))

(defun get-unvisited-customer-ids (state)
	(loop for key being the hash-keys of (state-unvisited-locations state) collect key))

; --- setters
(defun set-vehicle-route (state route &optional vehicle)
	"set vehicle route, if vehicle is omited the current vehicle is used"
	(if (null vehicle) (setf vehicle (get-current-vehicle state)))
	(setf (aref (state-vehicle-routes state) vehicle) route))

(defun set-remaining-capacity (state new-cap &optional vehicle)
	"set remaining capacity of a vehicle, if vehicle is omited the current vehicle is used"
	(if (null vehicle) (setf vehicle (get-current-vehicle state)))
	(setf (aref (state-remaining-capacity state) vehicle) new-cap))

(defun set-remaining-length (state new-len &optional vehicle)
	"set remaining length of a vehicle route, if vehicle is omited the current vehicle is used"
	(if (null vehicle) (setf vehicle (get-current-vehicle state)))
	(setf (aref (state-remaining-tour-length state) vehicle) new-len))

; --- others
(defun remove-location (state location) ; NOTE this returns a copy of the hash table with the element deleted
	(let ((new-hash (copy-hash-table (state-unvisited-locations state))))
		(remhash location new-hash)
	new-hash))

;; -----------------------------
;; GLOBAL
;; -----------------------------

(defvar *vrp-data*) ; backup of the problem data
(defvar *customer-hash*)
(defvar *vector-slices*)

; These values will be used as reference for cost and heuristic as the value
(defvar *farthest-customer*) ; will hold the largest distance from the depot
(defvar *demanding-customer*) ; will hold the customer with highest demand

; GLOBAL HASH ACCESSOR FUNCTIONS
(defun get-depot-location ()
	(get-location 0))

(defun get-location(id)
	(car (gethash id *customer-hash*)))

(defun get-demand(id)
	(second (gethash id *customer-hash*)))

(defun set-farthest-customer (problem)
	"Set the farthest customer variable with given problem"
	(setf *farthest-customer*
		(let ((max-distance 0))
			(dolist (item (rest (vrp-customer.locations problem)))
				(let ((distance (distance (get-depot-location) (rest item))))
					(if (> distance max-distance)
						(setf max-distance distance))))
		max-distance)))

(defun set-demanding-customer (problem)
	"Set the farthest customer variable with given problem"
	(setf *demanding-customer*
		(let ((max-demand 0))
			(dolist (item (rest (vrp-customer.demand problem)))
				(let ((demand (cadr item)))
					(if (> demand max-demand)
						(setf max-demand demand))))
		max-demand)))

(defun make-customer-hash (locations demands)
	"Creates a new hash-table from the location and demands lists. Its indexed by the customer ID."
	(let ((customer-hash (make-hash-table :test #'equalp)))
		(loop for location in locations
			  for demand   in demands
			  do (setf (gethash (car location) customer-hash) (list (rest location) (second demand)))) ; [id] -> ((x y) demand)
		customer-hash))

;; -----------------------------
;; BASIC FUNCTIONS
;; -----------------------------

(defun distance (locationA locationB) ; locationA and locationB are locations from customer-hash -> (x y)
	"Calculates Euclidean distance between 2 locations"
	(sqrt (+ (expt (- (cadr locationB) (cadr locationA)) 2) (expt (- (car locationB) (car locationA)) 2))))

(defun op-2d (op v1 v2)
	"Apply op to both vectors"
	(list (funcall op (car v1) (car v2)) (funcall op (cadr v1) (cadr v2))))

(defun copy-vrp (vrp-prob)
	(make-vrp :name 			  (vrp-name vrp-prob)
			  :vehicle.capacity   (vrp-vehicle.capacity vrp-prob)
			  :vehicles.number 	  (vrp-vehicles.number vrp-prob)
			  :customer.locations (vrp-customer.locations vrp-prob)
			  :max.tour.length 	  (vrp-max.tour.length vrp-prob)))

(defun copy-hash-table (table)
	(let ((new-table (make-hash-table :test #'equalp :size (hash-table-size table))))
		(maphash #'(lambda(key value) (setf (gethash key new-table) value)) table)
	new-table))

(defun copy-full-state (state)
	"Create a full copy of a state"
	(make-state :vehicle-routes (let ((n (copy-array (state-vehicle-routes state)))) (dotimes (i (vrp-vehicles.number *vrp-data*)) (setf (aref n i) (copy-list (aref n i)))) n) ; because copy-array doesn't make copy of the lists...
				:current-vehicle (state-current-vehicle state)
				:unvisited-locations (copy-hash-table (state-unvisited-locations state))
				:number-unvisited-locations (state-number-unvisited-locations state)
				:remaining-tour-length (copy-array (state-remaining-tour-length state))
				:remaining-capacity (copy-array (state-remaining-capacity state))))

(defun change-array-copy (arr pos new-val)
	"Change a pos (position) in arr (array) to new-val (new value) and return a new array copy with that change"
	(let ((new-arr (copy-array arr)))
		(setf (aref new-arr pos) new-val)
	new-arr))

(defun invalid-vehicle (state vehicle)
	(or (>= vehicle (length (state-vehicle-routes state))) (< vehicle 0)))

(defun invalid-index (vehicle-route index)
	(or (> index (length vehicle-route)) (< index 0)))

(defun insert-customer-on-path (state id vehicle index added-length)
	"Insert a customer ID in given vehicle path at specific index, added-length should contain how much the route will be increased"
	(if (invalid-vehicle state vehicle)
		(error "~S is not a valid vehicle-id." vehicle)
		(let ((vehicle-route (get-vehicle-route state vehicle)))
			(if (invalid-index vehicle-route index)
				(error "Index out-of-range [input: ~D, Maxlength: ~D] in vehicle-route of vehicle ~D." index (length vehicle-route) vehicle)
				(progn
					(if (= index 0) ; push normal
						(push id vehicle-route)
						(push id (cdr (nthcdr (1- index) vehicle-route))))
					; update the state accordingly
					(set-vehicle-route state vehicle-route vehicle)
					(set-remaining-capacity state (- (get-remaining-capacity state vehicle) (get-demand id)) vehicle)
					(set-remaining-length state (- (get-remaining-length state vehicle) added-length) vehicle)
					(setf (state-transition-cost state) added-length)
					(setf (state-inserted-pair state) (list vehicle id))
					(setf (state-unvisited-locations state) (remove-location state id))
					(setf (state-number-unvisited-locations state) (1- (state-number-unvisited-locations state))))))))

(defun insertion-cost (a b c)
	"Calculate cost in terms of aditional length when inserting c between a and b, Cost = c[a-c] + c[b-c] - c[a-b]
	NOTE LESS is BETTER"
	(let ((dac (distance (get-location a) (get-location c)))
		  (dbc (distance (get-location b) (get-location c)))
		  (dab (distance (get-location a) (get-location b))))
		(- (+ dac dbc) dab)))

;; -----------------------------
;; OPERATOR AND GOAL FUNCTION
;; -----------------------------

(defun create-initial-hash (locations)
	(let ((h (make-hash-table :test #'equalp)))
		(dolist (item (rest locations) h)
			(setf (gethash (car item) h) nil))))

(defun create-initial-state (problem &optional (initial-route (list 0)))
	"Create a state from a vrp struct"
	(setf *vrp-data* (copy-vrp problem))
	(setf *customer-hash* (make-customer-hash (vrp-customer.locations problem) (vrp-customer.demand problem)))
	(setf *vector-slices* (make-array (vrp-vehicles.number *vrp-data*) :initial-contents (generate-circle-slices (vrp-vehicles.number *vrp-data*))))
	(set-farthest-customer problem)
	(set-demanding-customer problem)
  	(make-state
		:vehicle-routes
			(make-array (vrp-vehicles.number problem) :initial-contents
				(let ((lst NIL))
					(dotimes (i (vrp-vehicles.number problem) lst)
						(setf lst (cons (copy-list initial-route) lst))))) ; copy the list otherwise the same reference is used
		:current-vehicle 0
		:unvisited-locations
			(create-initial-hash (vrp-customer.locations problem))
		:number-unvisited-locations
			(length (rest (vrp-customer.locations problem)))
		:remaining-tour-length
			(make-array (vrp-vehicles.number problem) :initial-contents (make-list (vrp-vehicles.number problem) :initial-element (vrp-max.tour.length  problem)))
		:remaining-capacity
			(make-array (vrp-vehicles.number problem) :initial-contents (make-list (vrp-vehicles.number problem) :initial-element (vrp-vehicle.capacity problem)))))

(defun gen-successors (state) ; TODO make a copy of the state and change its values with the setter functions
	"Generates the successor states of a given state"
	(let ((cv (get-current-vehicle state))
		  (cv-location NIL)
		  (generated-states NIL))

	;; If cv is null there are no active vehicles and the current state is not a solution
	;; Return null to force backtracking.
	(if (null cv)
		(return-from gen-successors NIL)
		(setf cv-location (get-location (car (get-vehicle-route state cv)))))

	(dolist (customer-id (get-unvisited-customer-ids state))
		(let* ((customer-location (get-location customer-id))
			   (rem-tour-len (- (get-remaining-length state cv) (distance cv-location customer-location)))
			   (rem-capacity (- (get-remaining-capacity state cv) (get-demand customer-id))))
			(if (and (>= rem-tour-len (distance customer-location (get-depot-location))) (>= rem-capacity 0))
				(setf generated-states
					(cons (make-state :vehicle-routes (change-array-copy (state-vehicle-routes state) cv (cons customer-id (get-vehicle-route state cv)))
									  :current-vehicle (state-current-vehicle state)
									  :inserted-pair (list cv customer-id)
									  :unvisited-locations (remove-location state customer-id)
									  :number-unvisited-locations (1- (state-number-unvisited-locations state))
									  :remaining-tour-length (change-array-copy	(state-remaining-tour-length state) cv rem-tour-len)
									  :remaining-capacity 	 (change-array-copy (state-remaining-capacity state)    cv rem-capacity))
						  generated-states)))))

	;; If generated-states is null then there are no other positions that that particular vehicle can travel to.
	;; Must return to depot.
	(if (null generated-states)
		(cons (make-state   :vehicle-routes (change-array-copy (state-vehicle-routes state) cv (cons 0 (get-vehicle-route state cv))) ; has to go back to the depot
							:current-vehicle (if (not (>= (1+ (state-current-vehicle state)) (vrp-vehicles.number *vrp-data*))) (1+ (state-current-vehicle state)) nil)
							:unvisited-locations 		(state-unvisited-locations state)
							:number-unvisited-locations (state-number-unvisited-locations state)
							:remaining-tour-length 		(change-array-copy	(state-remaining-tour-length state) cv (distance cv-location (get-depot-location)))
							:remaining-capacity 	 	(state-remaining-capacity state))
						  generated-states)
		generated-states)
	))

(defun gen-vehicle-states (id vehicle path state &key (index 1))
	"This function is used by gen-successors-insertion-method to generate a list of states that result from the insertion of an id at each step of the path"
	(when (equalp path '(0)) (return-from gen-vehicle-states NIL))
	(let ((cost (insertion-cost (car path) (cadr path) id)))
 	; NOTE this last comparison is used to reduce number of generated states since it was a problem before
	(if (or (> (get-demand id) (get-remaining-capacity state vehicle)) (> cost (get-remaining-length state vehicle)) (> cost (/ *farthest-customer* 2)))
		(gen-vehicle-states id vehicle (cdr path) state :index (1+ index)) ; then
		(cons ; else
			(let ((new-state (copy-full-state state)))
			(insert-customer-on-path new-state id vehicle index cost)
			new-state)
		(gen-vehicle-states id vehicle (cdr path) state :index (1+ index))))))

(defun usable-vehicles (state)
	"Returns the amount of free vehicles to use (counts one with empty path and discards the rest)"
	(let ((stop NIL))
	(dotimes (i (vrp-vehicles.number *vrp-data*))
		(when (equalp (aref (state-vehicle-routes state) i) '(0 0))
			(if stop (return i) (setf stop T))))))

(defun gen-successors-insertion-method (state)
	"Generate successors states by adding each location to each vehicle path in each possible position
	NOTE this functions assumes each vehicle route starts with (list 0 0), so make sure its created that way"
	(log-state state) ; PLACEHOLDER
	(print *nos-expandidos*)
	;(break )
	(let ((gstates NIL))
	(dolist (id (get-unvisited-customer-ids state))
		(dotimes (vehicle (usable-vehicles state))
			(setf gstates (nconc gstates (gen-vehicle-states id vehicle (get-vehicle-route state vehicle) state)))))
	gstates))

(defun gen-successors-hybrid-approach (state)
	"Generate successors states by adding each location to the current vehicle path in each possible position
	NOTE this functions assumes each vehicle route starts with (list 0 0), so make sure its created that way"
	(let ((gstates NIL) (cv (get-current-vehicle state)))
	(dolist (id (get-unvisited-customer-ids state))
		(setf gstates (nconc gstates (gen-vehicle-states id cv (get-vehicle-route state cv) state))))
	(when (and (null gstates) (not (equalp cv (vrp-vehicles.number *vrp-data*)))) ; current vehicle couldn't fit more locations and its not the last vehicle
		(incf cv) (setf (state-current-vehicle state) cv) ; update cv on the state
		(dolist (id (get-unvisited-customer-ids state))
			(setf gstates (nconc gstates (gen-vehicle-states id cv (get-vehicle-route state cv) state)))))
	(log-state state) ; PLACEHOLDER
	(break )
	gstates))

(defun is-goal-state (state)
	"Checks if a given state is the goal state"
	(equalp (state-number-unvisited-locations state) 0))

;; -----------------------------
;; HEURISTICS AND COST FUNCTIONS
;; -----------------------------

(defun generate-circle-slices (vn)
	"Geretas list of vn simetric slices, in which vn is the number of vehicles"
	(let ((v1 '(30 0)) (vlist nil) (angle 0) (increment (/ 360 vn)) (dpl (get-depot-location)))
	(dotimes (i (1- vn))
		(incf angle increment)
		(let* ((rad_ang (/ (* angle pi) 180))
				(x (- (* (car v1) (cos rad_ang)) (* (cadr v1) (sin rad_ang))))
				(y (+ (* (car v1) (sin rad_ang)) (* (cadr v1) (cos rad_ang)))))
		(setf vlist (nconc vlist (list (op-2d #'+ (list x y) dpl)))))) ; append and translate vector to correct coordinates
	(cons (op-2d #'+ v1 dpl) vlist)))

(defun get-angle (a b)
	"Gets the angle at the depot between points a and b"
	; this function uses the law of cosines ->   cos(x) = ( r^2 + R^2 - d^2 ) / ( 2rR )
	;  where r and R are distance from (0,0) to a point, d is the distance between them and x is the angle at (0,0) between the 2 edges leading to both points
	(let ((p-distance (distance (get-depot-location) a))
		  (v-size (distance (get-depot-location) b))
		  (v-p-distance (distance a b)))
	(acos (/ (- (+ (* p-distance p-distance) (* v-size v-size)) (* v-p-distance v-p-distance)) (* 2 p-distance v-size)))))

(defun get-arc-distance (point vector)
	"Returns the size of the arc from point to vector"
	(* (distance (get-depot-location) point) (get-angle point vector)))

(defun heuristic (state)
	(when (null (state-inserted-pair state)) (return-from heuristic 0))
	(let ((slice-vector (aref *vector-slices* (car (state-inserted-pair state)))))
	(get-arc-distance (get-location (cadr (state-inserted-pair state))) slice-vector)))
	; TODO add the average of traveled distance here to favor longer paths

(defun alternative-heuristic (state)
	0)






; The cost is calculated by adding the distance to get to this state from the previous and the diference to the maximum demand (it should have lower costs for customers with high demand)
; NOTE @AndreSobral since distance is more important i gave it a factor of 1.5 -- if this remains it must be tweaked
(defun cost-function (state)
	"This function gives the cost of a state"
	0)

(defun insertion-cost-function (state)
	"Gives a cost of a state based on its transition cost,
		(the cost of the inserting the last costumer i.e. the added distance introduced by the insertion)"
	(state-transition-cost state))

;; -----------------------------
;; SOLVE FUNCTION
;; -----------------------------

(defun vrp (problema tipo-procura
        &key (profundidade-maxima most-positive-fixnum)
             (espaco-em-arvore? nil))
	"Solve instances of the Vehicle Routing Problem"
  (flet ((faz-a-procura (problema tipo-procura
             profundidade-maxima espaco-em-arvore?)
		(cond ((string-equal tipo-procura "a*.best.heuristic")
				(a* (cria-problema (create-initial-state problema (list 0 0))
										(list #'gen-successors-hybrid-approach)
										:objectivo? #'is-goal-state
										:custo #'insertion-cost-function
										:heuristica #'heuristic)
					:espaco-em-arvore? espaco-em-arvore?))
			((string-equal tipo-procura "a*.best.alternative.heuristic")
				(a* (cria-problema (create-initial-state problema)
										(list #'gen-successors)
										:objectivo? #'is-goal-state
										:custo #'cost-function
										:heuristica #'alternative-heuristic)
					:espaco-em-arvore? espaco-em-arvore?))
			((string-equal tipo-procura "iterative.sampling")
				(iterative-sampling
					(create-initial-state problema)
					:gen-successors #'gen-successors))
			((string-equal tipo-procura "simulated.annealing.or.genetic.algoritm")
				(simulated-annealing
					(create-problem-simulated-annealing
						(initial-solution (create-initial-state problema (list 0 0)))
						#'neighbor-states
						:schedule #'exponential-multiplicative-cooling
						:state-value #'state-value)))
		((string-equal tipo-procura "best.approach")
			(best-approach (create-initial-state problema))))))  ; TODO
(let ((*nos-gerados* 0)
	(*nos-expandidos* 0)
	(tempo-inicio (get-internal-run-time )))
	(let ((solucao (faz-a-procura problema tipo-procura
				profundidade-maxima
				espaco-em-arvore?)))
(list solucao
			(- (get-internal-run-time ) tempo-inicio)
			*nos-expandidos*
			*nos-gerados*)))))

;; -----------------------------
;; OTHER FUNCTIONS
;; -----------------------------

(defun log-state (state &optional (clusters *vector-slices*))
	"Log state to file to be made into a graph"
	(with-open-file (str "out.txt"
						:direction :output
						:if-exists :supersede
						:if-does-not-exist :create)
		(format str "~S~%~%~S~%~%CLUSTERS~S" *vrp-data* state clusters)))
