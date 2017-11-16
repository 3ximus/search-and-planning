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
	name                  ; string name of the instance
	vehicle.capacity      ; integer maximum vehicle capacity
	vehicles.number       ; integer maximum number of vehicles (vehicle tours)
	max.tour.length       ; float maximum tour length
	customer.locations    ; ordered list of locations (each location has 3 elements (id x y)) the first element is the depot
	customer.demand)      ; ordered list of demands (each demand has 2 elements (location-id demand-integer)) first element is the depot with demand value at zero

(defstruct state
	vehicle-routes                ; array of lists containing the location history of each vehicle (current location being the first element)
	unvisited-locations           ; hash table containing only keys with city ids
	number-unvisited-locations    ; number of unvisited cities
	remaining-tour-length         ; array with remaining tour length for each vehicle
	remaining-capacity)           ; array with remaining cargo for each vehicle

;; STATE ACCESSORS -------------------------------------

(defun vehicle-route (state &optional vehicle)
	"get vehicle route, if vehicle is omited the current vehicle is used"
	(if (null vehicle) (setf vehicle (get-current-vehicle state)))
	(aref (state-vehicle-routes state) vehicle))

(defun remaining-capacity (state &optional vehicle)
	"get remaining capacity of a vehicle, if vehicle is omited the current vehicle is used"
	(if (null vehicle) (setf vehicle (get-current-vehicle state)))
	(aref (state-remaining-capacity state) vehicle))

(defun remaining-length (state &optional vehicle)
	"get remaining length of a vehicle trip, if vehicle is omited the current vehicle is used"
	(if (null vehicle) (setf vehicle (get-current-vehicle state)))
	(aref (state-remaining-tour-length state) vehicle))

(defun remove-location (state location) ; NOTE this returns a copy of the hash table with the element deleted
	(let ((new-hash (copy-hash-table (state-unvisited-locations state))))
		(remhash location new-hash)
	new-hash))

(defun get-current-vehicle (state)
	(let ((nr-vehicles (length (state-vehicle-routes state))))
		(loop for i from 0 to (- nr-vehicles 1)
			do (if (not (and (> (length (vehicle-route state i)) 1) 	; has traveled
							 (equalp (car (vehicle-route state i)) 0))) ; is not back at the depot
					(return i)))))

(defun get-unvisited-customer-ids (state)
	(loop for key being the hash-keys of (state-unvisited-locations state) collect key))

;; -----------------------------
;; GLOBAL
;; -----------------------------

(defvar *vrp-data*)
(defvar *customer-hash*)

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

(defun copy-vrp (vrp-prob)
	(make-vrp :name 			  (vrp-name vrp-prob)
			  :vehicle.capacity   (vrp-vehicle.capacity vrp-prob)
			  :vehicles.number 	  (vrp-vehicles.number vrp-prob)
			  :max.tour.length 	  (vrp-max.tour.length vrp-prob)))

(defun copy-hash-table (table)
	(let ((new-table (make-hash-table :test #'equalp :size (hash-table-size table))))
		(maphash #'(lambda(key value) (setf (gethash key new-table) value)) table)
	new-table))

(defun change-array-copy (arr pos new-val)
	"Change a pos (position) in arr (array) to new-val (new value) and return a new array copy with that change"
	(let ((new-arr (copy-array arr)))
		(setf (aref new-arr pos) new-val)
	new-arr))

;; -----------------------------
;; OPERATOR AND GOAL FUNCTION
;; -----------------------------

(defun create-initial-hash (locations)
	(let ((h (make-hash-table :test #'equalp)))
		(dolist (item (rest locations) h)
			(setf (gethash (car item) h) nil))))

(defun create-initial-state (problem)
	"Create a state from a vrp struct"
	(setf *vrp-data* (copy-vrp problem))
	(setf *customer-hash* (make-customer-hash (vrp-customer.locations problem) (vrp-customer.demand problem)))
	(setf *farthest-customer*
		(let ((max-distance 0))
			(dolist (item (rest (vrp-customer.locations problem)))
				(let ((distance (distance (get-depot-location) (rest item))))
					(if (> distance max-distance)
						(setf max-distance distance))))
		max-distance))
	(setf *demanding-customer*
		(let ((max-demand 0))
			(dolist (item (rest (vrp-customer.demand problem)))
				(let ((demand (cadr item)))
					(if (> demand max-demand)
						(setf max-demand demand))))
		max-demand))
  	(make-state
		:vehicle-routes
			(make-array (vrp-vehicles.number problem) :initial-contents (make-list (vrp-vehicles.number problem) :initial-element (list 0)))
		:unvisited-locations
			(create-initial-hash (vrp-customer.locations problem))
		:number-unvisited-locations
			(length (rest (vrp-customer.locations problem)))
		:remaining-tour-length
			(make-array (vrp-vehicles.number problem) :initial-contents (make-list (vrp-vehicles.number problem) :initial-element (vrp-max.tour.length  problem)))
		:remaining-capacity
			(make-array (vrp-vehicles.number problem) :initial-contents (make-list (vrp-vehicles.number problem) :initial-element (vrp-vehicle.capacity problem)))))

(defun gen-successors (state)
	"Generates the successor states of a given state"
	(let ((cv (get-current-vehicle state))
		  (cv-location NIL)
		  (generated-states NIL))

	;; If cv is null there are no active vehicles and the current state is not a solution (because a* didnt end).
	;; Return null to force backtracking.
	(if (null cv)
		(return-from gen-successors NIL)
		(setf cv-location (get-location (car (vehicle-route state cv)))))

	(dolist (customer-id (get-unvisited-customer-ids state))
		(let* ((customer-location (get-location customer-id))
			   (rem-tour-len (- (remaining-length state cv) (distance cv-location customer-location)))
			   (rem-capacity (- (remaining-capacity state cv) (get-demand customer-id))))
			(if (and (>= rem-tour-len (distance customer-location (get-depot-location))) (>= rem-capacity 0))
				(setf generated-states
					(cons (make-state :vehicle-routes (change-array-copy (state-vehicle-routes state) cv (cons customer-id (vehicle-route state cv)))
								  	  :unvisited-locations (remove-location state customer-id)
								  	  :number-unvisited-locations (1- (state-number-unvisited-locations state))
								  	  :remaining-tour-length (change-array-copy	(state-remaining-tour-length state) cv rem-tour-len)
									  :remaining-capacity 	 (change-array-copy (state-remaining-capacity state)    cv rem-capacity))
						  generated-states)))))

	;; If generated-states is null then there are no other positions that that particular vehicle can travel to.
	;; Must return to depot.
	(if (null generated-states)
		(cons (make-state   :vehicle-routes (change-array-copy (state-vehicle-routes state) cv (cons 0 (vehicle-route state cv))) ; has to go back to the depot
							:unvisited-locations 		(state-unvisited-locations state)
							:number-unvisited-locations (state-number-unvisited-locations state)
							:remaining-tour-length 		(change-array-copy	(state-remaining-tour-length state) cv (distance cv-location (get-depot-location)))
							:remaining-capacity 	 	(state-remaining-capacity state))
						  generated-states)
		generated-states)
	))

(defun is-goal-state (state)
	"Checks if a given state is the goal state"
	(equalp (state-number-unvisited-locations state) 0))

;; -----------------------------
;; HEURISTICS AND COST FUNCTIONS
;; -----------------------------

(defun heuristic (state)
	0)

(defun alternative-heuristic (state)
	0)

; The cost is calculated by adding the distance to get to this state from the previous and the diference to the maximum demand (it should have lower costs for customers with high demand)
; NOTE @AndreSobral since distance is more important i gave it a factor of 1.5 -- if this remains it must be tweaked
(defun cost-function (state)
	"This function gives the cost of a state"
	(let* ((cv (get-current-vehicle state))
		  (cv-id (car (vehicle-route state cv)))
		  (distance-gethere (if (equalp cv-id 0) 0 (distance (get-location cv-id) (get-location (cadr (vehicle-route state cv)))))) ; TODO if this cost function turns out to be viable this value should be saved to the state since its calculated when generating it
		  (demand-gethere (if (equalp cv-id 0) 0 (get-demand (car (vehicle-route state cv))))))
		(+ (- *demanding-customer* demand-gethere) (* 1.5 distance-gethere))))

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
				(a* (cria-problema (create-initial-state problema)
										(list #'gen-successors)
										:objectivo? #'is-goal-state
										:custo #'cost-function
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
				(iterative-sampling (create-initial-state problema)))  ; TODO
			((string-equal tipo-procura "simulated.annealing.or.genetic.algoritm")
				(simulated-annealing
					(create-problem-simulated-annealing
						(create-initial-state problema)
						;#'gen-successors
						#'get-first-solution ; PLACEHOLDER
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
