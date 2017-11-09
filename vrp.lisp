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
	unvisited-locations           ; list of unvisited locations
	number-unvisited-locations    ; number of unvisited cities
	remaining-tour-length
	remaining-capacity)

(defvar *vrp-data*)
(defvar *customerHash*)

;; -----------------------------
;; BASIC FUNCTIONS
;; -----------------------------

(defun distance (locationA locationB) ; locationA and locationB are locations from customerHash -> (x y)
	"Calculates Euclidean distance between 2 locations"
	(sqrt (+ (expt (- (cadr locationB) (cadr locationA)) 2) (expt (- (car locationB) (car locationA)) 2))))

(defun lst-to-array (lst)
	(let ((lst-len (length lst)))
	(make-array (list lst-len) :initial-contents lst)))

(defun copy-vrp (vrp-prob)
	(make-vrp :name 			  (vrp-name vrp-prob)
			  :vehicle.capacity   (vrp-vehicle.capacity vrp-prob)
			  :vehicles.number 	  (vrp-vehicles.number vrp-prob)
			  :max.tour.length 	  (vrp-max.tour.length vrp-prob)))
			  ;:customer.locations (lst-to-array (vrp-customer.locations vrp-prob)) ; TODO remove now that we have hash tables?
			  ;:customer.demand 	  (lst-to-array (vrp-customer.demand vrp-prob)))) ; TODO remove now that we have hash tables?

(defun change-array-copy (arr pos new-val)
	"Change a pos (position) in arr (array) to new-val (new value) and return a new array copy with that change"
	(let ((new-array (copy-array arr)))
		(setf (aref new-array pos) new-val)
	new-array))

;; Each item of the list, must have the "id" as the first value -- i.e (id value1 value2 ...)
(defun lst-to-hash (lst)
	(let ((hash (make-hash-table :test #'equalp)))
		(dolist (lst-item lst hash)
			(setf (gethash (car lst-item) hash) (rest lst-item)))))

;; Each item of the list, must have the "id" as the first value -- i.e (id value)
(defun add-lst-to-hash (lst hash)
	(dolist (lst-item lst hash)
		(setf (gethash (car lst-item) hash) (cons (gethash (car lst-item) hash) (second lst-item)))))

;; Hash functions
(defun makeCustomerHash (locations demands)
	(let ((customerHash (lst-to-hash locations)))
		(add-lst-to-hash demands customerHash)))

(defun getCustomerLocation(hash id)
	(car (gethash id hash)))

(defun getCustomerDemand(hash id)
	(cdr (gethash id hash)))

(defun removeCustomerInfo (id hash)
	(remhash id hash))

;; -----------------------------
;; OPERATOR AND GOAL FUNCTION
;; -----------------------------

(defun create-initial-state (problem)
	"Create a problem from a vrp struct"
	(setf *vrp-data* (copy-vrp problem))
	(setf *customerHash* (makeCustomerHash (vrp-customer.locations problem) (vrp-customer.demand problem)))
  	(make-state
		:vehicle-routes
			(make-array (vrp-vehicles.number problem) :initial-contents
				(let ((lst ())) ; make array with list of locations, starting at the depot
					(dotimes (i (vrp-vehicles.number problem))
						(setf lst (cons (list 0) lst)))
				lst))
		:unvisited-locations (rest (vrp-customer.locations problem)); PLACEHOLDER replace with hash table
		:number-unvisited-locations (length (vrp-customer.locations problem))
		:remaining-tour-length
		 	(make-array (vrp-vehicles.number problem) :initial-contents
				(let ((lst ())) ; make array with remaining tour length for each vehicle
					(dotimes (i (vrp-vehicles.number problem))
						(setf lst (cons (vrp-max.tour.length problem) lst)))
				lst))
		:remaining-capacity
			(make-array (vrp-vehicles.number problem) :initial-contents
				(let ((lst ())) ; make array with remaining cargo for each vehicle
					(dotimes (i (vrp-vehicles.number problem))
						(setf lst (cons (vrp-vehicle.capacity problem) lst)))
				lst))))

(defun gen-successors (state)
	"Generates the successor states of a given state"
	(let* ((current-vehicle
			(loop for i from 0 to (vrp-vehicles.number *vrp-data*) do
				(if (not (and (> (length (aref (state-vehicle-routes state) i)) 1) ; has traveled
							  (equalp (car (aref (state-vehicle-routes state) i)) 0))) ; is not back at the depot
					(return i))))
		  (current-vehicle-location (aref (state-vehicle-routes state) current-vehicle))
		  (generated-states NIL))
	(dolist (location (state-unvisited-locations)) ; PLACEHOLDER location will be an integer and we iterate over hash keys
		(cons (make-state :vehicle-routes (change-array-copy (state-vehicle-routes state) current-vehicle (cons location (aref (state-vehicle-routes state) current-vehicle)))
						  :unvisited-locations NIL ; PLACEHOLDER replace with hash table removal of "location" for unvisited locations
						  :number-unvisited-locations (1- (state-number-unvisited-locations))
						  :remaining-tour-length (change-array-copy (state-remaining-tour-length state) current-vehicle
						  							(- (aref (state-remaining-tour-length state) current-vehicle) (distance (getCustomerLocation current-vehicle-location) (getCustomerLocation location))))
						  :remaining-capacity (change-array-copy (state-remaining-capacity state) current-vehicle (- (aref (state-remaining-capacity state) current-vehicle) (getCustomerDemand location))))
			generated-states))))

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

(defun cost-function (state)
	"This functionn gives the cost of a state"
	0)

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
				(simulated-annealing (create-initial-state problema)))  ; TODO
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
