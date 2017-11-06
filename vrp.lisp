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

;; -----------------------------
;; BASIC FUNCTIONS
;; -----------------------------

(defun distance (locationA locationB)
	"Calculates Euclidean distance between 2 locations"
	(sqrt (+ (expt (- (cadr locationB) (cadr locationA)) 2) (expt (- (car (last locationB)) (car (last locationA))) 2))))

;; -----------------------------
;; OPERATOR AND GOAL FUNCTION
;; -----------------------------

(defun create-internal-problem (vrp-struct)
	"Create a problem from a vrp struct"
	vrp-struct) ; NOTE for now it does nothing

(defun gen-successors (state)
	"Generates the successor states of a given state"
	NIL)

(defun is-goal-state (state)
	"Checks if a given state is the goal state"
	T)

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
				(a* (cria-problema (create-internal-problem problema)
										(list #'gen-successors)
										:objectivo? #'is-goal-state
										:custo #'cost-function
										:heuristica #'heuristic)
					:espaco-em-arvore? espaco-em-arvore?))
			((string-equal tipo-procura "a*.best.alternative.heuristic")
				(a* (cria-problema (create-internal-problem problema)
										(list #'gen-successors)
										:objectivo? #'is-goal-state
										:custo #'cost-function
										:heuristica #'alternative-heuristic)
					:espaco-em-arvore? espaco-em-arvore?))
			((string-equal tipo-procura "iterative.sampling")
				(iterative-sampling (create-internal-problem problema)))  ; TODO
			((string-equal tipo-procura "simulated.annealing.or.genetic.algoritm")
				(simulated-annealing (create-internal-problem problema)))  ; TODO
			((string-equal tipo-procura "best.approach")
				(best-approach (create-internal-problem problema))))))  ; TODO
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
