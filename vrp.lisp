;;; Procura e planeamento
;;; Vehicle Routing Problem

(in-package :user)

(load "procura")  ; default basic searches
(load "iterative-sampling")
(load "genetic")
(load "simulated-annealing")

;; -----------------------------
;; STRUCTURES
;; -----------------------------

(defstruct vrp
  name                  ; string name of the instance
  vehicle.capacity      ; integer maximum vehicle capacity
  vehicles.number       ; integer maximum number of vehicles (vehicle tours)
  max.tour.length       ; float maximum tour length
  customer.locations    ; ordered list of locations (each location has 3 elements (id x y)) the first element is the depot
  customer.demand)      ; ordered list of demands (each demand has 2 elements (location-id demand-integer)) first element is the depot with demand value at zero

;; -----------------------------
;; SIMPLE FUNCTIONS
;; -----------------------------

;; -----------------------------
;; OPERATOR AND GOAL FUNCTION
;; -----------------------------

(defun gen-successors (state)
	"Generates the successor states of a given state"
	)

(defun is-goal-state (state)
	"Checks if a given state is the goal state"
	)

;; -----------------------------
;; HEURISTICS AND COST FUNCTIONS
;; -----------------------------

; TODO

;; -----------------------------
;; SOLVE FUNCTION
;; -----------------------------

(defun vrp (probleml search-strategy)
	"Solve instances of the Vehicle Routing Problem"
	)