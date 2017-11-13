
;; Randomizer function
(defun select-random-state (successor-states-lst)
	(let  ((nr-states (length successor-states-lst)))
		(nth (random nr-states) successor-states-lst)))

(defun iterative-sampling (init-state)
	(let ((successor-states-lst NIL)
		  (curr-state init-state)
		  (next-state NIL))
		(loop while (not (is-goal-state curr-state))
			do
			(setf successor-states-lst (gen-successors curr-state))
			(if (null successor-states-lst)
				(setf next-state init-state) ;; restart search
				(setf next-state (select-random-state successor-states-lst)))
			(setf curr-state next-state)
			(format T "~S~%" curr-state) ; debug
		)
		(state-vehicle-routes curr-state)))
		