
; ***************************** ;
;		Iterative sampling	    ;
; ***************************** ;

(defun select-random-state (successor-states-lst)
	(let  ((nr-states (length successor-states-lst)))
		(nth (random nr-states) successor-states-lst)))

(defun iterative-sampling (init-state &key gen-successors (max-sol-number 0))
	(let ((successor-states-lst NIL)
		  (curr-state init-state)
		  (next-state NIL)
		  (*iter-samp-data* (list)) ; lista com solucoes partiais obtidas
		  (i 0))
		(loop while (not (is-goal-state curr-state))
			do
			(setf successor-states-lst (gen-successors curr-state))
			(if (null successor-states-lst)
				(progn
					(if (not (= max-sol-number 0))
						(progn
							(setf *iter-samp-data* (storeResult *iter-samp-data* curr-state)) ;; guarda solucao parcial
							(incf i)
							(if (>= i max-sol-number) (return)))) ;; exit loop
					(setf next-state init-state) 	;; restart search
				)
				(setf next-state (select-random-state successor-states-lst)))
			(setf curr-state next-state)
			; (break) ;debug
			; (format T "~S~%" curr-state) ; debug
		)
		
		(if (is-goal-state curr-state)
			(state-vehicle-routes curr-state)
			(if (not (= max-sol-number 0)) (print-stats-iter-samp *iter-samp-data*)))
	))

; ****************************************** ;
;		Data processing aux functions		 ;
; ****************************************** ;

(defun array-to-list (array)
  (let* ((dimensions (array-dimensions array))
         (depth      (1- (length dimensions)))
         (indices    (make-list (1+ depth) :initial-element 0)))
    (labels ((recurse (n)
               (loop for j below (nth n dimensions)
                     do (setf (nth n indices) j)
                     collect (if (= n depth)
                                 (apply #'aref array indices)
                               (recurse (1+ n))))))
      (recurse 0))))

(defun avg(data-lst)
	(/ (apply '+ data-lst) (length data-lst)))

(defun storeResult (stats-lst curr-state)
	(cons (list (state-number-unvisited-locations curr-state) 					; number of unvisited cities
				(avg (array-to-list (state-remaining-tour-length curr-state))) 	; array with remaining tour length for each vehicle
				(avg (array-to-list (state-remaining-capacity    curr-state))))	; array with remaining cargo for each vehicle
				stats-lst))

(defun print-stats-iter-samp (data-lst)
	(format T "~%-------------------- Iterative sampling stats --------------------~%")
	(format T "Avg number of unvisited locations: ~D~%" (avg (mapcar #'car 	  data-lst)))
	(format T "Avg remaining tour length: ~D~%" 		(avg (mapcar #'second data-lst)))
	(format T "Avg remaining capacity: ~D~%" 			(avg (mapcar #'third  data-lst)))
	(format T "Total number iterations: ~D~%"			(length data-lst)))
		