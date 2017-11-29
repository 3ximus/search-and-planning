
;; Retorna routes para cada veiculo
(defun generalized-assignment (vrp-prob)
	(let* ((customers (rest  (vrp-customer.locations vrp-prob)))
		   (depot     (first (vrp-customer.locations vrp-prob)))
		   (seeds  	  (select-customer-seeds vrp-prob))
		   (routes 	  (let ((lst NIL))
						(dotimes (i (vrp-vehicles.number vrp-prob) lst)
							(setf lst (cons (list 0) lst))))))
		
		(format T "~%~% seeds: ~D~%~%" seeds)
		(break)

		(dolist (customer customers routes)
			(push (first customer)  (nth (generalized-assignment-customer seeds (rest customer) (rest depot)) routes)))
		(dolist (route routes routes)
			(push 0 route)))
		
		(format T "~%~% general assignment result: ~%")
		(dolist (route routes)
			(format T "~D~%" route))
	)

;; Retorna seeds
(defun select-customer-seeds (vrp-prob)
	(let  ((seeds NIL)

		   (rem-capacity (let ((lst NIL))
							(dotimes (i (vrp-vehicles.number vrp-prob) lst)
								(setf lst (cons (vrp-vehicle.capacity vrp-prob) lst)))))
		   (points_quad (let ((lst NIL))
							(dotimes (i (vrp-vehicles.number vrp-prob) lst)
								(setf lst (cons (list) lst)))))
		   (customers 		 (rest  (vrp-customer.locations vrp-prob)))
		   (demands          (rest  (vrp-customer.demand vrp-prob)))
		   (nr-vehicles      (vrp-vehicles.number vrp-prob))
		   (vehicle-capacity (vrp-vehicle.capacity vrp-prob))
		   (fraction 0)
		   (aux_sum  0)
		   (counter  0))
	
	(setf fraction (/ (apply '+ (mapcar #'second demands)) (* nr-vehicles vehicle-capacity)))

	(dolist (demand demands)
		(if (> (+ aux_sum (second demand)) (* fraction vehicle-capacity))
			(progn
				(setf (nth counter rem-capacity) (- (nth counter rem-capacity) aux_sum))
				(setf aux_sum (second demand))
				(setf counter (get-max-rem-capacity rem-capacity vehicle-capacity))
			)
			(setf aux_sum (+ aux_sum (second demand))))
		(setf (nth counter points_quad) (cons (rest (nth (- (first demand) 1) customers)) (nth counter points_quad))))

	(let ((seed_x 0)
		  (seed_y 0))
	(dolist (points points_quad seeds)
		(setf seed_x (avg (mapcar #'car points)))
		(setf seed_y (avg (mapcar #'second points)))
		(setf seeds (cons (list seed_x seed_y) seeds))))))

;; Retorna id do veiculo ao qual foi assigned o customer
(defun generalized-assignment-customer (seeds customer depot)
	(let ((min_dik 99999)
		  (tmp 0)
		  (counter 0)
		  (vehicle-id  0))

	(dolist (seed seeds vehicle-id)
		(setf tmp (generalized-assignment-heuristic seed customer depot))
		(if (< tmp min_dik)
			(progn
				(setf min_dik tmp)
				(setf vehicle-id counter)))
		(incf counter))))

;; Retorna o valor dik de um customer para um determinado seed
(defun generalized-assignment-heuristic (seed customer depot)
	(let ((d1 0)
		  (d2 0))
	(setf d1 (+ (distance depot customer) (distance customer seed) (distance seed depot)))
	(setf d2 (+ (distance depot seed)	  (distance seed customer) (distance customer depot)))
	(- (min d1 d2) (+ (distance depot seed) (distance seed depot)))))

(defun get-max-rem-capacity (rem-capacity vehicle-capacity)
	(let ((counter 0)
		  (return_counter 0)
		  (max_capacity 0))
		(dolist (capacity rem-capacity return_counter)
			(if (= capacity vehicle-capacity) (return-from get-max-rem-capacity counter))
			(if (< max_capacity capacity) (progn (setf max_capacity capacity) (setf return_counter counter)))
			(incf counter))))