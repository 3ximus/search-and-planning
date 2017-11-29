(defun sweep (vrp-prob)
	let* (
		  ; Constantes
		  (customers (rest  (vrp-customer.locations vrp-prob)))
		  (demands   (rest  (vrp-customer.demand vrp-prob)))
		  (depot     (first (vrp-customer.locations vrp-prob)))
		  (customer_ids_sorted_by_angle (calculate_angle_to_depot depot customers))

		  ; Constantes Veiculo
		  (vehicle-max-capacity (vrp-vehicle.capacity vrp-prob))
		  (vehicle-max-distance (vrp-max.tour.length vrp-prob))
		  
		  ; Variaveis
		  (demand NIL)
		  (demand_capacity 0)
		  ;(distance_traveled 0)
		  
		  ;Varivaveis referentes ao veiculo
		  (vehicle  0)
		  (rem-capacity (create-list (vrp-vehicles.number vrp-prob) vehicle-max-capacity))
		  (rem-distance (create-list (vrp-vehicles.number vrp-prob) vehicle-max-distance))

		  ;Valor de Retorno
		  (routes (create-list (vrp-vehicles.number vrp-prob) (list 0)))


		(dolist (customer_id customer_ids_sorted_by_angle)

			; Valores de capacidade e distancia para o novo customer id
			(setf demand_capacity 	(get-demand-capacity demands customer_id))
			(setf distance_traveled (get-distance-traveled vehicle routes depot customer_id customers))
			
			; Verifica a Requisitos
			(if (or (verify_distance_constraint vehicle rem-distance distance_traveled customers depot customer_id)
					(verify_capacity_constraint vehicle rem-capacity demand-capacity))
				
				(setf vehicle (next-vehicle rem-capacity rem-distance))
				
				(progn
					(setf (nth vehicle rem-capacity) (- (nth vehicle rem-capacity) demand_capacity))
					(setf (nth vehicle rem-distance) (- (nth vehicle rem-distance) distance_traveled))
				)
			)	
			(setf (nth vehicle routes) (cons customer_id (nth vehicle routes))) ;Adiciona veiculo a um cluster
		) 

		
		(dolist (route routes routes)
			(push 0 route))
		
		(format T "~%~% sweep result: ~%")
		(dolist (route routes)
			(format T "~D~%" route))
	))

; Retorna lista de pontos ordenada por angulos 
(defun calculate_angles_to_depot (depot customers)
	(let ((angles (create-list (length customers) NIL))
		  (angle 0)
		  (customer_id 0))
		(dolist (customer customers angles)
			(setf angle (get-angle depot customer))
			(setf customer_id (1- (first customer)))
			(setf (nth customer_id angles) (cons (append customer (list angle)) angles)))
		(sort angles #'< :key #'fourth)
		(mapcar #'first angles) ; apendas retorna id's
	))

; Verifica requisitos de distancia 
(defun verify_distance_constraint (vehicle rem-distance distance_traveled customers depot customer_id)
	(let* ((depot_location 	  (rest depot)
		   (customer_location (rest (nth (1- customer_id) customers))))
		(< (-  (nth vehicle rem-distance) distance_traveled (distance customer_location depot_location)) 0))))

; Verifica requisitos de capacidade
(defun verify_capacity_constraint (vehicle rem-capacity demand-capacity)
	(< (- (nth vehicle rem-capacity) demand_capacity)))

; Distancia do ultimo ponto ao novo ponto candidato
(defun get-distance-traveled (vehicle routes depot customer_id customers)
	(let* ((depot_location    (rest depot))
		   (last-location_id  (nth 0 (nth vehicle routes)))
		   (last-location     (if (= last-location_id 0) depot_location (rest (nth (1- last-location_id) customers))))
		   (customer_location (rest (nth (1- customer_id) customers))))
		(distance last-location customer_location)))

; Capacidade do pedido ao cliente
(defun get-demand-capacity (demands customer_id)
	(let ((demand (nth (1- customer_id) demands)))
		(second demand)))

; Escolhe o proximo veiculo com base na remaining distance e capacity
(defun next-vehicle (rem-capacity rem-distance)
	) ; TODO

(defun create-list (size initial-content)
	(let ((lst NIL))
		(dotimes (i size lst)
			(setf lst (cons initial-content lst)))))


