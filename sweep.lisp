(defun sweep (vrp-prob)
	(let* (
		  ; Constantes
		  (customers (rest  (vrp-customer.locations vrp-prob)))
		  (demands   (rest  (vrp-customer.demand vrp-prob)))
		  (depot     (first (vrp-customer.locations vrp-prob)))

		  ; Constantes Veiculo
		  (vehicle-max-capacity (vrp-vehicle.capacity vrp-prob))

		  ; Variaveis
		  (demand NIL)
		  (demand_capacity 0)

		  ;Varivaveis referentes ao veiculo
		  (vehicle  0)
		  (rem-capacity (create-list (vrp-vehicles.number vrp-prob) vehicle-max-capacity))

		  ;Valor de Retorno
		  (clusters (create-list (vrp-vehicles.number vrp-prob) (list))))

		(let ((dist (mapcar #'second demands)))
		(format T "~%total customer demand: ~D~%" (apply #'+ dist))
		(format T "total vehicle capacity: ~D~%" (* 160 5)))
		;(format T "total vehicle capacity after cluster: ~D~%" (- (* 160 5) 3 12 22 13 10)))
		;(break)

		(dolist (customer_id (calculate_angles_to_depot depot customers) clusters)

			; Valores de capacidade e distancia para o novo customer id
			(setf demand_capacity 	(get-demand-capacity demands customer_id))

			; debug
			; (format T "~%demand_capacity: ~D~%" demand_capacity)
			; (format T "id: ~D~%" customer_id)
			; (format T "vehicle: ~D~%" vehicle)
			; (format T "clusters: ~D~%" clusters)
			; (format T "remaining capacity: ~D~%" rem-capacity)

			; (break)

			; Verifica Requisito de capacidade
			(if (= vehicle 0)
				(if (verify_capacity_constraint vehicle rem-capacity demand_capacity)
					(progn
						(setf last-vehicle vehicle)
						(incf vehicle)
						(setf (nth vehicle rem-capacity) (- (nth vehicle rem-capacity) demand_capacity))
						(setf (nth vehicle clusters) (cons customer_id (nth vehicle clusters)))
					)
					(progn
						(setf (nth vehicle rem-capacity) (- (nth vehicle rem-capacity) demand_capacity))
						(setf (nth vehicle clusters) (cons customer_id (nth vehicle clusters))) ;Adiciona veiculo a um cluster
					))

			(if (not (verify_capacity_constraint last-vehicle rem-capacity demand_capacity))
				(progn
					(setf (nth last-vehicle rem-capacity) (- (nth last-vehicle rem-capacity) demand_capacity))
					(setf (nth last-vehicle clusters) (cons customer_id (nth last-vehicle clusters))) ;Adiciona veiculo a um cluster antigo
				)
				(if (verify_capacity_constraint vehicle rem-capacity demand_capacity)
					(progn
						(setf last-vehicle vehicle)
						(incf vehicle)
						(setf (nth vehicle rem-capacity) (- (nth vehicle rem-capacity) demand_capacity))
						(setf (nth vehicle clusters) (cons customer_id (nth vehicle clusters)))
					)
					(progn
						(setf (nth vehicle rem-capacity) (- (nth vehicle rem-capacity) demand_capacity))
						(setf (nth vehicle clusters) (cons customer_id (nth vehicle clusters))) ;Adiciona veiculo a um cluster
					)))
			)
		)

		(format T "alocated points: ~D~%" (apply #'+ (mapcar #'length clusters)))
		clusters

	))

; Retorna lista de pontos ordenada por angulos
(defun calculate_angles_to_depot (depot customers)
	(let* ((angles NIL)
		   (angle 0)
		   (depot_location (rest depot))
		   (customer_id 0)
		   (customer_location NIL)
		   (aux_point (list (+ (first depot_location) 10) (second depot_location))))


		(dolist (customer customers angles)
			(setf customer_location (rest customer))
			(setf angle     		(get-angle-sweep aux_point customer_location depot_location))
			(setf customer_id 		(1- (first customer)))
			(setf angles (cons (list (first customer) angle (second customer) (third customer)) angles))

			; debug
			; (format T "~%customer location: ~D~%" customer_location)
			; (format T "id: ~D~%" (first customer))
			; (format T "angles: ~D~%" angles)
			; (break)
		)

		(setf angles (sort (copy-list angles) #'< :key #'second))

		; debug
		(format T "~%")
		(dolist (ang angles)
			(format T "id: ~D angle: ~D -> (~D, ~D)~%" (first ang) (second ang) (third ang) (fourth ang)))
		(format T "tamanho total: ~D~%" (length angles))
		;(break)



		(mapcar #'first angles) ; apendas retorna id's
	))



(defun get-angle-sweep (a b depot_location)
	"Gets the angle at the depot between points a and b"
	; this function uses the law of cosines ->   cos(x) = ( r^2 + R^2 - d^2 ) / ( 2rR )
	;  where r and R are distance from (0,0) to a point, d is the distance between them and x is the angle at (0,0) between the 2 edges leading to both points
	(let ((p-distance (distance depot_location a))
		  (v-size (distance depot_location b))
		  (v-p-distance (distance a b)))

		   (adjust-angle (rtd (acos (/ (- (+ (* p-distance p-distance) (* v-size v-size)) (* v-p-distance v-p-distance)) (* 2 p-distance v-size))))
		   				  depot_location
		   				  b)
	))

(defun adjust-angle (angle depot_location point)
	(let ((point_y (second point))
		  (depot_y (second depot_location))
		  (my-angle angle))

	(if (< point_y (* 1 depot_y))
		(setf my-angle (- 360 angle)))

	my-angle))

(defun rtd (a)
	(/ (* a 180.0) pi))

; Verifica requisitos de distancia
(defun verify_distance_constraint (vehicle rem-distance distance_traveled customers depot customer_id)
	(let ((depot_location 	  (rest depot)
		  (customer_location (rest (nth (1- customer_id) customers))))
		(< (-  (nth vehicle rem-distance) distance_traveled (distance customer_location depot_location)) 0))))

; Verifica requisitos de capacidade
(defun verify_capacity_constraint (vehicle rem-capacity demand_capacity)
	(< (- (nth vehicle rem-capacity) demand_capacity) 0))

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
(defun next-vehicle (rem-capacity vehicle-capacity)
	(let ((vehicle 0)
		  (return_counter 0)
		  (max_capacity 0))
		(dolist (capacity rem-capacity return_counter)
			(if (= capacity vehicle-capacity) (return-from next-vehicle vehicle))
			(if (< max_capacity capacity) (progn (setf max_capacity capacity) (setf return_counter vehicle)))
			(incf vehicle))))

(defun create-list (size initial-content)
	(let ((lst NIL))
		(dotimes (i size lst)
			(setf lst (cons initial-content lst)))))


