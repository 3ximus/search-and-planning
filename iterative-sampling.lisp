



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
