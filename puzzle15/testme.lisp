(load "puzzle-15")

;;; FUNCOES DE TESTE!!!!!


(defun basic-checks-of-15-puzzle-result (res)
  (cond ((not (listp res)) (error "The result must be a list!"))
	((not (arrayp (first res))) (error "Each element of the result list must be an array!"))
	((not (equalp '(4 4) (array-dimensions (first res))))
	 (error "The array dimensions of the elements on the results list is wrong!"))
	((not (equalp (first (last res)) '#2A((1 2 3 4) (5 6 7 8) (9 10 11 12) (13 14 15 nil))))
	 (error "The final element of the result list does not represent a goal state!"))
	(T "Passed basic tests.")))


(basic-checks-of-15-puzzle-result (solve-problem (make-array '(4 4) :initial-contents '((1 2 3 4) (5 6 7 8) (13 9 10 11) (14 nil 15 12))) "a*"))
