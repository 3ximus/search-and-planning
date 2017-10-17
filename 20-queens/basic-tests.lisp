
(in-package :user)


(defun basic-checks-of-20-queens-result (res)
  (cond ((not (arrayp res)) (error "The result must be an array!"))
		((not (equalp '(20 20) (array-dimensions res))) (error "The array dimensions are wrong!"))
		(T "Passed basic tests.")))


#| (defun basic-checks-of-15-puzzle-result (res)
  (cond ((not (listp res)) (error "The result must be a list!"))
	((not (arrayp (first res))) (error "Each element of the result list must be an array!"))
	((not (equalp '(4 4) (array-dimensions (first res))))
	 (error "The array dimensions of the elements on the results list is wrong!"))
	((not (equalp (first (last res)) '#2A((1 2 3 4) (5 6 7 8) (9 10 11 12) (13 14 15 nil))))
	 (error "The final element of the result list does not represent a goal state!"))
	(T "Passed basic tests."))) |#
