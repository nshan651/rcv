;;;; Playgroun for just messing around with CL

;; Sum a list
(defun sumlist (lst)
  (reduce #'+ lst))

(sumlist '(1 2 3))

;; Find the average of a list of numbers
(defun avg (lst)
  (/ (reduce #'+ lst) (length lst)))

(avg '(1 2 3 4))

;;; Monte Carlo estimation of pi
  ;(format t "rand-x: ~a, rand-y: ~a~%" rand-x rand-y)

;; Random float between -1.0 and 1.0
(- 1.0 (random 2.0))

;; My attempt
(defun monte-carlo (interval)
  (loop :for i :from 0 :below (* interval interval)
	:do
	   ;; Randomly generate normalized x and y values
	   (let ((xi (- 1.0 (random 2.0)))
		 (yi (- 1.0 (random 2.0)))
		 ;(origin (+ (* xi xi) (* yi yi)))
		 )
	   (format t "i: ~a, xi: ~a, yi: ~a~%" i xi yi))
   ))
(monte-carlo 2)

(dotimes (i 5 i))

(defun estimate-pi (num-points)
  "Estimate the value of pi through repeated random sampling."
  (let ((inside-count 0))
    (dotimes (_ num-points)
      (let ((x (- (random 1.0) 0.5))
            (y (- (random 1.0) 0.5)))
      ;(let ((x (- 1.0 (random 2.0)))
      ;      (y (- 1.0 (random 2.0))))
        (when (<= (+ (* x x) (* y y)) 0.25)
          (incf inside-count))))
    (* 4.0 (/ inside-count num-points))))

(estimate-pi 1000000)
