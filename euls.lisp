;; n times
(defun sum-mod-3-5 (n)
  (loop for i below n do
       (cond ((= 0 (mod i 3)) (setf result (+ result i)))
             ((= 0 (mod i 5)) (setf result (+ result i)))
             (t nil))))

;; up to n
(defun sum-even-fib (n)
  (let ((tmp 0)
        (pcr 1)
        (ctr 2)
        (total 0))
    (loop do
          (when (evenp ctr)
            (setf total (+ total ctr)))
          (setf tmp ctr)
          (setf ctr (+ ctr pcr))
          (setf pcr tmp)
          while (>= n ctr))
    (print total)))

;; for small primes (<10m)
(defun eratosthenes (n)
  (let ((il (make-array (+ 1 n) :element-type 'integer :initial-element 1)))
    (setf (aref il 0) 0)
    (setf (aref il 1) 0)
    (loop for p from 2 to n while (<= (* p p) n) do
          (loop for j from (* p p) to n by p do
              (setf (aref il j) 0)))
    il))

;; interprets the array output of the sieve
(defun parse-sieve (arr)
  (loop for i from 0 below (length arr)
     when (/= 0  (* i (aref arr i)))
     collect (* i (aref arr i))))
