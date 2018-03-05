;;;; Some of this is useful for Project Euler
;;;; some of it is not at all

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

(defun primes-below (n)
  (parse-sieve (eratosthenes n)))

;; floyd cycle detection in output of f
;; maybe give comparison func as param
(defun floyd-cycle (f x0)
  (let ((lam 1)
        (mu 0)
        (tortoise (funcall f x0))
        (hare (funcall f (funcall f x0))))
    ;; main phase: find repetition x_i = x_2i
    ;; eventually they will both be in the cycle,
    ;; and their distance (nu) divisible by the period lam
    (loop while (not (eql tortoise hare)) do
         (setf tortoise (funcall f tortoise))
         (setf hare (funcall f (funcall f hare))))
    ;; tortoise position = distance between hare and tortoise
    ;; and divisible by the period lam, distance between is
    ;; now constant at 2nu, a multiple of lam
    ;; when their values meet tortoise will be at cycle start mu
    (setf tortoise x0)
    (loop while (not (eql tortoise hare)) do
         (setf tortoise (funcall f tortoise))
         (setf hare (funcall f hare))
         (setf mu (1+ mu)))
    ;; find length of shortest cycle starting from x_mu
    (setf hare (funcall f tortoise))
    (loop while (not (eql tortoise hare)) do
         (setf hare (funcall f hare))
         (setf lam (1+ lam)))
    (values lam mu)))

;; setf *print-circle* t to prevent hang
(defun make-circle (f lam &optional (mu 0))
  (let ((lo (loop for i from mu below (+ lam mu) collect (funcall f i))))
    (setf (cdr (last lo)) lo)))

(defun make-rho (f lam mu &optional (x0 0))
  "Append a circular list to a list resulting in a 'rho' shaped directive graph. lam: cycle period, mu: cycle start"
  (let ((li (loop for i from x0 below mu collect (funcall f i))))
    (setf (cdr (last li)) (make-circle f lam mu))
    li))

(defun walk-into (seq ctr)
  (let ((ptr seq))
    (loop for i from 1 to ctr do
         (setf ptr (cdr ptr)))
    ptr))

(defun seq-walker (seq start)
  (let ((ctr start))
    (lambda ()
      (setf ctr (1+ ctr))
      (car (walk-into seq (1- ctr))))))

(defun seq-rewalker (seq)
  (lambda (x)
    (car (walk-into seq x))))

(defun pollards-rho (n)
  (let ((x 2) (y 2) (d 1))
    (flet ((g (x) (mod (+ (* x x) 1)
                       n)))
      (loop while (= d 1) do
           (setf x (g x))
           (setf y (g (g y)))
           (setf d (gcd (abs (- x y)) n)))
      (if (/= d n)
          d
          nil))))

;; What is the largest prime factor of the number 600851475143 ?

