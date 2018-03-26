;;;; Some of this is useful for Project Euler
;;;; some of it is not at all

;;; General Notes
;;; The expression (values) returns zero values. This is the standard idiom for returning no values from a function.

;; n times
(defun sum-mod-3-5 (n)
  (let ((result 0))
    (loop for i below n do
         (cond ((= 0 (mod i 3)) (setf result (+ result i)))
               ((= 0 (mod i 5)) (setf result (+ result i)))))
    result))

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

(defun is-prime (n &optional list)
  (not (null (member n (if (not (null list))
                           list
                           (parse-sieve (eratosthenes (1+ n))))))))

;; first exp after the 'or' is to account for negatives
(defun num-to-list (n)
  (map 'list (lambda (c) (or (digit-char-p c) '-)) (prin1-to-string n))) 

(defun list-to-num (li)
  (let ((result ""))
    (loop for i from 0 below (length li) do
         (setf result (concatenate 'string result (prin1-to-string (nth i li)))))
    (values (parse-integer result))))

;; don't call length on one of these ;-)
(defun num-loop (nli)
  "Returns a circular version of a list."
  (let ((new-nli nli))
    (setf (cdr (last new-nli)) new-nli)
    new-nli))

(defun rotate-number (n)
  "Returns the number which is n's digits rotated one place to the left. Eg. 719 => 197"
  (let* ((nli (num-to-list n))
         (len (length nli))
         (nlo (num-loop nli)))
    (list-to-num (loop for i from 0 below len collect (nth (1+ i) nlo)))))

(defun is-circular-prime (n &optional list)
  (let ((rn n))
    (loop for i from 1 to (length (num-to-list n)) do
         (if (not (is-prime rn list))
             (return-from is-circular-prime nil)
             (setf rn (rotate-number rn))))
    t))

(defun circular-primes-below (n)
  (let ((primes (primes-below n)))
    (loop for x in primes when (is-circular-prime x primes) collect x)))

(defun car-times (count object)
  (loop for i from 1 to count do (car object)))

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

;; maybe could implement as wrapper for a func to fit into floyd-cycle
;; add an optional start position parameter?
(defun floyd-seq (seq)
  "Floyd cycle detect on a sequence."
  (let* ((lam 1)
         (mu 0)
         (tortoise (cdr seq))
         (hare (cdr tortoise)))
    (loop while (not (eql (car tortoise) (car hare))) do
         (setf tortoise (cdr tortoise))
         (setf hare (cdr (cdr hare))))
    (setf tortoise seq)
    (loop while (not (eql (car tortoise) (car hare))) do
         (setf tortoise (cdr tortoise))
         (setf hare (cdr hare))
         (setf mu (1+ mu)))
    (setf hare (cdr tortoise))
    (loop while (not (eql (car tortoise) (car hare))) do
         (setf hare (cdr hare))
         (setf lam (1+ lam)))
    (values lam mu)))

;; need to continue detecting cycle until the longest cycle length
(defun floyd-seq-longest () nil)

;; don't call subseq on circular seq
(defun cycle-seq-length (seq &optional lam mu)
  "Returns the length of the cycling portion of a sequence's values."
  (when (cyclic-p seq)
    (error "~a is a circular sequence." seq))
  (when (or (null lam) (null mu))
    (multiple-value-setq (lam mu) (floyd-seq seq)))
  (let* ((inseq (subseq seq mu))
         (cycseq (subseq inseq lam)))
    (make-loop cycseq)
    (do ((i 0 (incf i)))
        ((or (null inseq)
             (/= (pop cycseq) (pop inseq)))
         i))))

;; "naive" cycle detection, space complexity proportional to lam + mu
;; detects cycle by reference not value
;; this below from Let Over Lambda
(defun cyclic-p (l)
  (cyclic-p-aux l (make-hash-table)))

(defun cyclic-p-aux (l seen)
  (if (consp l)
    (or (gethash l seen)
        (progn
          (setf (gethash l seen) t)
          (or (cyclic-p-aux (car l) seen)
              (cyclic-p-aux (cdr l) seen))))))

;; setf *print-circle* t to prevent hang
;; don't call length, subseq, or try to push to it
(defun make-loop (seq)
  (setf (cdr (last seq)) seq))

(defun make-circle (f lam &optional (mu 0))
  (let ((lo (loop for i from mu below (+ lam mu) collect (funcall f i))))
    (setf (cdr (last lo)) lo)))

(defun make-rho (f lam mu &optional (x0 0))
  "Append a circular list to a list resulting in a 'rho' shaped directive graph. 
   lam: cycle period, mu: cycle start"
  (let ((li (loop for i from x0 below mu collect (funcall f i))))
    (setf (cdr (last li)) (make-circle f lam mu))
    li))

(defun straighten-rho (rho end)
  "Returns a sequence made from an imprint of a rho sequence up to end index."
  (let ((inrho rho)
        (straight nil))
    (dotimes (i end)
      (push (pop inrho) straight))
    (reverse straight)))

(defun rho-func (rho)
  (lambda (x)
    (nth x rho)))

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

;; double precision floating point holds up to 10^308
(defun grab-digits (r d)
  "Grab a list of 'd' digits from rational number 'r'."
  (num-to-list (truncate (coerce (* r (expt 10 d)) 'long-float))))

;; truncate returns both sides of the decimal point
#+nil
(defun digit-grabber (r)
  "Makes a digit streaming function for rational number 'r'."
  (let ((internal-r r))
    (lambda ()
      (multiple-value-bind (d new-f) (truncate (* internal-r 10))
        (setf internal-r new-f)
        d))))

;; scavenged divide function
(defun divide (a b &key (precision 8))
  (let ((frac 0))
    (multiple-value-bind (whole remainder)
        (floor a b)
      (unless (zerop remainder)
        (dotimes (i precision)
          (setf remainder (* remainder 10))
          (multiple-value-bind (quot rem)
              (floor remainder b)
            (setf frac (+ (* frac 10) quot))
            (when (zerop rem) (return))
            (setf remainder rem))))
      (values whole frac))))

(defun get-frac (a b d)
  (nth-value 1 (divide a b :precision d)))

;; 983
(defun reciprocal-cycles (&key (precision 500))
  "Crunch up primes to 1/d below d=1000 and check for largest cycle in decimal digits."
  (let ((length 0)
        (longest-d 0))
    (loop for x in (primes-below 1000)
          for cyc = (floyd-seq (num-to-list (get-frac 1 x precision))) do
         (when (> cyc length)
           (progn (setf length cyc)
                  (setf longest-d x))))
    (values longest-d length)))

(defun palindromep (num)
  "Is num palindromic? eg. 9009, 99, 8118"
  (let* ((numli (num-to-list num))
         (off (if (evenp (length numli))
                  0
                  1))
         (mid (-  (/ (length numli) 2) off))
         (sub1 (subseq numli 0 mid))
         (sub2 (subseq numli mid)))
    (if (equal sub1 (reverse sub2))
        t
        nil)))
