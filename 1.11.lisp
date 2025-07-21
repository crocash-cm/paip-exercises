(use-package :iterate)

;; Exercise 1.1 [m] Define a version of last-name that handles "Rex Morgan MD," "Morton Downey, Jr.," and whatever other cases you can think of.

(defparameter *surtitles* '(ba bsc ma msc phd md))
(defparameter *names* '((John Morgan MD) (Lisa Chandling Phd) (Mike Brooks MA) (Jane Eyre) (Peter Snelding)))

(defun last-name (names)
  "Extract surname even in names with postnomial title."
  (let ((surnames '()))
    (iter (for name in names)
      (cond ((member (first (last name)) *surtitles*)
	     (setf surnames (append surnames (list (nth (- (length name) 2) name)))))
	    ('t (setf surnames (append surnames (last name))))))
    surnames))


;; Exercise 1.2 [m] Write a function to exponentiate, or raise a number to an integer power. For example: (power 3 2) = 32 = 9.

;;(defun power (n power)
;;  (expt n power))

(defun power (n power)
  (let ((product 0) (factor-list '()))
    (iter (for i from 1 to power)
      (setf factor-list (collect n)))
    (setf product (reduce #'* factor-list))
    product))

;; 1.3 Write a function that counts the number of atoms in an expression. (For my version of this exercise, () and nil do not count as atoms.)

(defun count-atoms (exp)
  (let ((count 0))
    (iter (for atom in exp)
      (unless (equal atom 'nil)
	(incf count)))
    count))

;; 1.4 Write a functiuon that counts the number of times an expression occurs anywhere within another expression, e.g. in multiply nested lists, (count-anywhere 'a '(a ((a) b) a)) => 3

(defparameter *expression* '(a ((a b) a)))

(defun count-anywhere (atom exp)
  "Searches for atom in exp (list of nexted lists) and returns count."
  (cond ((null exp) 0)
	((equalp atom (car exp)) (+ 1 (count-anywhere atom (cdr exp))))
	((symbolp (car exp)) (count-anywhere atom (cdr exp)))
	(t (+ (count-anywhere atom (car exp)) (count-anywhere atom (cdr exp))))))

;; Exercise 1.5: Write a function to compute the dot product of two sequences of numbers, represented as lists. The dot product is computed by multiplying corresponding elements and then adding up the resulting products. Example:

(defun dot-product (list-x list-y)
  (reduce #'+ (mapcar #'* list-x list-y)))
