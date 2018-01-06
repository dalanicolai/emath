(require 'ert)

(defun derivative (v e)
  (pcase e
    ((pred symbolp) (if (eq e v) 1 e))
    ((pred numberp) 0)
    (`(+) 0)
    (`(+ ,x . ,ys) (expr+ (derivative v x)
                          (derivative v (apply #'expr+ ys))))
    (`(*) 0)
    (`(* ,x . ,ys) (derivative2* v x (apply #'expr* ys)))
    (`(/ ,x ,y) (derivative2* v x (expr** y -1)))
    (`(** ,x ,(and (pred numberp) y))
     (if (equal y 0) 0
       (expr* y
              (expr** x (- y 1))
              (derivative v x))))
    (`(ln ,x) (expr/ (derivative v x) x))
    (`(sin ,x) (expr* `(cos ,x) (derivative v x)))
    (`(cos ,x) (expr* -1 `(sin ,x) (derivative v x)))
    (_ (error "Don't know how to differentiate %S" e))))

(ert-deftest math-derivative ()
  (should (equal (derivative 'x 'x)                   1))
  (should (equal (derivative 'x 7)                    0))
  (should (equal (derivative 'x '(+ x 7))             1))
  (should (equal (derivative 'x '(* x 7))             7))
  (should (equal (derivative 'x '(* 3 (** x 2)))      '(* 6 x)))
  (should (equal (derivative 'x '(ln x))              '(/ 1 x)))
  (should (equal (derivative 'x '(ln (* 2 x)))        '(/ 1 x)))
  (should (equal (derivative 'x '(ln (** x 2)))       '(/ 2 x)))
  (should (equal (derivative 'x '(ln 1))              0))
  (should (equal (derivative 'x '(sin x))             '(cos x)))
  (should (equal (derivative 'x '(sin (** x 2)))      '(* 2 (cos (** x 2)) x)))
  (should (equal (derivative 'x '(** (sin x) 3))      '(* 3 (** (sin x) 2) (cos x))))
)

(defun derivative2* (v a b)
  (expr+ (expr* (derivative v a) b)
         (expr* a (derivative v b))))

(ert-deftest math-derivative2* ()
  (should (equal (derivative2* 'x 'x 7) 7))
  (should (equal (derivative2* 'x 7 'x) 7)))


;;; sot - a restricted sum-of-terms representation of expressions.
;;;
;;; A `sot` is a list `(num denom map)` where num and denom are numeric
;;; constants and `map` is an alist mapping `term`s onto numeric coefficients,
;;; representing the sum of (/ num denom) and the weighted sum of the map. Each
;;; term appears exactly once in the map. Denom is always positive. If num and
;;; denom are exact integers, they are relatively prime.
;;;
;;; A `term` is a list `(num denom map)` where num and denom are numeric
;;; constants and `map` is a non-empty map from `factor`s onto `exponent`s. A
;;; `term` represents the product of (/ num denom) and the factors raised to
;;; their exponents. Each factor appears exactly once in the map. Denom is
;;; always positive. If num and denom are exact integers, they are relatively prime.
;;;
;;; A `factor` is a variable or a function application where the function is not
;;; `+`, `-`, `*`, `/`, or `**`. `+` and `-` should be distributed to other
;;; `terms`; `*` and `/` should be flattened out into the `term`; and `**` should be
;;; combined into the `factor`'s exponent.
;;;
;;; An `exponent` is another `sot`.
;;;
;;; The maps in the above descriptions are association lists.
;;;
;;; So, the following are valid sots:
;;;
;;; (42 1)                      ; the constant 42
;;; (0 1 (1 1 (x . 1)))         ; the variable x
;;; (1 3 (-2 7 (x . 2)))        ; (+ (* (/ 2 7) (expt x 2)) (/ 1 3))
;;; (0 1 (1 1 (x . 2) (y . -1)) ; (/ (expt x 2) y)

(defun sot (e)
  "Convert the expression 'e' to sot form."
  (pcase e
    ((and (pred numberp) e) `(,e 1))
    ((and (pred symbolp) e) `(0 1 (1 1 (,e 1))))
    (`(+) (sot 0))
    (`(+ ,e) (sot e))
    (`(+ . ,es) ;; convert addends, combine like terms

    

(defun expr2+ (a b)
  (cond
   ((and (numberp a) (numberp b)) (+ a b))
   ((numberp b) (expr2+ b a))
   ((eq a 0) b)
   ((app-p '+ b) (fold #'expr2+ a (cdr b)))
   ((app-p '+ a) `(+ ,@(cdr a) ,b))
   (t `(+ ,a ,b))))

(defun expr+ (&rest e)
  (pcase e
    (`() 0)
    (`(,x) x)
    (xs (fold #'expr2+ 0 xs))))

(ert-deftest math-expr+ ()
  (should (equal (expr+) 0))
  (should (equal (expr+ 'x) 'x))
  (should (equal (expr+ 1 2) 3))
  (should (equal (expr+ 'x 2) '(+ 2 x)))
  (should (equal (expr+ 2 'x) '(+ 2 x)))
  (should (equal (expr+ 'x 'y) '(+ x y)))
  (should (equal (expr+ 'x 'y 'z) '(+ x y z)))
  (should (equal (expr+ 0 'x) 'x))
  (should (equal (expr+ 'x '0) 'x))
  (should (equal (expr+ '(+ 1 x) '(+ x 2)) '(+ 3 x x)))
  (should (equal (expr+ 0 '(+ 1 x)) '(+ 1 x))))

(defun expr2* (a b)
  (cond
   ((and (numberp a) (numberp b)) (* a b))
   ((numberp b) (expr2* b a))
   ((eq a 0) 0)
   ((eq a 1) b)
   ((app-p '* b) (fold #'expr2* a (cdr b)))
   ((app-p '* a) `(* ,@(cdr a) ,b))
   (t `(* ,a ,b))))

(defun insert-power (powers base exp)
  (let ((pair (assoc base powers)))
    (if (not pair)
        (setq powers (append powers `((,base . ,exp))))
      (let ((new (expr+ exp (cdr pair))))
        (if (equal new 0)
            (setq powers (delq pair powers))
          (setf (cdr pair) new)))))
  powers)

(ert-deftest math-insert-power ()
  (should (equal (insert-power '() 'x 1) '((x . 1))))
  (should (equal (insert-power '((x . 1)) 'x 1) '((x . 2))))
  (should (equal (insert-power '((x . 2)) 'y 1) '((x . 2) (y . 1))))
  (should (equal (insert-power '((x . 2) (y . 1)) 'x -2) '((y . 1))))
  (should (equal (insert-power '((x . 2) (y . 1)) 'y -2) '((x . 2) (y . -1))))
)

(defun flatten*/ (e)
  "Flatten a tree of multiplications and divisions into a single product.
Return a simple list of factors, without a surrounding (* ...).
ex: (* (* x y) z (/ u (/ v w))) => (x y z u (** v -1) w)"
  (pcase e
    (`(* . ,xs) (mapcan #'flatten*/ xs))
    (`(/ ,x ,y1 . ,ys) (let ((divisors (flatten*/ `(* ,y1 ,@ys))))
                         `(,x ,@(mapcar (lambda (x) (expr** x -1)) divisors))))
    (x `(,x))))

(ert-deftest math-flatten*/ ()
  (should (equal (flatten*/ '(*)) ()))
  (should (equal (flatten*/ '(* x y)) '(x y)))
  (should (equal (flatten*/ '(/ v w)) '(v (** w -1))))
  (should (equal (flatten*/ '(* (* x y) z (/ u (/ v w)))) '(x y z u (** v -1) w)))
)


(defun expr* (&rest e)
  (let ((xs (mapcan #'flatten*/ e)))

    ;; Produce an alist mapping each factor to the sum of the exponents applied
    ;; to it. Drop exponents that sum to zero.
    ;; (x y (** x 2) 3 (** 4 0.5) (** 4 1.5))
    ;; => ((x . 3) (y . 1) (3 . 1) (4 . 2))
    (let (powers)
      (dolist (x xs)
        (pcase x
          (`(** ,base ,exp) (setq powers (insert-power powers base exp)))
          (x                (setq powers (insert-power powers x 1)))))

      ;; Apply numeric exponents to numeric bases. Accumulate factors with
      ;; negative exponents as a separate divisor, to avoid doing a reciprocal
      ;; if we don't need to: (* (** 9 4) (** 3 -10)) should be an exact (/ 1
      ;; 9), not 0.11111..., so we shouldn't compute (expt 3 -10) as an
      ;; intermediate result.
      (let ((numerator 1) (divisor 1))
        (setq powers
              (mapcan (lambda (p)
                        (let ((base (car p)) (exp (cdr p)))
                          (if (and (numberp base) (numberp exp))
                              (progn
                                (if (>= exp 0)
                                    (setq numerator (* numerator (expt base exp)))
                                  (setq divisor (* divisor (expt base (- exp)))))
                                ;; drop this element; yay mapcan
                                ())
                            (list p))))
                      powers))
        (if (zerop numerator) 0
          ;; Normalize the sign: make sure the divisor is positive.
          (if (< divisor 0)
              (setq numerator (- numerator) divisor (- divisor)))

          ;; If the numerator and divisor are integers, leave them as a fraction
          ;; in lowest terms.
          (if (and (integerp numerator) (integerp divisor))
              (let ((d (gcd numerator divisor)))
                (setq numerator (/ numerator d) divisor (/ divisor d)))
            ;; They're not all integers, so go ahead and do it numerically.
            (setq numerator (/ numerator divisor) divisor 1))

          ;; Split the powers into those with positive exponents and those with
          ;; negative exponents, so we can produce a fraction if necessary.
          ;; Convert from (base . exponent) pairs to expressions.
          (let (positive negative)
            (dolist (pair (reverse powers))
              (let* ((base (car pair)) (exp (cdr pair)))
                (if (and (numberp exp) (< exp 0))
                    (push (expr** base (- exp)) negative)
                  (push (expr** base exp) positive))))

            ;; Combine the factor lists and the constants into an expression.
            (unless (= numerator 1)
              (push numerator positive))
            (unless (= divisor 1)
              (push divisor negative))
            (setq positive (pcase positive
                             (`() 1)
                             (`(,x) x)
                             (`(,x1 . ,xs) `(* ,x1 ,@xs))))
            (setq negative (pcase negative
                             (`() 1)
                             (`(,x) x)
                             (`(,x1 . ,xs) `(* ,x1 ,@xs))))
            (pcase `(,positive ,negative)
              (`(1 1) 1)
              (`(,n 1) n)
              (`(,n ,d) `(/ ,n ,d)))))))))

(ert-deftest math-expr* ()
  (should (equal (expr*) 1))
  (should (equal (expr* 'x) 'x))
  (should (equal (expr* 2 3) 6))
  (should (equal (expr* 2 '(** 3 2) '(** 3 -3)) '(/ 2 3)))
  (should (equal (expr* '(** 9 5) '(** 3 -10)) 1))
  (should (equal (expr* '(** 9 5) '(** 3 -11)) '(/ 1 3)))
  (should (equal (expr* '(** 2 0.25) '(** 2 0.75)) 2.0))
  (should (equal (expr* 0 'x) 0))
  (should (equal (expr* 1 'x) 'x))
  (should (equal (expr* 'x 1) 'x))
  (should (equal (expr* 'x 2) '(* 2 x)))
  (should (equal (expr* 2 'x) '(* 2 x)))
  (should (equal (expr* 'x 'y) '(* x y)))
  (should (equal (expr* 'x 5 'y 7) '(* 35 x y)))
  (should (equal (expr* '(* x 2) '(* y 3)) '(* 6 x y)))
  (should (equal (expr* '(* (** x 2) (** y 3)) '(* (** x 5) (** y 7)))
                 '(* (** x 7) (** y 10))))
  (should (equal (expr* '(* (** x 2) (** y 3)) '(* (** x -5) (** y -3)))
                 '(/ 1 (** x 3))))
  (should (equal (expr* 2 'x '(** x -2)) '(/ 2 x)))
  (should (equal (expr* '(** x -1)) '(/ 1 x)))
  (should (equal (expr* '(/ 1 2) '(** x -1)) '(/ 1 (* 2 x))))
)

(defun expr** (a b)
  (pcase `(,a ,b)
    (`(,(and (pred numberp) a) ,(and (pred natnump) b))
     (expt a b))
    (`(,(and (pred numberp) a) ,(and (pred integerp) b))
     `(** ,(expt a (- b)) -1))
    (`(,a 0) 1)
    (`(,a 1) a)
    (`(0 ,b) 0)
    (`(1 ,b) 1)
    (`((* . ,as) ,b) (apply #'expr* (mapcar (lambda (a) (expr** a b)) as)))
    (`((** ,a ,b) ,c) (expr** a (expr* b c)))
    (`(,a ,b) `(** ,a ,b))))

(ert-deftest math-expr** ()
  (should (equal (expr** 'x 0) 1))
  (should (equal (expr** 'x 1) 'x))
  (should (equal (expr** 0 'x) 0))
  (should (equal (expr** 1 'x) 1))
  (should (equal (expr** 2 3) 8))
  (should (equal (expr** 2 -1) '(** 2 -1)))
  (should (equal (expr** 2 -2) '(** 4 -1)))
  (should (equal (expr** 'x 'y) '(** x y)))
  (should (equal (expr** (expr** 'x 'y) 'z) '(** x (* y z))))
  (should (equal (expr** (expr** 'x 3) 5) '(** x 15)))
  (should (equal (expr** '(* x y z) 2) '(* (** x 2) (** y 2) (** z 2))))
  (should (equal (expr** '(** (f x) -1) -1) '(f x)))
)

(defun expr-ln (x)
  (pcase x
    (1 0)
    (`%e 1)
    (x `(ln ,x))))

(ert-deftest math-expr-ln ()
  (should (equal (expr-ln 1) 0))
  (should (equal (expr-ln '%e) 1))
  (should (equal (expr-ln 10) '(ln 10))))

(defun expr/ (a b) (expr* a (expr** b -1)))

(ert-deftest math-expr/ ()
  (should (equal (expr/ 0 'q) 0))
  (should (equal (expr/ 'z 1) 'z))
  (should (equal (expr/ 4 2) 2))
  (should (equal (expr/ 4 3) '(/ 4 3)))
  (should (equal (expr/ 2 '(* 2 x)) '(/ 1 x)))
)

(defun expr/* (as bs)
  (let ((ak 1) (bk 1))
    (if (numberp (car as))
        (setq ak (car as) as (cdr as)))
    (if (numberp (car bs))
        (setq bk (car bs) bs (cdr bs)))
    (if (zerop (mod ak bk))
        (setq ak (/ ak bk) bk 1))
    (let ((as2 (apply #'list as))
          (bs2 (apply #'list bs)))
      (dolist (a as)
        (if (member a bs2)
            (setq as2 (delete a as2)
                  bs2 (delete a bs2))))
      (setq as as2 bs bs2))
    (if (not (equal ak 1)) (push ak as))
    (if (not (equal bk 1)) (push bk bs))
    (cond
     ((null bs) (apply #'expr* as))
     ((null as) `(/ 1 ,(apply #'expr* bs)))
     (t `(/ ,(apply #'expr* as) ,(apply #'expr* bs))))))

(ert-deftest math-expr/* ()
  (should (equal (expr/* '(1) '(2)) '(/ 1 2)))
  (should (equal (expr/* '(6) '(2)) '3))
  (should (equal (expr/* '(1) '(x)) '(/ 1 x)))
  (should (equal (expr/* '(x) '(1)) 'x))
  (should (equal (expr/* '(x) '(x)) 1))
  (should (equal (expr/* '(10 x) '(5 x)) 2))
  (should (equal (expr/* '(10 x y) '(5 z x)) '(/ (* 2 y) z)))
  (should (equal (expr/* '(2) '(2 x)) '(/ 1 x)))
)


;;; primitives

(defun fold (fn initial list)
  (while list
    (setq initial (funcall fn initial (car list))
          list (cdr list)))
  initial)

(defun app-p (op e)
  (and (consp e) (eq (car e) op)))
