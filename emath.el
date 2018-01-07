;;;; emath.el - symbolic algebra in Emacs Lisp
;;;; Jim Blandy <jimb@red-bean.com>

;;; A toy. This is probably not as good at what it does as Calc mode, since
;;; that's been around longer.

(require 'ert)
(require 'cl-extra)

(defun emath-derivative (v e)
  (pcase e
    ((pred symbolp) (if (eq e v) 1 e))
    ((pred numberp) 0)
    (`(+) 0)
    (`(+ ,x . ,ys) (expr+ (emath-derivative v x)
                          (emath-derivative v (apply #'expr+ ys))))
    (`(*) 0)
    (`(* ,x . ,ys) (derivative2* v x (apply #'expr* ys)))
    (`(/ ,x ,y) (derivative2* v x (expr** y -1)))
    (`(** ,x ,(and (pred numberp) y))
     (if (equal y 0) 0
       (expr* y
              (expr** x (- y 1))
              (emath-derivative v x))))
    (`(ln ,x) (expr/ (emath-derivative v x) x))
    (`(sin ,x) (expr* `(cos ,x) (emath-derivative v x)))
    (`(cos ,x) (expr* -1 `(sin ,x) (emath-derivative v x)))
    (_ (error "Don't know how to differentiate %S" e))))

(ert-deftest emath-derivative ()
  (should (equal (emath-derivative 'x 'x)                   1))
  (should (equal (emath-derivative 'x 7)                    0))
  (should (equal (emath-derivative 'x '(+ x 7))             1))
  (should (equal (emath-derivative 'x '(* x 7))             7))
  (should (equal (emath-derivative 'x '(* 3 (** x 2)))      '(* 6 x)))
  (should (equal (emath-derivative 'x '(ln x))              '(/ 1 x)))
  (should (equal (emath-derivative 'x '(ln (* 2 x)))        '(/ 1 x)))
  (should (equal (emath-derivative 'x '(ln (** x 2)))       '(/ 2 x)))
  (should (equal (emath-derivative 'x '(ln 1))              0))
  (should (equal (emath-derivative 'x '(sin x))             '(cos x)))
  (should (equal (emath-derivative 'x '(sin (** x 2)))      '(* 2 (cos (** x 2)) x)))
  (should (equal (emath-derivative 'x '(** (sin x) 3))      '(* 3 (** (sin x) 2) (cos x))))
)

(defun derivative2* (v a b)
  (expr+ (expr* (emath-derivative v a) b)
         (expr* a (emath-derivative v b))))

(ert-deftest emath-derivative2* ()
  (should (equal (derivative2* 'x 'x 7) 7))
  (should (equal (derivative2* 'x 7 'x) 7)))


;;; sot - a restricted sum-of-terms representation of expressions.
;;;
;;; A `sot` is a list `(k map)` where k is a `number` and `map` is an alist
;;; mapping `term`s onto `number` coefficients, representing the sum of k and
;;; the weighted sum of the map. Each term appears exactly once in the map.
;;;
;;; A `term` is a non-empty map from `factor`s onto `exponent`s. A `term`
;;; represents the product of the factors raised to their exponents. Each factor
;;; appears exactly once in the map. Denom is always positive. If num and denom
;;; are exact integers, they are relatively prime.
;;;
;;; A `factor` is a variable or a function application where the function is not
;;; `+`, `-`, `*`, `/`, or `**`. `+` and `-` should be distributed to other
;;; `terms`; `*` and `/` should be flattened out into the `term`; and `**` should be
;;; combined into the `factor`'s exponent.
;;;
;;; An `exponent` is another `sot`.
;;;
;;; A `number` is either an integer, a floating-point value, or a pair `(num .
;;; denom)`, where `num` and `denom` are integers with no common divisior and
;;; `denom` is a positive integer greater than 1, representing the ratio num /
;;; denom.
;;;
;;; The maps in the above descriptions are association lists.
;;;
;;; So, the following are valid sots:
;;;
;;; (42)                                ; the constant 42
;;; ((2 . 3))                           ; the constant 2/3
;;; (0 (((x . 1)) . 1))                 ; the variable x
;;; ((1 . 3) (((x . 2)) (-2 . 7)))      ; 1/3 - 2/7 x^2
;;; (0 (((x . 2) (y . -1)) . 1)         ; x^2/y

(defun sot (e)
  "Convert the expression 'e' to sot form."
  (pcase e
    ((and (pred numberp) e) `(,e 1))
    ((and (pred symbolp) e) `(0 1 (1 1 (,e 1))))
    (`(+) (sot 0))
    (`(+ ,e) (sot e))
    (`(+ . ,es)
     (let ((es (mapcar #'sot es)))
       (

(defun insert-term (map term coeff)
  (emath-insert map #'
    

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

(ert-deftest emath-expr+ ()
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
  (emath-insert powers #'expr+ #'zerop base exp))

(ert-deftest emath-insert-power ()
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

(ert-deftest emath-flatten*/ ()
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

(ert-deftest emath-expr* ()
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

(ert-deftest emath-expr** ()
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

(ert-deftest emath-expr-ln ()
  (should (equal (expr-ln 1) 0))
  (should (equal (expr-ln '%e) 1))
  (should (equal (expr-ln 10) '(ln 10))))

(defun expr/ (a b) (expr* a (expr** b -1)))

(ert-deftest emath-expr/ ()
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

(ert-deftest emath-expr/* ()
  (should (equal (expr/* '(1) '(2)) '(/ 1 2)))
  (should (equal (expr/* '(6) '(2)) '3))
  (should (equal (expr/* '(1) '(x)) '(/ 1 x)))
  (should (equal (expr/* '(x) '(1)) 'x))
  (should (equal (expr/* '(x) '(x)) 1))
  (should (equal (expr/* '(10 x) '(5 x)) 2))
  (should (equal (expr/* '(10 x y) '(5 z x)) '(/ (* 2 y) z)))
  (should (equal (expr/* '(2) '(2 x)) '(/ 1 x)))
)


;;; numbers

;;; An emath number is either an integer, a float, or a ratio. (num . denom),
;;; where both are integers, denom is greater than 1, and the two have no common
;;; divisors, representing the fraction num/denom.
;;;
;;; Floats are treated as inexact values, integers and ratios are exact, and
;;; inexactness is contagious.

(defconst emath-num-conversions
  '((integerp           floatp          float)
    (integerp           emath-ratio-p   emath-ratio-from-integer)
    (emath-ratio-p      floatp          emath-ratio-to-float))
  "A table mapping pairs of numeric type predicates to conversion functions.
This table drives the `emath-num-type-case` macro.
Each entry has the form (from to converter), where `converter' is
a function that converts values satisfying the predicate `from'
to values satisfying the predicate `to'.

All conversions from a type must be listed after all conversions to that type.

If you view the set of conversions listed as a relation between
the types mentioned, the set and relation must be an upper
semilattice: there must be no cycles, and every pair of types
must be convertible to some more general common type.")

(defun emath-types ()
  "Return a list of the types mentioned in `emath-num-conversions'.
The result is ordered from least to most general."
  (emath-topological-sort emath-num-conversions #'eq #'car #'cadr))



(defun emath-num-+ (a b)
  (emath-num-type-case (a b)
    (integerp (+ a b))
    (emath-ratio-p (emath-ratio-+ a b+)
    (floatp (+ a b)))))
        




(defun emath-float-make (n)
  "Given a float or some lower-ranked numeric type, return an approximately equivalent float."
  (if 
  (pcase n
    ((pred integerp) (float n))
    ((
  
(defun emath-num-+ (a b)
  (cond
   ((and (numberp a) (numberp b)) (+ a b))
   ((and (emath-ratio-p a) (emath-ratio-p b)) (emath-ratio-+ a b))
   ;; Now we know one is a ratio and one is a number.
   ((emath-ratio-p b) (emath-num-+ b a))
   ;; Now we know a is a ratio.
   ((floatp b) (+ (emath-ratio-to-float a) b))
   ;; Now we know b is an integer.
   (t (emath-ratio-+ a (emath-ratio-from-integer b)))))

(ert-deftest emath-num-+ ()
  (should (equal (emath-num-+ 2          3)         5))
  (should (equal (emath-num-+ 2.0        3)       5.0))
  (should (equal (emath-num-+ '(2 . 3)   3) '(11 . 3)))
  (should (equal (emath-num-+ 2        3.0)       5.0))
  (should (equal (emath-num-+ 2.0      3.0)       5.0))
  (should (equal (emath-num-+ '(2 . 3) 3.0)       5.0))



;;; ratios

;;; An emath ratio is a pair of two integers (num . denom) where denom is
;;; greater than 1, and the two have no common divisors.i It represents the
;;; fraction num/denom.

(defun emath-ratio-p (x)
  (and (consp x)
       (integerp (car x))
       (integerp (cdr x))
       (> (cdr x) 1)))

(defun emath-ratio-from-integer (n) (cons n 1))
(defun emath-ratio-to-float (r) (/ (float (car r)) (cdr r)))

(defun emath-ratio-+ (a b)
  ;; Dividing out the lcm isn't really necessary: since we must compute and
  ;; factor out the gcd after the addition anyway, simply multiplying each
  ;; numerator by the other fraction's denominator would work too. But doing so
  ;; keeps the factors by which we scale the numerators smaller, so there's less
  ;; chance of overflow.
  (let* ((lcm (cl-lcm (cdr a) (cdr b)))
         (num (+ (* (car a) (/ lcm (cdr a)))
                 (* (car b) (/ lcm (cdr b)))))
         (gcd (gcd num lcm)))
    (cons (/ num gcd) (/ lcm gcd))))

(ert-deftest emath-ratio-+ ()
  (should (equal (emath-ratio-+ '(4 . 5) '(2 . 3)) '(22 . 15)))
  (should (equal (emath-ratio-+ '(1 . 3) '(1 . 6)) '(1 . 2)))
  (should (equal (emath-ratio-+ '(1 . 264) '(1 . 312)) '(1 . 143)))
  (should (equal (emath-ratio-+ '(3 . 35) '(1 . 21)) '(2 . 15)))
  (should (equal (emath-ratio-+ '(-3 . 35) '(1 . 21)) '(-4 . 105)))
)

(defun emath-ratio-* (a b)
  ;; Compute cross-fraction gcds, to avoid large intermediate products.
  (let ((gcd-ab (gcd (car a) (cdr b)))
        (gcd-ba (gcd (car b) (cdr a))))
    (cons (* (/ (car a) gcd-ab) (/ (car b) gcd-ba))
          (* (/ (cdr a) gcd-ba) (/ (cdr b) gcd-ab)))))

(defun emath-ratio-reciprocal (a)
  (emath-ratio-normalize-sign (cons (cdr a) (car a))))

(ert-deftest emath-ratio-* ()
  (should (equal (emath-ratio-* (cons (* 5 11) (* 2 3)) (cons (* 3 13) (* 5 7)))
                 (cons (* 11 13) (* 2 7))))
  (should (equal (emath-ratio-* '(61 . 59) '(59 . 61)) '(1 . 1)))
  (should (equal (emath-ratio-* '(-1 . 1) '(22 . 7)) '(-22 . 7)))
  )

(defun emath-ratio-normalize-sign (a)
  (if (< (cdr a) 0)
      (cons (- (car a)) (- (cdr a)))
    a))

(defun emath-ratio-**-integer (base exp)
  (cond
   ((equal exp 1) base)
   ((equal exp -1) (emath-ratio-normalize-sign
                    (cons (cdr base) (car base))))
   ((< exp 0) (emath-reciprocal (cons (expt (car base) (- exp))
                                      (expt (cdr base) (- exp)))))
   (t (cons (expt (car base) exp)
            (expt (cdr base) exp)))))

(ert-deftest emath-ratio-**-integer ()
  (should (equal (emath-ratio-**-integer '(2 . 3) 1) '(2 . 3)))
  (should (equal (emath-ratio-**-integer '(2 . 3) -1) '(3 . 2)))
  (should (equal (emath-ratio-**-integer '(-2 . 3) -1) '(-3 . 2)))
  (should (equal (emath-ratio-**-integer '(5 . 7) 2) '(25 . 49)))
  (should (equal (emath-ratio-**-integer '(5 . 7) -2) '(49 . 25)))
  )


;;; primitives

(defun fold (fn initial list)
  (while list
    (setq initial (funcall fn initial (car list))
          list (cdr list)))
  initial)

(defun app-p (op e)
  (and (consp e) (eq (car e) op)))

(defun emath-insert (map combine omitp key value)
  (let ((pair (assoc key map)))
    (if (not pair)
        (setq map (append map `((,key . ,value))))
      (let ((new (funcall combine value (cdr pair))))
        (if (funcall omitp new)
            (setq map (delq pair map))
          (setf (cdr pair) new)))))
  map)

(defun emath-topological-sort (edges node-eq-p from to)
    ;; Alist mapping each node to a list of the form (count . targets), where
    ;; count is the number of inbound edges, and targets is a set of the targets
    ;; of its outbound edges.
    (let (nodes)
      (dolist (edge edges)
        (let ((from (funcall from edge))
              (to (funcall to edge)))
          (let ((entry (cl-assoc to nodes :test node-eq-p)))
            (if entry (incf (cadr entry))
              (push (list to 1) nodes)))
          (let ((entry (cl-assoc from nodes :test node-eq-p)))
            (if entry (push to (cddr entry))
              (push (list from 0 to) nodes)))))

      ;; Divide the node list into those that have no incoming edges (`ready')
      ;; and those that do (`pending').
      (let (ready pending)
        (dolist (node nodes)
          (if (zerop (cadr node))
              (push node ready)
            (push node pending)))

        ;; Treat `ready` as a working queue of ready nodes. Repeatedly: dequeue
        ;; a node, add it to the result, delete its outgoing edges, and enqueue
        ;; any `pending' nodes whose incoming count drops to zero.
        (let (sorted)
          (while (consp ready)
            (let ((node (pop ready)))
              (push (car node) sorted)
              (dolist (to (cddr node))
                (let ((entry (cl-assoc to pending :test node-eq-p)))
                  (when (zerop (decf (cadr entry)))
                    (setq pending (delq entry pending))
                    (setq ready (nconc ready (list entry))))))))
          (reverse sorted)))))

(defconst emath-test-dag
  '((5 . 11)
    (7 . 11) (7 . 8)
    (3 . 8) (3 . 10)
    (11 . 2) (11 . 9) (11 . 10)
    (8 . 9)))

(ert-deftest emath-topological-sort ()
  (should (equal (emath-topological-sort '((3 . 8) (3 . 10)) #'eq #'car #'cdr)
                 '(3 8 10)))
  (should (equal (emath-topological-sort emath-test-dag #'eq #'car #'cdr)
                 '(5 7 3 11 8 10 2 9)))
)



(provide 'emath)
