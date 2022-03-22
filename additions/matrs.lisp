(defun first-column (matrix)
  (cond ((null matrix) nil)
        (t (cons (car (car matrix)) (first-column (cdr matrix))))))

(defun remain-columns (matrix)
  (cond ((null matrix) nil)
        (t (cons (cdr (car matrix)) (remain-columns (cdr matrix))))))

(defun ll-transpose (matrix)
  (cond ((null matrix) nil)
        ((null (car matrix)) nil)
        (t (cons (first-column matrix) (ll-transpose (remain-columns matrix))))))


(defun del-elem (k lst)
  (cond ((zerop k) (cdr lst))
        ((= k (- (length lst) 1)) (butlast lst))
        (t (nconc (subseq lst 0 k) (subseq lst (+ k 1))))))
 
(defun minor (matr k)
 (let ((m (- k 1)))
  (* (nth m (car matr)) (expt -1 m)
     (ll-determinant (mapcar #'(lambda (x) (del-elem m x)) (cdr matr))))))
 
(defun minors-internal (matr i n acc) 
    (cond ((eql i n) acc)
          (t (minors-internal matr (+ i 1) n (cons (minor matr (+ i 1)) acc)))))

(defun ll-determinant (matr)
  (let ((n (length matr)))
    (cond ((= n 0) nil)
          ((= n 1) (caar matr))
          ((= n 2) (- (* (caar matr) (cadadr matr)) (* (cadar matr) (caadr matr))))
          (t (apply #'+ (minors-internal matr 0 n ()))))))


(defun del-intersec (matr i j) 
    (del-elem i (mapcar #'(lambda (mtr) (del-elem j mtr)) matr)))

(defun nappend-elem (lst el) 
    (nconc lst (cons el nil)))

(defun inv-matr (matr det i j n res acc) 
    (cond ((= i n) (ll-transpose res))
          ((= j n) (inv-matr matr det (+ i 1) 0 n (nappend-elem res acc) ()))
          (t (inv-matr matr det i (+ j 1) n res (nappend-elem acc (* (expt -1 (+ i j)) (/ (ll-determinant (del-intersec matr i j)) det)))))))

(defun ll-inverse-matrix (matr)
    (let ((det (ll-determinant matr)))
        (cond ((or (eql det nil) (eql 0 det)) nil)
            (t (inv-matr matr det 0 0 (length matr) () ())))))