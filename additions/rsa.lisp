(defun coprimep (a b) (eql (gcd a b) 1))

(defun div (a b) (multiple-value-bind (q r) (floor a b) q))

(defun mod-inv (q r newq newr n) 
    (cond ((zerop newr) (cond ((> r 1) Nil) ((< q 0) (+ q n)) (t q)))
            (t (let* ((quotient (div r newr))
                        (oldq q) 
                        (q newq) 
                        (newq (- oldq (* quotient newq)))
                        (oldr r)
                        (r newr)
                        (newr (- oldr (* quotient newr)))) 
                    (mod-inv q r newq newr n)))))

(defun modular-inverse (a n) 
    (mod-inv 0 n 1 a n))

(defun chose-e (n)
    (cond ((and (> n 65537) (coprimep n 65537)) 65537)
            ((and (> n 257) (coprimep n 257)) 257)
            ((and (> n 17) (coprimep n 17)) 17)
            (t 3)))

(defun rsa-gen-key (p q)
    (let* ((n (* p q))
            (l (lcm (- p 1) (- q 1)))
            (e (chose-e n))
            (d (modular-inverse e l)))
        (list `(,e . ,n) `(,d . ,n))))

(defun rsa-code (m e n) 
    (mod (expt m e) n))

(defun rsa-decode (c d n)
    (mod (expt c d) n))