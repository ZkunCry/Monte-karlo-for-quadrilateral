(defun monte-karlo (lst)
    (let ((x1 (first lst)) (y1 (second lst)) 
                          (x2 (third lst)) (y2 ( fourth lst )) 
                          (x3 (fifth lst )) (y3 (sixth lst )) 
                         (x4 (seventh lst)) (y4 (eighth lst))
                         (x5 (ninth lst)) (y5 (tenth lst)) 
                          (x6 (car (cdr(cddddr(cdr (cddddr lst)))))) (y6 (car(cddr(cddddr(cdr (cddddr lst)))))) 
                          (x7  (car(cdddr(cddddr(cdr (cddddr lst)))))) (y7  (car(cddddr(cddddr(cdr (cddddr lst))))))
                           (x8  (car(cdr(cddddr(cddddr(cdr (cddddr lst))))))) (y8  (car(cddr(cddddr(cddddr(cdr (cddddr lst)))))))
                           (count 0)
                           (N 4000)
                           (accur 0)
                           (s 0))
         (setf count (for x1 y1 x2 y2 x3 y3 x4 y4 x5 y5 x6 y6 x7 y7 x8 y8  N count))
         (setf s ( / (* 16.0 count) N))
         (setf accur ( / (- s 16.0) 16.0))
         (return-from monte-karlo  (list s accur))))

(defun for (x1 y1 x2 y2 x3 y3 x4 y4 x5 y5 x6 y6 x7 y7 x8 y8  N count)
    (if (< N 1)
        (return-from for count))
    (let (  
          (x (random 5)) (y (random 5))
          (temp1 0) (temp2 0)(temp3 0)(temp4 0))
     (setf temp1 (/ (-   ( * (- y2 y1) ( - x x1)) ( * (- x2 x1) ( * y1 -1)))(- x2 x1)))
     (setf temp2 (/ (-   ( * (- y4 y3) ( - x x3)) ( * (- x4 x3) ( * y3 -1)))(- x4 x3)))
     (setf temp3 (/ (-   ( * (- x6 x5) ( - y x5)) (* -1( * x5 ( - y6 y5 ))))(- y6 y5)))
     (setf temp4 (/ (-   ( * (- x8 x7) ( - y x7)) (* -1( * x7 ( - y8 y7 ))))(- y8 y7)))
         (if(and
           (< y temp1) (> y temp2)
           (and 
            (< x temp3) (> x temp4)))
            (incf count)
            )
     (for x1 y1 x2 y2 x3 y3 x4 y4 x5 y5 x6 y6 x7 y7 x8 y8  (- N 1) count )))

(format t "~f" (monte-karlo '(-1 0 0 1 -3 0 0 1 4 0 0 4 0 2 2 0)))
(print (monte-karlo '(-1 0 0 1 -3 0 0 1 4 0 0 4 0 2 2 0)))
(print (monte-karlo '(-1 0 0 1 -3 0 0 1 4 0 0 4 0 2 2 0)))
(print (monte-karlo '(-1 0 0 1 -3 0 0 1 4 0 0 4 0 2 2 0)))
(print (monte-karlo '(-1 0 0 1 -3 0 0 1 4 0 0 4 0 2 2 0)))
  
                 
                   
