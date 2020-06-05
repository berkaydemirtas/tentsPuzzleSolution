#lang scheme
; 2017400234

;You can replace #f's with your function definitions and define more helper functions as you need to use this template.

; Solver function
(define TENTS-SOLUTION (lambda (list1) (
                                        if(eq? (SUM (car list1)) (SUM (car(cdr list1))) )
                                          (if(eq? (SUM (car list1)) (size (car(cdr(cdr list1))) 0)  )          
        ( TENTS-SOLUTION2 (car list1) (car(cdr list1)) (car(cdr(cdr list1))) (car(cdr(cdr list1))) (clear3 (car(cdr(cdr list1))) (clear2 (clear (size (car list1) 0) (size (car(cdr list1)) 0) 1 '() (car list1)) (car(cdr list1)) '() ) '())'())
                                     #f)
                                          #f
                                        )) )

(define TENTS-SOLUTION2 (lambda (row column trees trees2 locs list1 )(
               
                 if(AdjacentLists list1 list1)
                   (if(rowAndColumnCheck list1 row column )
                    ( if(null? trees)
                        list1                           
                  (RETURN-FIRST-NOT-FALSE2 TENTS-SOLUTION2 row column (cdr trees) trees2 locs (intersection (NEIGHBOR-LIST (car trees)) locs '() ) list1))
                    #f)
                   #f

                                                ) ))

(define RETURN-FIRST-NOT-FALSE2 (lambda (x row column trees trees2 locs neigs list1 ) (
                                                  if(null? neigs)
                                                    #f
                                                    

                                                     ((lambda (a) ( if a
                                                     a
                                                    (RETURN-FIRST-NOT-FALSE2 x row column trees trees2 locs (cdr neigs) list1)  ))    
                                                    ( x row column  trees trees2 locs (cons (car neigs) list1 )  )   )
                                                  
                                                                                  )))

(define SUM(lambda (list1)(
               if(null? list1)
                  0
                  (+ (car list1) (SUM (cdr list1))) 
                           )))

(define REPLACE-NTH (lambda (list1 x y)(
                                  if(eq? x 1)
                                    (cons y (cdr list1))
                                    (cons (car list1) (REPLACE-NTH (cdr list1) (- x 1) y))
                                    
                                       
                               )))
; Helper functions

(define commonElement(lambda (list1 list2)(
                        if(null? list1)
                          #t
                        (if(member (car list1) list2)
                          #f
                          (commonElement (cdr list1) list2))

                                           )))

(define change(lambda(list1 element list2)(
                             if(null? list1)
                               list2
                             (if(eq? (car(car list1)) (car element))
                                  (if(eq? (car(cdr(car list1))) (car(cdr element)))
                                 (change (cdr list1) element (cons '(-2 -2) list2))
                               (change (cdr list1) element (cons (car list1) list2)))
                                  (change (cdr list1) element (cons (car list1) list2))

                                     ))))


(define size (lambda (list1 x)(
                              if(null? list1)
                                x
                                (size (cdr list1) (+ 1 x))
                              )))

(define clear (lambda (x y z list1 list2)(

                            if(eq? x 0)
                                list1              
                            (if(eq? (nthElement x list2) 0)
                              (clear  (- x 1) y 1 list1 list2 )
                             
                                (if(eq? y z)
                                (clear  (- x 1) y 1 (cons (Appendd x z) list1) list2 )   
                                (clear x y (+ z 1)(cons (Appendd x z) list1) list2 )))
                     
                            )))

(define clear2 (lambda (list1 list2 list3)(
                    if(null? list1)
                      list3
                    (if(eq? (nthElement (car(cdr(car list1))) list2 ) 0 )
                       (clear2 (cdr list1) list2 list3)
                       (clear2 (cdr list1) list2 (cons (car list1) list3)))
  
                                     )))

(define clear3 (lambda (trees list1 list2)(
                           if(null? list1)
                             list2
                           (if(member (car list1) trees)
                             (clear3 trees (cdr list1) list2)
                             (clear3 trees (cdr list1) (cons (car list1) list2)))

                                           )))

(define intersection (lambda (neigs list1 list2)(
                                  if(null? neigs)
                                    list2
                                    (
                                     if(member (car neigs) list1)
                                       (intersection (cdr neigs) list1 (cons (car neigs) list2))
                                       (intersection (cdr neigs) list1 list2)
                                       


                                     )
 

                                            )))

(define rowAndColumnCheck(lambda (list1 list2 list3)(

                                if(member -1 list2)
                                    #f
                                  (if(member -1 list3)
                                     #f                     
                                  (if(null? list1)
                                  #t
                                 
                                 (
                                  if(< (car (car list1)) 1)
                                 #f
                                 (if(< (car(cdr(car list1))) 1 )
                                   #f
                                   (if(> (car (car list1)) (size list2 0) )
                                      #f
                                      (if(> (car (cdr(car list1))) (size list3 0) )
                                      #f
                                    
                                 (rowAndColumnCheck (cdr list1) (REPLACE-NTH list2 (car(car list1)) (- (nthElement (car(car list1)) list2) 1)) (REPLACE-NTH list3 (car(cdr(car list1))) (- (nthElement (car(cdr(car list1))) list3) 1)) 
                                  ))))

                                            ))))))
(define allZero(lambda (list1)(

                     if(null? list1)
                     #t
                     (if(eq? (car list1) 0)
                     (allZero (cdr list1))
                     #f)

                               )))

(define AdjacentLists (lambda (list1 list2)(
                                  if(null? list1)
                                    #t
                                  (if(ADJACENT-WITH-LIST (car list1) (cdr list2))
                                     #f
                                   (AdjacentLists (cdr list1) (cdr list2)))
                                   
                                       )))



(define nthElement (lambda (x list1)(
                           if(eq? x 1)
                             (car list1)
                             (nthElement (- x 1) (cdr list1))
                             

                                          )))


                                                                                                
(define RETURN-FIRST-NOT-FALSE (lambda (x list) (
                                                  if(null? list)
                                                    #f
                                                    (
                                                  if(x(car list))
                                                    (x(car list))
                                                    (RETURN-FIRST-NOT-FALSE x (cdr list)))
                                                                                  )))

(define ADJACENT (lambda (list1 list2)(
                                       if(eq? (car list1) (car list2))
                                         
                                          (if(eq? (car (cdr list1)) (car (cdr list2)))
                                             #t
                                         (if(eq? (- (car(cdr list1)) (car(cdr list2))) 1)
                                                  #t
                                                  (if(eq? (- (car(cdr list1)) (car(cdr list2))) -1)
                                                     #t
                                                     #f)


                                          ))
                                         (if(eq? (car (cdr list1)) (car (cdr list2)))
                                            (
                                             if(eq? (- (car list1) (car list2)) 1)
                                                  #t
                                                  (if(eq? (- (car list1) (car list2)) -1)
                                                     #t
                                                     #f)

                                             )
                                            (if(eq? (- (car list1) (car list2)) 1 )
                                              ( if(eq? (- (car(cdr list1)) (car(cdr list2))) 1)
                                                  #t
                                                  (if(eq? (- (car(cdr list1)) (car(cdr list2))) -1)
                                                     #t
                                                     #f))
                                                  
                                              (
                                               if(eq? (- (car list1) (car list2)) -1 )
                                              ( if(eq? (- (car(cdr list1)) (car(cdr list2))) 1)
                                                  #t
                                                  (if(eq? (- (car(cdr list1)) (car(cdr list2))) -1)
                                                     #t
                                                     #f)
                                                  )

                                              #f

                                               )
                                              
                                            )
                                       ))))
(define Appendd(lambda ( x y)(
                         cons x (Appendd2 '() y) 
                             )))

(define Appendd2(lambda (list1 x)(
                         cons x list1 
                                 )))

(define NEIGHBOR-LIST (lambda (list1)(
                             list (Appendd (+ 1 (car list1)) (car (cdr list1))) (Appendd (- (car list1) 1) (car (cdr list1))) (Appendd  (car list1)(+ 1 (car (cdr list1)))) (Appendd  (car list1) (-(car (cdr list1))1))

                                       )))

(define ADJACENT-WITH-LIST (lambda (list1 list2)(
                                            if(null? list2)
                                               #f
                                               (if(ADJACENT list1 (car list2))
                                                  #t
                                                  (ADJACENT-WITH-LIST list1 (cdr list2)))
                                               )))
