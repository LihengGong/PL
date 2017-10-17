;;1
;; (fromTo k n)  returns the list of integers from k to n. The size
;;               of the problem can be seen as the number of integers
;;               between k and n, inclusive.
;; Base Case:  if k > n (i.e. if the size of the problem is 0), then
;;             the result is the empty list.
;; Hypothesis: Assume (fromTo (+ k 1) n) returns the list of integers
;;             from k+1 to n, since the size of that problem is one
;;             less than the size of the orignal problem, (fromTo k n).
;; Recursive step: (fromTo k n) = (cons k (FromTo (+ k 1) n)
(define (fromTo k n)
    (cond ((> k n) '())
          (else (cons k (fromTo (+ k 1) n)))))


;;2
;; (removeMults m L) returns a list containing all the elements of L
;;                   that are not multiples of m.
;;  Base Case: if L is empty, then return the empty list.
;;  Hypothesis: Assume removeMults works on m and (cdr L), if (car L)
;;              is a multiple of m, then (car L) should not be included
;;              in the result, otherwise it should be kept.
;;  Recursive step: if (modulo (car L) m) is 0:
;;                       (removeMults m L) = (removeMults m (cdr L))
;;                  else:
;;                       (removeMults m (cdr L)) = 
;;                             (cons (car L) (removeMults m (cdr L)))
(define (removeMults m L)
    (cond ((null? L) L)
          ((= (modulo (car L) m) 0) (removeMults m (cdr L)))
          (else (cons (car L) (removeMults m (cdr L))))))

;;3
;;(removeAllMults L) returns a list 

(define (removeAllMults L)
    (cond ((null? L) L)
          (else (cons (car L) (removeMults (car L) (removeAllMults (cdr L)))))))

;;4
(define (primes n)
    (removeAllMults (fromTo 2 n)))


;;5
;;returns the maximum nesting depth of any element within L
;;Base Case: if L is empty, then the result is 0.
;;Hypothesis: If (car L) is also a list, we assume that function maxDepth also 
;;            works for (car L). Another assumption is that function maxDepth
;;            works for (cdr L). Either way, the problem size is smaller than
;;            the original one.
;;Recursive step: The maxDepth of L is either the depth of (car L) or (cdr L),
;;                whichever is larger. More specifically, if (car L) is also 
;;                a list, then this is one level of nesting, 
;;                so the maxDepth of (car L) plus 1 is the first element's 
;;                nesting depth. Then the maxDepth of (cdr L) is also computed.
;;                The result is the larger one.
;;                If (car L) is not a list, then this element is of no interest,
;;                just check the maxDepth of (cdr L).

(define (maxDepth L)
    (cond ((null? L) 0)
          ((pair? (car L)) (let ((result1 (+ (maxDepth (car L)) 1))
                           (result2 (maxDepth (cdr L))))
                             (cond ((> result1 result2) result1)
                             (else result2))))
          (else (maxDepth (cdr L)))))
