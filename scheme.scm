;;1
;; (fromTo k n)  returns the list of integers from k to n. The size
;;               of the problem can be seen as the number of integers
;;               between k and n, inclusive.
;; Base Case:  if k > n (i.e. if the size of the problem is 0), then
;;             the result is the empty list.
;; Hypothesis: Assume (fromTo (+ k 1) n) returns the list of integers
;;             from k+1 to n, since the size of that problem is one
;;             less than the size of the orignal problem, (fromTo k n).
;; Recursive Step: (fromTo k n) = (cons k (FromTo (+ k 1) n)
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
;;  Recursive Step: if (modulo (car L) m) is 0:
;;                       (removeMults m L) = (removeMults m (cdr L))
;;                  else:
;;                       (removeMults m (cdr L)) = 
;;                             (cons (car L) (removeMults m (cdr L)))
(define (removeMults m L)
    (cond ((null? L) L)
          ((= (modulo (car L) m) 0) (removeMults m (cdr L)))
          (else (cons (car L) (removeMults m (cdr L))))))


;;3
;; (removeAllMults L) returns a list containing elements of L that 
;; are not multiples of each other
;; Base Case: If the list is null, just return
;; Hypothesis: Assume removeAllMults works for (cdr L), which means
;;             startring from the second element, all elements are
;;             not multiples of each other.
;; Recursive Step: Based on the assumption, just need to remove the
;;                 elements that are multiples of (car L)
(define (removeAllMults L)
    (cond ((null? L) L)
          (else (cons (car L) (removeMults (car L) (removeAllMults (cdr L)))))))


;;4 (primes n) computes the list of all primes less than or equal to n
(define (primes n)
    (removeAllMults (fromTo 2 n)))


;;5
;;returns the maximum nesting depth of any element within L
;;Base Case: if L is empty, then the result is 0.
;;Hypothesis: If (car L) is also a list, we assume that function maxDepth also 
;;            works for (car L). Another assumption is that function maxDepth
;;            works for (cdr L). Either way, the problem size is smaller than
;;            the original one.
;;Recursive Step: The maxDepth of L is either the depth of (car L) or (cdr L),
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


;;6
;;------------------------------------------------------------------------
;; In essence, this is a rearrangement of list elements. Suppose we have
;; at least 3 elements in list, then infix to prefix transform would be
;; like this: (cadr exp) (car exp) (cddr exp)
;;                ^        ^         ^
;;                |        |         |
;;             second     first     third and beyond
;; Since (car exp) and (cdr (cdr exp)) could be non-atom(i.e. they could
;; be a list themselves), the function should to be applied to them
;; recursively: (cadr exp) (prefix (car exp)) (prefix (cddr exp))
;; Of course, the above three results should form a list as the final 
;; result:
;; (list (cadr exp) (prefix (car exp)) (prefix (cddr exp)))
;; The base case is simple: if exp is an atom, there is nothing to do, 
;; just return it. 
;; But the recursive step is subtle. I spent almost a day to tweak the
;; recursive step. A corner case(at least for me it is a corner case) is
;; a single nested list. For example, if exp is (1 + (2 * 3)), then
;; (2 * 3) should also be processed recursively.
;; ------------------------------------------------------------------------
;; (prefix exp) transforms an infix arithmetic expression exp into 
;;              prefix notation
;; Base Case: exp is atom, then return exp.
;; Hypothesis: For list exp with at least 3 elements, assume prefix also 
;;             works for (cadr exp) and (cddr exp). Another assumption
;;             is that prefix also works for nested list.
;; Recursive Step: if exp contains at least three elements: then 
;;                 (car exp) and (cddr exp) should be processed recursively:
;;                 (prefix exp) = (list (cadr exp) 
;;                                      (prefix (car exp))
;;                                      (prefix (cddr exp)))
;;                 A corner case is that exp is a single element but that
;;                 single element itself is a nested list, then
;;                 (prefix exp) = (prefix (car exp))
(define (prefix exp)
  (cond ((pair? exp)
           (cond ((null? (cdr exp)) (prefix (car exp)))
                 (else (list (cadr exp) (prefix (car exp)) (prefix (cddr exp))))))
        (else exp)))


;;7

(define (composition fns)
    (cond ((null? (cdr fns)) (lambda (x) ((car fns) x)))
          (else (lambda (x) ((car fns) ((composition (cdr fns)) x))))))


;;8
;; Processing the head(or the first few elements) of a list is much easier than
;; processing the tail.
;; (bubble-to-nth L n) returns a list containing all the elements of L, except 
;; that the largest element among the first N elements of L is now the Nth 
;; element of the resulting list, and the elements after the Nth element are 
;; left in their original order.
;; Base Case: n is 1, then there is nothing to do, just return the list.
;; Hypothesis: Assume that bubble-to-nth works for (n - 1) elements, which are
;;             from element 2 to element n.
;; Recursive Step: Make sure that (cadr L) is larger than (car L), then the
;;                 recursive call will make sure that the last one is the largest.
(define (bubble-to-nth L n)
    (cond ((= n 1) L)
          (else (if (> (car L) (cadr L))
                    (cons (cadr L) (bubble-to-nth (cons (car L) (cddr L)) (- n 1)))
                    (cons (car L) (bubble-to-nth (cdr L) (- n 1)))))))


;;9
;; (b-s L N) returns the a list containing the elements of L in their original 
;; order except that the first N elements are in sorted order.
;; Base Case: N is 1, then the first element itself is already sorted.
;; Hypothesis: (bubble-to-nth L N) will make sure that the largest 
;;             element among the first N elements of L is now the Nth element of 
;;             the resulting list.
;; Recursive Step: The recursive step will call (bubble-to-nth L (- N 1)), and
;;                 (bubble-to-nth L (- N 1)) will make sure that the largest 
;;                 element among the first N-1 elements of L is now the Nth 
;;                 element of the resulting list, and in each recursion, one
;;                 element will be in the right place.
(define (b-s1 L N)
    (cond ((= N 1) L)
          (else (b-s1 (bubble-to-nth L N) (- N 1)))))


;;10
;; (bubble-sort L) return a list of the elements of L in sorted order
(define (bubble-sort L)
    (b-s L (length L)))
