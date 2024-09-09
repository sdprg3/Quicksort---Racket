#lang racket

;; Generate a list of random integers
(define (generate-random-integers count min-value max-value)
  (define (generate n)
    (if (zero? n)
        '()
        (cons (+ min-value (random (- max-value min-value)))
              (generate (- n 1)))))
  (generate count))

;; Function to find the median of a list
(define (median lst)
  (let ([sorted (sort lst <)])
    (list-ref sorted (quotient (length sorted) 2))))

;; Function to find the median of medians (pivot selection)
(define (median-of-medians lst)
  (let loop ([remaining lst] [medians '()])
    (cond
      [(not (at-least-5-items? remaining)) 
       (median remaining)]
      [else
       (loop (drop remaining 5) 
             (cons (median (take remaining 5)) medians))])))

;; Helper function to check if list has at least 5 items
(define (at-least-5-items? lst)
  (define (check lst n)
    (cond
      [(null? lst) #f]      ;; If the list is empty, return #f
      [(= n 0) #t]          ;; If we've seen at least 5 items, return #t
      [else (check (cdr lst) (- n 1))])) ;; Recursively check the rest of the list
  (check lst 5))

;; Selection sort for small lists
(define (selection-sort lst)
  (define (select-min lst)
    (if (null? (cdr lst))
        lst
        (let* ([min-val (apply min lst)]
               [min-index (index-of min-val lst)]
               [rest (remove min-val lst)])
          (append (list min-val) (selection-sort rest)))))
  
  (define (index-of value lst)
    (let loop ([lst lst] [index 0])
      (cond
        [(null? lst) (error "Value not found in the list")]
        [(equal? (car lst) value) index]
        [else (loop (cdr lst) (+ index 1))])))
  
  (select-min lst))

;; Quicksort using the median-of-medians partitioning method
(define (qsrt lst)
  (cond
    [(null? lst) '()]       ;; Base case: empty list is already sorted
    [(null? (cdr lst)) lst] ;; Base case: single-item list is already sorted
    [(<= (length lst) 5) (selection-sort lst)] ;; Use selection sort for small lists
    [else
     (let* ([pivot (median-of-medians lst)]
            [smaller (filter (lambda (x) (< x pivot)) lst)]
            [equal (filter (lambda (x) (= x pivot)) lst)]
            [greater (filter (lambda (x) (> x pivot)) lst)])
       (append (qsrt smaller) equal (qsrt greater)))]))

;; Function to filter a list based on a value and comparison type
(define (filter-by-comparison lst value comparison-type)
  (cond
    [(equal? comparison-type 'less)
     (filter (lambda (x) (< x value)) lst)]
    [(equal? comparison-type 'equal)
     (filter (lambda (x) (= x value)) lst)]
    [(equal? comparison-type 'greater)
     (filter (lambda (x) (> x value)) lst)]
    [else (error "Invalid comparison type. Use 'less, 'equal, or 'greater.")]))

;; Function to find the minimum value of a list
(define (find-min lst)
  (foldl min (car lst) (cdr lst)))  ;; Start with the first item and fold with the min function

;; Function to find the maximum value of a list
(define (find-max lst)
  (foldl max (car lst) (cdr lst)))  ;; Start with the first item and fold with the max function

;; Function to check if a list is sorted in nondecreasing order
(define (sorted? lst)
  (define (check lst)
    (cond
      [(or (null? lst) (null? (cdr lst))) #t]
      [(<= (car lst) (cadr lst)) (check (cdr lst))]
      [else #f]))
  (check lst))

;; Function to test sorting and manage printing based on list size
(define (test-sorting size)
  (displayln (format "Testing list of size ~a:" size))
  (define test-list (generate-random-integers size 0 10000))
  (define sorted-list (qsrt test-list))
  
  ;; Display the sorted list if the size is smaller than 100,000
  (if (< size 100000)
      (begin
        (displayln "Sorted list:")
        (displayln sorted-list))
      (displayln "Sorted list not displayed for large list size."))
  
  ;; Find and display the minimum and maximum values of the list
  (define min-value (find-min test-list))
  (define max-value (find-max test-list))
  (displayln (format "Minimum value: ~a" min-value))
  (displayln (format "Maximum value: ~a" max-value))
  
  ;; Check if the sorted list is actually sorted
  (displayln (format "Is the list sorted? ~a" (sorted? sorted-list)))
  
  ;; Filter sorted list items based on comparison type and the value 5000
  (define filtered-less (filter-by-comparison sorted-list 5000 'less))
  (define filtered-equal (filter-by-comparison sorted-list 5000 'equal))
  (define filtered-greater (filter-by-comparison sorted-list 5000 'greater))

  ;; Display the filtered results if the size is smaller than 100,000
  (if (< size 100000)
      (begin
        (displayln "Filtered list (less than 5000):")
        (displayln filtered-less)
        (displayln "Filtered list (equal to 5000):")
        (displayln filtered-equal)
        (displayln "Filtered list (greater than 5000):")
        (displayln filtered-greater))
      (displayln "Filtered results not displayed for large list size."))
  
  ;; Add two newlines after each test result
  (displayln "")
  (displayln ""))

;; Test sorting for lists of size 4, 43, 403, 400,003, and 10,000,000
(test-sorting 4)
(test-sorting 43)
(test-sorting 403)
(test-sorting 400003)
(test-sorting 10000000)
