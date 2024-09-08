#lang racket

;; Function to generate a list of random integers
(define (generate-random-integers count min-value max-value)
  (define (generate n)
    (if (zero? n)
        '()
        (cons (random min-value (add1 max-value))
              (generate (- n 1)))))
  (generate count))

;; Helper function to find the median of a list
(define (median lst)
  (let ([sorted (sort lst <)])
    (list-ref sorted (quotient (length sorted) 2))))

;; Helper function to split the list into sublists of size 5
(define (split-into-sublists lst)
  (define (split-iter lst acc)
    (cond [(null? lst) (reverse acc)]
          [(< (length lst) 5) (reverse (cons lst acc))]
          [else (split-iter (drop lst 5) (cons (take lst 5) acc))]))
  (split-iter lst '()))

;; Selection sort for sorting small sublists
(define (selection-sort lst)
  (define (sort-helper unsorted sorted)
    (cond
      [(null? unsorted) (reverse sorted)]
      [else
       (let ([min-item (apply min unsorted)])
         (sort-helper (filter (lambda (x) (not (= x min-item))) unsorted)
                      (cons min-item sorted)))]))
  (sort-helper lst '()))

;; Find the median of a list of medians
(define (median-of-medians lst)
  (let ([sorted-lists (map selection-sort (split-into-sublists lst))])
    (if (<= (length lst) 5)
        (median lst)
        (median (map median (split-into-sublists lst))))))

;; Partition the list based on the pivot
(define (partition lst pivot)
  (let ([less (filter (lambda (x) (< x pivot)) lst)]
        [equal (filter (lambda (x) (= x pivot)) lst)]
        [greater (filter (lambda (x) (> x pivot)) lst)])
    (list less equal greater)))

;; Quicksort using median-of-medians
(define (quicksort lst)
  (if (null? lst)
      '()
      (let ([pivot (median-of-medians lst)])
        (let ([part (partition lst pivot)])
          (append (quicksort (car part))
                  (cadr part)
                  (quicksort (caddr part)))))))

;; Function to find the minimum value in a list
(define (find-min lst)
  (apply min lst))

;; Function to find the maximum value in a list
(define (find-max lst)
  (apply max lst))

;; Function to filter list based on comparison type and value
(define (filter-by-comparison lst value comparison-type)
  (cond
    [(equal? comparison-type 'less-than)
     (filter (lambda (x) (< x value)) lst)]
    [(equal? comparison-type 'equal-to)
     (filter (lambda (x) (= x value)) lst)]
    [(equal? comparison-type 'greater-than)
     (filter (lambda (x) (> x value)) lst)]
    [else
     (error "Invalid comparison type. Use 'less-than, 'equal-to, or 'greater-than.")]))

;; Function to check if a list is sorted in non-decreasing order
(define (is-sorted? lst)
  (define (check-sorted? prev lst)
    (cond
      [(null? lst) #t]
      [(< prev (car lst)) (check-sorted? (car lst) (cdr lst))]
      [(= prev (car lst)) (check-sorted? (car lst) (cdr lst))]
      [else #f]))
  (if (null? lst) #t (check-sorted? (car lst) (cdr lst))))

;; Test cases
(define (test-sorting size)
  (displayln (format "Testing list of size ~a:" size))
  (define test-list (generate-random-integers size 0 100))
  (define sorted-list (quicksort test-list))
  (displayln "Unsorted list:")
  (displayln test-list)
  (displayln "Sorted list:")
  (displayln sorted-list)
  (displayln (format "Minimum value: ~a" (find-min test-list)))
  (displayln (format "Maximum value: ~a" (find-max test-list)))
  (displayln "Filtering results on the sorted list:")
  (displayln (format "Items less than 50: ~a" (filter-by-comparison sorted-list 50 'less-than)))
  (displayln (format "Items equal to 50: ~a" (filter-by-comparison sorted-list 50 'equal-to)))
  (displayln (format "Items greater than 50: ~a" (filter-by-comparison sorted-list 50 'greater-than)))
  (displayln (format "Is the original list sorted? ~a" (is-sorted? test-list)))
  (displayln (format "Is the sorted list sorted? ~a" (is-sorted? sorted-list)))
  (newline))

;; Run tests for lists of different sizes
(test-sorting 4)
(test-sorting 43)
(test-sorting 403)
;; (test-sorting 400003) ;; Commented out for faster execution
