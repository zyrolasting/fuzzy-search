#lang racket/base

(require racket/contract)

; A function used to score a match.
(define score/c
  (-> (hash/c exact-nonnegative-integer?
              exact-nonnegative-integer?
              #:immutable #t)
      string?
      exact-positive-integer?
      exact-positive-integer?
      exact-integer?))

(define score/match/c
  (-> (hash/c exact-nonnegative-integer?
              exact-nonnegative-integer?
              #:immutable #t)
      string?
      exact-positive-integer?
      exact-positive-integer?
      exact-integer?
      exact-integer?
      exact-integer?))

(define score/pair/c
  (-> char?
      char?
      exact-integer?))

(provide
 score-if
 (contract-out [score/c chaperone-contract?]
               [score/match/c chaperone-contract?]
               [score/pair/c chaperone-contract?]
               [score/forrest score/c]
               [score/all (->* () #:rest (non-empty-listof score/c) score/c)]
               [score/match (->* () #:rest (non-empty-listof score/match/c) score/c)]
               [score/match/pair (->* () #:rest (non-empty-listof score/pair/c) score/match/c)]
               [score/all/unmatched-letter (-> exact-integer? score/c)]
               [score/all/leading-letter (-> exact-integer? exact-positive-integer? score/c)]
               [score/match/sequence (-> exact-integer? score/match/c)]
               [score/match/first-letter (-> exact-integer? score/match/c)]
               [score/pair/camel-case (-> exact-integer? score/pair/c)]
               [score/pair/prefixed (-> exact-integer? (non-empty-listof char?) score/pair/c)]
               [fuzzy-search
                (->* (string? string?)
                     (score/c #:max-match-count exact-positive-integer?
                              #:max-depth exact-positive-integer?
                              #:case-sensitive? any/c)
                     (values boolean?
                             number?
                             (hash/c exact-nonnegative-integer?
                                     exact-nonnegative-integer?
                                     #:immutable #t)))]))


; Returns:
;  - #t pattern was found via fuzzy search of str.
;  - A search score.
;  - A hash of indicies.
;    e.g. #hash((0 . 20)) means "The first (zero-based) match was found at position 20"
(define (fuzzy-search needle
                      haystack
                      [assign-score score/forrest]
                      #:max-match-count [max-match-count 256]
                      #:max-depth [max-depth 10]
                      #:case-sensitive? [case-sensitive? #f])
  (fuzzy-search-recursive needle
                          haystack
                          0 (string-length needle)
                          0 (string-length haystack)
                          (hasheq)
                          0
                          max-match-count
                          0
                          max-depth
                          assign-score
                          case-sensitive?))


(define (fuzzy-search-recursive needle
                                haystack
                                needle-start-index
                                needle-length
                                haystack-start-index
                                haystack-length
                                parent-table
                                match-count
                                max-match-count
                                depth
                                max-depth
                                assign-score
                                case-sensitive?)
  (define tr (if case-sensitive? values char-downcase))
  (for/fold ([needle-index needle-start-index]
             [match-count match-count]
             [my-state #f]
             [my-table parent-table]
             [best-child-state #f]
             [best-child-score 0]
             [best-child-table #hasheq()]

             #:result
             (if my-state
                 (let ([my-score (assign-score my-table haystack haystack-length match-count)])
                   (if (> best-child-score my-score)
                       (values best-child-state best-child-score best-child-table)
                       (values my-state my-score my-table)))
                 (values #f 0 (hasheq))))

            ; Iterate over the haystack. Run the body on the next needle character.
            ([haystack-index (in-range haystack-start-index haystack-length)]
             ; If we found every needle character, then we found the needle.
             #:break (or (= needle-index needle-length)
                         (>= depth max-depth))

             #:when (char=? (tr (string-ref needle needle-index))
                            (tr (string-ref haystack haystack-index))))


    (if (>= match-count max-match-count) ; Needle is everywhere. Search terms might be vague.
        (values needle-index
                match-count
                #f
                my-table
                best-child-state
                best-child-score
                best-child-table)

        (let-values ([(child-state child-score child-table)
                      (fuzzy-search-recursive needle
                                              haystack
                                              needle-index
                                              needle-length
                                              (add1 haystack-index)
                                              haystack-length
                                              my-table
                                              match-count
                                              max-match-count
                                              (add1 depth)
                                              max-depth
                                              assign-score
                                              case-sensitive?)])
          (if (and child-state (> child-score best-child-score))
              (values (add1 needle-index)
                      (add1 match-count)
                      #t
                      (hash-set my-table match-count haystack-index)
                      child-state
                      child-score
                      child-table)
              (values (add1 needle-index)
                      (add1 match-count)
                      #t
                      (hash-set my-table match-count haystack-index)
                      best-child-state
                      best-child-score
                      best-child-table))))))

;; Begin scoring procedures

(define-syntax-rule (score-if expr offset)
  (if expr offset 0))

(define (score/all . procs)
  (λ args
    (for/sum ([f (in-list procs)])
      (apply f args))))

(define (score/all/unmatched-letter vec)
  (λ (my-table haystack haystack-length match-count)
    (* vec (- haystack-length match-count))))

(define (score/all/leading-letter vec limit)
  (λ (my-table haystack haystack-length match-count)
    (max limit (* vec (hash-ref my-table 0)))))


(define (score/match . procs)
  (λ (my-table haystack haystack-length match-count)
    (for*/sum ([occurrence (in-range match-count)]
               [proc (in-list procs)])
      (let ([haystack-index (hash-ref my-table occurrence)])
        (proc my-table
              haystack
              haystack-length
              match-count
              occurrence
              haystack-index)))))

(define (score/match/sequence vec)
  (λ (my-table haystack haystack-length match-count occurrence haystack-index)
    (score-if (and (> occurrence 0)
                   (= (hash-ref my-table occurrence)
                      (add1 (hash-ref my-table (sub1 occurrence)))))
            vec)))

(define (score/match/first-letter vec)
  (λ (my-table haystack haystack-length match-count occurrence haystack-index)
    (score-if (= haystack-index 0) vec)))

(define (score/match/pair . procs)
  (λ (my-table haystack haystack-length match-count occurrence haystack-index)
    (if (> haystack-index 0)
        (let ([prev (string-ref haystack (sub1 haystack-index))]
              [curr (string-ref haystack haystack-index)])
          (for/sum ([proc (in-list procs)])
            (proc prev curr)))
        0)))

(define (score/pair/camel-case vec)
  (λ (prev curr)
    (if (and (char=? prev (char-downcase curr))
             (char=? curr (char-upcase curr)))
        vec
        0)))

(define (score/pair/prefixed vec seps)
  (λ (prev curr)
    (score-if (member prev seps) vec)))

(define score/forrest
  (score/all
   (λ _ 100)
   (score/all/leading-letter -5 -15)
   (score/all/unmatched-letter -1)
   (score/match
    (score/match/sequence 15)
    (score/match/first-letter 15)
    (score/match/pair
     (score/pair/camel-case 30)
     (score/pair/prefixed 30 '(#\space #\_))))))
