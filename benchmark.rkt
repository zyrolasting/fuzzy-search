#lang racket/base

(require racket/random
         racket/string
         "main.rkt")

(define full-charset
  (string-append
   "abcdefghijklmnopqrstuvwxyz"
   "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
   "01234567890"
   "()[]{}~!@#$%^&*`;:'\"<>.,|\\+=-_?/"))

(define (random-string charset len)
  (apply string (random-sample charset len)))

(define (random-needle len)
  (random-string full-charset len))

(define (random-haystack needle word-count word-length)
  (define last-char (string (string-ref needle (sub1 (string-length needle)))))
  (define restricted-charset (string-replace full-charset last-char ""))
  (string-join
   (for/list ([i (in-range word-count)])
     (if (= i (sub1 word-count))
         (string-append (random-string restricted-charset (sub1 word-length)) last-char)
         (random-string restricted-charset word-length)))
   " "))

(define (trial needle-length word-count word-length)
  (define needle (random-needle needle-length))
  (define haystack (random-haystack needle word-count word-length))
  (time-apply (λ () (fuzzy-search needle haystack
                                  #:max-match-count (* needle-length 2)
                                  #:max-depth 3)) null))

(define (mean nums)
  (exact->inexact (/ (foldl + 0 nums) (length nums))))

(define (mean-timings timings)
  (list (mean (map cadr timings))
        (mean (map caddr timings))
        (mean (map cadddr timings))))

(define (run-trials n needle-length word-count word-length)
  (mean-timings
   (for/list ([i (in-range n)])
     (begin0
         (call-with-values (λ () (trial needle-length word-count word-length))
                           list)
       (collect-garbage 'minor)))))

(module+ main
  (require racket/cmdline
           racket/list
           racket/match)

  (command-line
   #:args (trial-count
           needle-length-min
           needle-length-max
           word-count
           word-length)

   (define argv
     (list trial-count
           needle-length-min
           needle-length-max
           word-count
           word-length))

   (match-define (list trial-count*
                       needle-length-min*
                       needle-length-max*
                       word-count*
                       word-length*)
     (map (λ (pos arg)
            (define maybe-num (string->number arg))
            (unless (number? maybe-num)
              (eprintf "Expected numerical argument in position ~a. Got '~v'." pos arg)
              (exit 1))
            (inexact->exact (truncate maybe-num)))
          (range (length argv))
          argv))

   (printf (string-append
            "Mean times of ~a trial(s) using ~a ~a-character~n"
            "word(s) and ~a to ~a-character needles:~n~n")
           trial-count*
           word-count*
           word-length*
           needle-length-min*
           needle-length-max*)
   (define-values (res total-cpu total-real total-gc)
     (time-apply
      (λ ()
        (for ([needle-length (in-range needle-length-min* (add1 needle-length-max*))])
          (match-define (list cpu real gc) (run-trials trial-count* needle-length word-count* word-length*))
          (printf "needle-length: ~a\tcpu: ~a\treal: ~a\tgc: ~a~n"
                  needle-length cpu real gc)))
      null))
   (printf "~nTotal timings (includes dataset construction and cleanup):~n")
   (printf "  cpu: ~a real: ~a gc: ~a~n"
           total-cpu total-real total-gc)))
