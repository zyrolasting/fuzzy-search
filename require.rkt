#lang racket/base

(provide fuzzy)

(require racket/require-syntax
         (for-syntax racket/base
                     racket/file
                     racket/require-transform
                     racket/path
                     syntax/location
                     syntax/parse
                     "main.rkt"))

(define-for-syntax (find-proximate-modules dir)
  (map (λ (p) (path->string (find-relative-path dir p)))
       (find-files (λ (p) (and (file-exists? p)
                               (equal? (path-get-extension p)
                                       #".rkt")))
                   dir)))

(define-for-syntax (find-best-path needle proximate)
  (define-values (_ best-path)
    (for/fold ([score 0]
               [path #f])
              ([prox (in-list proximate)])
      (define-values (match?* score* table*)
        (fuzzy-search needle prox))
      (if (> score* score)
          (values score* prox)
          (values score path))))
  best-path)

(define-syntax fuzzy
  (make-require-transformer
   (λ (stx)
     (syntax-parse stx
       [(_ needle:expr ...+)
        (let* ([dir (simplify-path (or (syntax-source-directory stx)
                                       (current-directory)))]
               [proximate (find-proximate-modules dir)])
          (expand-import
           (datum->syntax
            stx
            `(combine-in . ,(map (λ (n) `(file ,(find-best-path (syntax-e n) proximate)))
                                 (syntax->list #'(needle ...)))))))]))))
