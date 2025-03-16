#lang lsl-v2

(define-contract (Divides n) (lambda (x) (zero? (modulo n x))))

(: x (Divides 10))
(define x 2)

;; EXPANDS TO:

#;(begin
    ;; defines contract
    (define Divides
      (lambda (n)
        ;; ensures usage of contracts references is allowed within the contract definition
        (syntax-parameterize ([contract-pos (make-valid-contract-transformer)])
          ;; ensure when compiling pred--an lsl expression--that contracts refs are no longer valid
          (let ([pred (syntax-parameterize ([contract-pos (make-invalid-contract-transformer)])
                        (lambda (x) (zero? (modulo n x))))])
            (unless (procedure? pred)
              (raise-syntax-error #f "invalid immediate contract (must be a predicate)" #'e))
            (new immediate%
                 [stx #'(lambda (x) (zero? (modulo n x)))]
                 [check pred])))))
    (void) ;; attach contract (:) has some compile time behavior
    (define x
      (let* ([name 'x]
             [body (rename-if-proc name 2)]
             [pos (positive-blame name (quote-module-name))]
             [neg (negative-blame name (quote-module-name))]
             [ctc (syntax-parameterize ([contract-pos (make-valid-contract-transformer)])
                    ((contract-pos Divides) 10))])
        (rt-attach-contract! pos neg ctc body))))
