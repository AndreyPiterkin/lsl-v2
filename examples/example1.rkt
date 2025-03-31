#lang lsl-v2

(define-contract (Divides n) (lambda (x) (zero? (modulo n x))))

;; TODO: no longer works; no support for lsl-val parameterized contracts
(: x (Divides (+ 5 5)))
(define x 3)



























;; EXPANDS TO:
#;(begin
    ;; defines contract
    (define Divides
      (lambda (n)
        ;; ensures usage of contracts references is allowed within the contract definition
        (syntax-parameterize ([contract-pos (make-valid-contract-transformer)])
          ;; ensure when compiling pred--an lsl expression--that contracts refs are no longer valid
          (let ([check (syntax-parameterize ([contract-pos (make-invalid-contract-transformer)])
                         (lambda (x) (zero? (modulo n x))))]
                [gen (syntax-parameterize ([contract-pos (make-invalid-contract-transformer)])
                       #f)]
                [shrink (syntax-parameterize ([contract-pos (make-invalid-contract-transformer)])
                          #f)]
                [features (syntax-parameterize ([contract-pos (make-invalid-contract-transformer)])
                            #f)]
                [stx #'(lambda (x) (zero? (modulo n x)))])
            (unless (procedure? check)
              (raise-syntax-error #f "invalid immediate contract (must be a predicate)"
                                  #'(lambda (x) (zero? (modulo n x)))))
            (new immediate%
                 [stx stx]
                 [check check]
                 [gen gen]
                 [shrink shrink]
                 [features features])))))
    (void) ;; attach contract (:) has some compile time behavior
    (define x
      (let* ([name 'x]
             [body (rename-if-proc name 2)]
             [pos (positive-blame name (quote-module-name))]
             [neg (negative-blame name (quote-module-name))]
             [ctc (syntax-parameterize ([contract-pos (make-valid-contract-transformer)])
                    ((contract-pos Divides)
                     (syntax-parameterize ([contract-pos (make-invalid-contract-transformer)])
                       (+ 0 10))))])
        (rt-attach-contract! pos neg ctc body))))
