#lang racket

(provide
 define-remote-datatypes
 define-remote-proxy)

(require
  SwDev/Testing/communication
  (for-syntax (rename-in syntax/parse [define/syntax-parse def/stx])
              syntax/stx))

(begin-for-syntax
  (struct remote-datatype-binding (to-jsexpr-sym from-jsexpr-sym)
    #:property prop:procedure
    (lambda (s stx)
      (raise-syntax-error
       #f
       "remote datatypes can only be referred to in remote proxy definitions"
       stx)))
  (define-syntax-class bound-id
    (pattern id #:do [(unless (identifier-binding #'id)
                        (raise-syntax-error #f "unbound identifier" #'id))])))

(define-syntax (define-remote-datatypes stx)
  (syntax-parse stx
    [(_ [name:id to-jsexpr:bound-id from-jsexpr:bound-id] ...)
     #'(begin
         (define-syntax name
           (remote-datatype-binding #'to-jsexpr #'from-jsexpr))
         ...
         )]))

(begin-for-syntax
  (define-syntax-class arg-spec
    (pattern (~var b (static remote-datatype-binding? "remote datatype binding"))
             #:attr to-jsexpr (remote-datatype-binding-to-jsexpr-sym
                               (attribute b.value))
             #:attr from-jsexpr (remote-datatype-binding-from-jsexpr-sym
                                 (attribute b.value)))
    (pattern contract:expr
             #:attr to-jsexpr #'identity
             #:attr from-jsexpr
             (with-syntax ([c (syntax/loc #'contract
                                (invariant-assertion contract x))])
               #'(lambda (x) c x))))
  
  (define-syntax-class method-spec
    #:datum-literals (->)
    #:auto-nested-attributes
    (pattern [name:id (-> arg:arg-spec ... return:arg-spec)]
             #:attr string-name (datum->syntax #'here
                                               (symbol->string (syntax-e #'name)))))

  (define (generate-proxy ms)
    (def/stx (m:method-spec ...) ms)
    (def/stx ((tmp ...) ...) (stx-map generate-temporaries #'((m.arg ...) ...)))
    
    #'(class object%
        (init-field in)
        (init-field out)
        (super-new)
        
        (define/public (m.name tmp ...)
          (let ([msg (list m.string-name (m.arg.to-jsexpr tmp) ...)])
            (send-message msg out)
            (let ([res (read-message in)])
              (m.return.from-jsexpr res))))
        ...
        ))

  (define (generate-driver ms)
    (def/stx (m:method-spec ...) ms)
    (def/stx ((tmp ...) ...) (stx-map generate-temporaries #'((m.arg ...) ...)))

    #'(lambda (in out object)
        (let loop ()
          (match (read-message in)
            [(list m.string-name tmp ...)
             (let ([res (send object m.name (m.arg.from-jsexpr tmp) ...)])
               (send-message (m.return.to-jsexpr res) out))]
            ...
            )
          (loop)))))

(define-syntax (define-remote-proxy stx)
  (syntax-parse stx
    [(_ (proxy:id driver:id)
        m:method-spec
        ...+)
     (def/stx proxy-expr (generate-proxy #'(m ...)))
     (def/stx driver-expr (generate-driver #'(m ...)))
     #'(begin
         (define proxy proxy-expr)
         (define driver driver-expr))]))

