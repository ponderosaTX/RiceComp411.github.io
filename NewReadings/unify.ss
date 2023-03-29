;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname unify) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; type term ::= (app symbol list-of-term) | (var symbol) | (const symbol)
(define-struct app (rator rands))
(define-struct var (name))
(define-struct const (name))

;; type subst ::= list-of-bindings % no duplicate names; no bound variable appears on rhs of any binding!
;; type (binding symbol term)
(define-struct binding (name term))

;; var-unify: var term -> subst
(define (var-unify v t)
  (cond [(equal? v t) empty]; empty substitution
        [(occur v t) (error 'unify "unification-failure between ~a and ~a" v t)]
        [else (list (make-binding (var-name v) t))])) 

;; unify: term term -> subst
(define (unify t1 t2)
  (cond [(var? t1) (var-unify t1 t2)]
        [(const? t1) (cond [(var? t2) (list (make-binding (var-name t2) t1))]
                           [(const? t2) (if (equal? t1 t2) empty 
                                            (error 'unify "unification-failure between ~a and ~a" t1 t2))]
                           [else ; (app? t2)
                            (error 'unify "unification-failure between ~a and ~a" t1 t2)])]
        [else ; (app? t1)
         (cond [(var? t2) (var-unify t2 t1)]
               [(const? t2) (error 'unify "unification-failure between ~a and ~a" t1 t2)]
               [else ; (app? t2)
                (if (equal? (app-rator t1) (app-rator t2)) 
                    (list-unify (app-rands t1) (app-rands t2) empty)
                    (error 'unify "unification-failure between ~a and ~a" t1 t2))])]))

;; list-unify: list-of-term list-of-term subst -> subst
(define (list-unify lt1 lt2 s)
  (if (null? lt1) (if (null? lt2) s (error 'list-unify "unification-failure between ~a and ~a" lt1 lt2))
      (if (null? lt2) (error 'list-unify "unification-failure between ~a and ~a" lt1 lt2)
          (let [(t1 (apply-subst s (first lt1)))
                (t2 (apply-subst s (first lt2)))]
            (list-unify (rest lt1) (rest lt2) (compose-subst (unify t1 t2) s))))))

;; apply-subst: subst term -> term
(define (apply-subst s t)
  (cond [(var? t) (lookup t s)] ; lookup returns s if no match is found
        [(const? t) t]
        [else ; (app? t)
         (make-app (app-rator t) (map (lambda (t1) (apply-subst s t1)) (app-rands t)))]))

;; lookup: var subst -> term
(define (lookup v s)
  (cond [(null? s) v]
        [(equal? (var-name v) (binding-name (first s))) (binding-term (first s))]
        [else (lookup v (rest s))]))

;; occur: var term -> bool
(define (occur v t)
  (cond [(var? t) (equal? v t)]
        [(const? t) false]
        [else ; (app? t) 
          (ormap (lambda (t1) (occur v t1)) (app-rands t))]))

;; compose-subst: subst subst -> subst  ;; second subst is done first
(define (compose-subst s1 s2)
  ;; the none of the vars in s1 have bindings in s2, but the terms in s2 may refer to variables bound in s1
  (append s1 (map (lambda (b) (make-binding (binding-name b) (apply-subst s1 (binding-term b)))) s2)))

(define t1 (make-app 'f (list (make-var 'x) (make-var 'y))))
(define t2 (make-app 'f (list (make-app 'f (list (make-const 'a) (make-const 'b))) (make-const 'b))))
(define t3 (make-const 'b))
(define t4 (make-app 'g (list (make-var 'x) (make-var 'y))))
(define t5 (make-var 'x))
(define ans1 (list (make-binding 'y (make-const 'b)) (make-binding 'x (make-app 'f (list (make-const 'a) (make-const 'b))))))

(check-expect (unify t1 t2) ans1)

; equations b1 = list a
;           list a2 = list a
;           b = B1 -> B2
;           B1 = b1
;           B2 = int
;           a3 = int

;           list a4 = list a3
;           list a5 = list a3
;           a6 = bool 
;           list a7 = list a6
;           list a8 = list a6

(define l1 (list (make-var 'b1) 
                 (make-app 'list (list (make-var 'a2)))
                 (make-var 'b)
                 (make-var 'B1)
                 (make-var 'B2)
                 (make-var 'a3)
                 (make-app 'list (list (make-var 'a4)))
                 (make-app 'list (list (make-var 'a5)))
                 (make-var 'a6)
                 (make-app 'list (list (make-var 'a7)))
                 (make-app 'list (list (make-var 'a8)))))

(define l2 (list (make-app 'list (list (make-var 'a)))
                 (make-app 'list (list (make-var 'a)))
                 (make-app 'fun (list (make-var 'B1) (make-var 'B2)))
                 (make-var 'b1)
                 (make-const 'int)
                 (make-const 'int)
                 (make-app 'list (list (make-var 'a3)))
                 (make-app 'list (list (make-var 'a3)))
                 (make-const 'bool)
                 (make-app 'list (list (make-var 'a6)))
                 (make-app 'list (list (make-var 'a6)))))

(define s1 (list (make-binding 'a8 (make-app 'list (list (make-var 'a6))))))

(define lt1 (list (make-var 'b1) 
                  (make-app 'list (list (make-var 'a2)))
                  (make-var 'b)
                  (make-var 'B1)
                  (make-var 'B2)
                  (make-var 'a3)))

(define lt2 (list (make-app 'list (list (make-var 'a)))
                  (make-app 'list (list (make-var 'a)))
                  (make-app 'fun (list (make-var 'B1) (make-var 'B2)))
                  (make-var 'b1)
                  (make-const 'int)
                  (make-const 'int)))

(define ct1 (make-var 'a))
(define ct2 (make-app 'fun (list (make-var 'a) (make-var 'b))))