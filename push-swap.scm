(define (make-stack vals)
  vals)

(define (make-stack-pair stack-a stack-b)
  (cons stack-a stack-b))

(define (stack-a pair-of-stack)
  (car pair-of-stack))
(define (stack-b pair-of-stack)
  (cdr pair-of-stack))

(define (stack-swap stack)
  (cons (cadr stack) (cons (caar stack) (cddr stack))))

(define (/sa p)
  (cons (stack-swap (stack-a p)) (stack-b p)))

(define (/sb p)
  (cons (stack-a p) (stack-swap (stack-b p))))

(define (/ss p)
  (cons (stack-swap (stack-a p)) (stack-swap (stack-b p))))

(define (/pa p)
  (let ((a (stack-a p))
        (b (stack-b p)))
    (if (null? b)
        p
        (make-stack-pair (cons (car b) a) (cdr b)))))

(define (/pb p)
  (let ((a (stack-a p))
        (b (stack-b p)))
    (if (null? b)
        p
        (make-stack-pair (cdr a) (cons (car a) b)))))

(define (stack-rotate stack)
  (let ((top (car stack)))
    (append (cdr stack) (list top))))

(define (/ra p)
  (make-stack-pair (stack-rotate (stack-a p)) (stack-b p)))

(define (/rb p)
  (make-stack-pair (stack-a p) (stack-rotate (stack-b p))))

(define (/rr p)
  (make-stack-pair (stack-rotate (stack-a p)) (stack-rotate (stack-b p))))

(define (stack-reverse-rotate stack)
  (let* ((rev (reverse stack))
         (last (car rev)))
    (cons last (reverse (cdr rev)))))

(define (/rra p)
  (make-stack-pair (stack-reverse-rotate (stack-a p)) (stack-b p)))

(define (/rrb p)
  (make-stack-pair (stack-a p) (stack-reverse-rotate (stack-b p))))

(define (/rrr p)
  (make-stack-pair (stack-reverse-rotate (stack-a p))
                   (stack-reverse-rotate (stack-b p))))
