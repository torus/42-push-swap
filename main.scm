(add-load-path! ".")
(use push-swap)

(define (main . args)
  (print (stack-rotate (make-stack (list 1 2 3 4))))
  (print (stack-reverse-rotate (make-stack (list 1 2 3 4))))
  )
