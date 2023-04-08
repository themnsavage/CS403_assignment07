(define (round-off z n)
  (let ((power (expt 10 n)))
    (/ (round (* power z)) power)))

(define (test op left right)
    (op left right))


(define (append! lst . lsts)
  (if (not (null? lsts))
      (if (null? (cdr lst))
          (begin
            (set-cdr! lst (car lsts))
            (apply append! (car lsts) (cdr lsts)))

          (apply append! (cdr lst) lsts))))


(define (sphere name_value radius_value)
        (define type "sphere")
        (define name name_value)
        (define radius radius_value)
        (define pi 3.14159265358979323846)
        (define area (round-off (* 4 pi (* radius radius)) 2))
        (define volume (round-off (* (/ 4 3) pi (* radius radius radius)) 2))
        (define info (string-append "Sphere: " name ", Radius=" (number->string radius) 
        "\n     " "Surface Area: " (number->string area) " Volume: " (number->string volume) "\n"))
        (lambda (selector)
                (cond ((eqv? selector 'get_radius) radius)
                      ((eqv? selector 'get_type) type)
                      ((eqv? selector 'get_name) name)
                      ((eqv? selector 'get_area) area)
                      ((eqv? selector 'get_volume) volume)
                      ((eqv? selector 'get_info) info)
                      (else '()))))

(define (perform . params)
    (define command(first params))
    (display "command: ")(display command)(newline)
    (define file_name(second params))
    (display "file name: ")(display file_name)(newline)
    (define test_conditions(cdr  (cdr params)))
    (display "test_conditions: ")(display test_conditions)(newline)

)
