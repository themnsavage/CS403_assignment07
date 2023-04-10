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

(define (is_valid_test_conditions test_conditions)
    (define r (modulo (length test_conditions) 3))
    (cond
        ((eqv? r 0) #t)
        (else #f)
    )
)

(define (is_file_valid file_name)
    (cond ((file-exists? file_name)
            #t
        )

        (else 
            #f
        )
    )
)

(define (str-split str ch)
  (let ((len (string-length str)))
    (letrec
      ((split
        (lambda (a b)
          (cond
            ((>= b len) (if (= a b) '() (cons (substring str a b) '())))
              ((char=? ch (string-ref str b)) (if (= a b)
                (split (+ 1 a) (+ 1 b))
                  (cons (substring str a b) (split b b))))
                (else (split a (+ 1 b)))))))
                  (split 0 0))))

(define (get_lines name)
  (let ((port (open-input-file name)))
        (define lines(get_lines_helper port '()))
        (close-input-port port)
    lines))

(define (get_lines_helper port lines)
  (let ((stuff (read-line port)))
    (if (eof-object? stuff)
	lines
	(begin 
	        (get_lines_helper port (append lines (str-split stuff #\space)))))))


(define (perform . params)
    (define command(first params))
    (define file_name(second params))
    (define valid_file(is_file_valid file_name))
    (cond
        ((eqv? valid_file #f)
            (display "Unable to open ") (display file_name) (display " for reading.")(newline)
        )
        (else
            (display "lines list: ")(display (get_lines file_name))(newline)
        )
    )
    
    (define test_conditions(cdr  (cdr params)))
    (define valid_test_conditions(is_valid_test_conditions test_conditions))
    (cond
        ((and (eqv? valid_test_conditions #t) (eqv? valid_file #t))
            (display "running test conditions")(newline)
        )
        ((eqv? valid_test_conditions #f)
            (display "Incorrect number of arguments.")(newline)
        )
    )
    
)
