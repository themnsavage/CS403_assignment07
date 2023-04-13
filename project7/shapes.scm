(define (round-off z n)
  (let ((power (expt 10 n)))
    (/ (round (* power z)) power)))

(define (sphere name_value radius_value)
        (define type "sphere")
        (define name name_value)
        (define radius (string->number radius_value))
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

(define (box name_value length_value width_value height_value)
        (define type "box")
        (define name name_value)
        (define length (string->number length_value))
        (define width (string->number width_value))
        (define height (string->number height_value))
        (define area (round-off (* 2 (+ (* length width) (* length height) (* width height))) 2))
        (define volume (round-off (* length width height) 2))
        (define info (string-append "Box: " name ", Length=" (number->string length) ", Width=" (number->string width) ", Height=" (number->string height)
        "\n     " "Surface Area: " (number->string area) " Volume: " (number->string volume) "\n"))
        (lambda (selector)
                (cond ((eqv? selector 'get_length) length)
                      ((eqv? selector 'get_width) width)
                      ((eqv? selector 'get_height) height)
                      ((eqv? selector 'get_type) type)
                      ((eqv? selector 'get_name) name)
                      ((eqv? selector 'get_area) area)
                      ((eqv? selector 'get_volume) volume)
                      ((eqv? selector 'get_info) info)
                      (else '()))))

(define (cylinder name_value radius_value height_value)
        (define type "cylinder")
        (define name name_value)
        (define radius (string->number radius_value))
        (define height (string->number height_value))
        (define pi 3.14159265358979323846)
        (define area (round-off (+ (* 2 pi radius  height) (* 2 pi (* radius radius))) 2))
        (define volume (round-off (* pi (* radius radius) height) 2))
        (define info (string-append "Cylinder: " name ", Radius=" (number->string radius) ", Height=" (number->string height)
        "\n     " "Surface Area: " (number->string area) " Volume: " (number->string volume) "\n"))
        (lambda (selector)
                (cond ((eqv? selector 'get_radius) radius)
                      ((eqv? selector 'get_height) height)
                      ((eqv? selector 'get_type) type)
                      ((eqv? selector 'get_name) name)
                      ((eqv? selector 'get_area) area)
                      ((eqv? selector 'get_volume) volume)
                      ((eqv? selector 'get_info) info)
                      (else '()))))

(define (torus name_value small_radius_value big_radius_value)
        (define type "torus")
        (define name name_value)
        (define small_radius (string->number small_radius_value))
        (define big_radius (string->number big_radius_value))
        (define pi 3.14159265358979323846)
        (define area (round-off (* (* 2 pi big_radius) (* 2 pi small_radius)) 2))
        (define volume (round-off (* (* pi (* small_radius small_radius)) (* 2 pi big_radius)) 2))
        (define info (string-append "Torus: " name ", Small Radius=" (number->string small_radius) ", Big Radius=" (number->string big_radius)
        "\n     " "Surface Area: " (number->string area) " Volume: " (number->string volume) "\n"))
        (lambda (selector)
                (cond ((eqv? selector 'get_small_radius) small_radius)
                      ((eqv? selector 'get_big_radius) big_radius)
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

(define (read_file file_name)
  (define valid_file(is_file_valid file_name))
  (cond
      ((eqv? valid_file #f)
          (display "Unable to open ") (display file_name) (display " for reading.")(newline)
          '()
      )
      (else
          (get_lines file_name)
      )
  )
)

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
	            (get_lines_helper port (cons (str-split (list->string (reverse (cdr (reverse (string->list stuff))))) #\space) lines ))
      )
    )        
  )
)

(define(print_lines lines)
  (cond
    ((null? lines) 'done)
    (else
      (print_line (first lines))
      (display "--------------")(newline)
      (print_lines (cdr lines))
    )
  )
)

(define(print_line line)
    (cond
      ((null? line) 'done)
      (else
        (display  (first line))(newline)
        (print_line (cdr line))
      )
  )
)

(define (create_shape name shape_type shape_info)
  (cond
    ((string=? shape_type "sphere")
      (sphere name (first shape_info))
    )
    ((string=? shape_type "box")
      (box name (first shape_info) (second shape_info) (third shape_info))
    )
    ((string=? shape_type "cylinder")
      (cylinder name (first shape_info) (second shape_info))
    )
    ((string=? shape_type "torus")
      (torus name (first shape_info) (second shape_info))
    )
    (else '())
  )
)

(define (create_list_of_shapes lines list_of_shapes)
  (cond ((null? lines) (reverse list_of_shapes))
        (else
          (create_list_of_shapes (cdr lines) (append list_of_shapes (list(create_shape (first (first lines)) (second (first lines)) (cdr (cdr (first lines)))))))
        )
  )
  
)

(define (print_command list_of_shapes)
  (cond
    ((null? list_of_shapes) 'done)
    (else
      (display ((car list_of_shapes) 'get_info))
      (print_command (cdr list_of_shapes))
    )
  )
)

(define (get_count list_of_shapes count)
  (cond
    ((null? list_of_shapes) 
      count
    )
    (else
      (get_count (cdr list_of_shapes) (+ count 1))
    )
  )
)

(define (count_command list_of_shapes)
  (define count(get_count list_of_shapes 0))
  (display "There are ")(display count)(display " shapes.")(newline)
)

(define (get_min_area list_of_shapes min_area)
  (cond
    ((null? list_of_shapes)
      min_area
    )
    (else
      (cond
        ((or (null? min_area) (< ((car list_of_shapes) 'get_area) min_area))
          (get_min_area (cdr list_of_shapes) ((car list_of_shapes) 'get_area))
        )
        (else
          (get_min_area (cdr list_of_shapes) min_area)
        )

      )
    )
  )
)

(define (get_min_volume list_of_shapes min_volume)
  (cond
    ((null? list_of_shapes)
      min_volume
    )
    (else
      (cond
        ((or (null? min_volume) (< ((car list_of_shapes) 'get_volume) min_volume))
          (get_min_volume (cdr list_of_shapes) ((car list_of_shapes) 'get_volume))
        )
        (else
          (get_min_volume (cdr list_of_shapes) min_volume)
        )

      )
    )
  )
)

(define (min_command list_of_shapes)
  (define min_area(get_min_area list_of_shapes '()))
  (define min_volume(get_min_volume list_of_shapes '()))

  (display "min(Surface Area)= ")(display min_area)(newline)
  (display "min(Volume)= ")(display min_volume)(newline)
)

(define (get_max_area list_of_shapes max_area)
  (cond
    ((null? list_of_shapes)
      max_area
    )
    (else
      (cond
        ((or (null? max_area) (> ((car list_of_shapes) 'get_area) max_area))
          (get_max_area (cdr list_of_shapes) ((car list_of_shapes) 'get_area))
        )
        (else
          (get_max_area (cdr list_of_shapes) max_area)
        )

      )
    )
  )
)

(define (get_max_volume list_of_shapes max_volume)
  (cond
    ((null? list_of_shapes)
      max_volume
    )
    (else
      (cond
        ((or (null? max_volume) (> ((car list_of_shapes) 'get_volume) max_volume))
          (get_max_volume (cdr list_of_shapes) ((car list_of_shapes) 'get_volume))
        )
        (else
          (get_max_volume (cdr list_of_shapes) max_volume)
        )

      )
    )
  )
)

(define (max_command list_of_shapes)
  (define max_area(get_max_area list_of_shapes '()))
  (define max_volume(get_max_volume list_of_shapes '()))

  (display "max(Surface Area)= ")(display max_area)(newline)
  (display "max(Volume)= ")(display max_volume)(newline)
)

(define (get_total_area list_of_shapes total_area)
  (cond
    ((null? list_of_shapes)
      total_area
    )
    (else
      (get_total_area (cdr list_of_shapes) (+ total_area ((car list_of_shapes) 'get_area)))
    )
  )
)

(define (get_total_volume list_of_shapes total_volume)
  (cond
    ((null? list_of_shapes)
      total_volume
    )
    (else
      (get_total_volume (cdr list_of_shapes) (+ total_volume ((car list_of_shapes) 'get_volume)))
    )
  )
)

(define (total_command list_of_shapes)
  (define total_area(round-off (get_total_area list_of_shapes 0) 2))
  (define total_volume(round-off (get_total_volume list_of_shapes 0) 2))

  (display "total(Surface Area)= ")(display total_area)(newline)
  (display "total(Volume)= ")(display total_volume)(newline)
)

(define (avg_command list_of_shapes)
  (define total_area(get_total_area list_of_shapes 0))
  (define total_volume(get_total_volume list_of_shapes 0))
  (define count(get_count list_of_shapes 0))

  (define avg_area(round-off (/ total_area count) 2))
  (define avg_volume(round-off (/ total_volume count) 2))

  (display "avg(Surface Area)= ")(display avg_area)(newline)
  (display "avg(Volume)= ")(display avg_volume)(newline)
)

(define (check_test_condition shape_object left_statement operator right_statement)
  (cond
    ((string=? left_statement "area")
      (cond
        ((string=? operator "==")
          (= (shape_object 'get_area) right_statement)
        )
        ((string=? operator "!=")
          (not (= (shape_object 'get_area) right_statement))
        )
        ((string=? operator ">")
          (> (shape_object 'get_area) right_statement)
        )
        ((string=? operator "<")
          (< (shape_object 'get_area) right_statement)
        )
        ((string=? operator ">=")
          (>= (shape_object 'get_area) right_statement)
        )
        ((string=? operator "<=")
          (<= (shape_object 'get_area) right_statement)
        )
      )
    )
    ((string=? left_statement "volume")
      (cond
        ((string=? operator "==")
          (= (shape_object 'get_volume) right_statement)
        )
        ((string=? operator "!=")
          (not (= (shape_object 'get_volume) right_statement))
        )
        ((string=? operator ">")
          (> (shape_object 'get_volume) right_statement)
        )
        ((string=? operator "<")
          (< (shape_object 'get_volume) right_statement)
        )
        ((string=? operator ">=")
          (>= (shape_object 'get_volume) right_statement)
        )
        ((string=? operator "<=")
          (<= (shape_object 'get_volume) right_statement)
        )
      )
    )
    (else
      (cond
        ((string=? operator "==")
          (string=? (shape_object 'get_type) right_statement)
        )
        ((string=? operator "!=")
          (not (string=? (shape_object 'get_type) right_statement))
        )
        ((string=? operator ">")
          (string>? (shape_object 'get_type) right_statement)
        )
        ((string=? operator "<")
          (string<? (shape_object 'get_type) right_statement)
        )
        ((string=? operator ">=")
          (string>=? (shape_object 'get_type) right_statement)
        )
        ((string=? operator "<=")
          (string<=? (shape_object 'get_type) right_statement)
        )
      )
    )
  )
)

(define (check_all_test_condition shape_object test_conditions)
  (cond
    ((null? test_conditions)
      #t
    )
    ((eqv? (check_test_condition shape_object (first test_conditions) (second test_conditions) (third test_conditions)) #f)
      #f
    )
    (else
      (check_all_test_condition shape_object (cdr (cdr (cdr test_conditions))))
    )
  )
)

(define (filter_list_of_shapes list_of_shapes test_conditions filter_list)
  (cond
    ((or (null? list_of_shapes) (null? test_conditions))
      filter_list
    )
    (else
      (cond
        ((eqv? (check_all_test_condition (car list_of_shapes) test_conditions) #t)
          (filter_list_of_shapes (cdr list_of_shapes) test_conditions (append filter_list (list (car list_of_shapes))))
        )
        (else
          (filter_list_of_shapes (cdr list_of_shapes) test_conditions filter_list)
        )
      )
    )
  )
)

(define (get_filter_list list_of_shapes test_conditions)
      (define valid_test_conditions(is_valid_test_conditions test_conditions))

      (cond
        ((and (eqv? valid_test_conditions #t) (not (null? list_of_shapes)))
          (cond
            ((null? test_conditions)
              list_of_shapes
            )
            (else
              (filter_list_of_shapes list_of_shapes test_conditions '())
            )
          )
        )
        ((eqv? valid_test_conditions #f)
            (display "Incorrect number of arguments.")(newline)
            '()
        )
    )
)

(define (perform . params)
    (define command(first params))
    (define file_name(second params))
    (define lines (read_file file_name))
    (define list_of_shapes(create_list_of_shapes lines '()))
    (define test_conditions(cdr  (cdr params)))
    (define filter_list(get_filter_list list_of_shapes test_conditions))

    (define valid_file(is_file_valid file_name))
    (define valid_test_conditions (is_valid_test_conditions test_conditions))
    (cond
      ((and (string=? command "print") (eqv? valid_file #t) (eqv? valid_test_conditions #t))
        (print_command filter_list)
      )
      ((and (string=? command "count") (eqv? valid_file #t) (eqv? valid_test_conditions #t))
        (count_command filter_list)
      )
      ((and (string=? command "min") (eqv? valid_file #t) (eqv? valid_test_conditions #t))
        (min_command filter_list)
      )
      ((and (string=? command "max") (eqv? valid_file #t) (eqv? valid_test_conditions #t))
        (max_command filter_list)
      )
      ((and (string=? command "total") (eqv? valid_file #t) (eqv? valid_test_conditions #t))
        (total_command filter_list)
      )
      ((and (string=? command "avg") (eqv? valid_file #t) (eqv? valid_test_conditions #t))
        (avg_command filter_list)
      )
      (else 'done)
    )
    (newline)
    
)
