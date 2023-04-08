(load "shapes.scm")
(define new_sphere(sphere "globe" 1))

(define area(new_sphere 'get_area))
(display "surface area: ")(display area)(newline)

(define volume(new_sphere 'get_volume))
(display "volume: ")(display volume)(newline)

(define info(new_sphere 'get_info))

(display info)

(define is_valid(test eqv? (new_sphere 'get_area) 12.57))
(display is_valid)(newline)

(define is_valid(test eqv? (new_sphere 'get_volume) 12.57))
(display is_valid)(newline)

(define s1(list (sphere "globe1" 1)))
(define s2(list (sphere "globe2" 2)))


(append! s1 s2)

(display s1)
