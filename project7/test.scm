(load "shapes.scm")
(define new_sphere(sphere "globe" 1))

(define area(new_sphere 'get_area))
(display "surface area: ")(display area)(newline)

(define volume(new_sphere 'get_volume))
(display "volume: ")(display volume)(newline)
