(load "shapes.scm")
(perform "print" "DATA1" ">=" 0)
(perform "print" "No_Existing_File")
(perform "print" "DATA1")
(perform "print" "DATA2")
(perform "print" "DATA3")
(perform "print" "DATA4")
(perform "print" "DATA5" "area" "!=" 13 "volume" "!=" 19)
(perform "count" "DATA5" "type" ">" "cyl" "type" "!=" "box" "area" ">" 15 "volume" "<" 100)
(perform "min" "DATA5" "type" "==" "box" "volume" "<=" 48)
(perform "max" "DATA5" "type" "==" "box" "volume" "!=" 48)
(perform "total" "DATA5" "type" "<=" "torus")
(perform "avg" "DATA5" "volume" "==" 48)