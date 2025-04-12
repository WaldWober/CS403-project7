;Constants
(define pi 3.14159265358979323846)

;-----Box logic-----

;create box
(define (initialize-box name length width height)
    (list name 'box (list length width height)))

;Get statements for box dimensions
(define (box-length box) (car (caddr box))) ; the first item of the dimensions list
(define (box-width box) (cadr (caddr box))) ; the second item of the dimensions list
(define (box-height box) (caddr (caddr box))) ; the third item of the dimensions list

;Box area and volume
(define (get-box-area box)
    (let ((l (box-length box))
        (w (box-width box))
        (h (box-height box)))
        (* 2 (+ (* l w) (* l h) (* w h)))
    )
)

(define (get-box-volume box)
    (let ((l (box-length box))
        (w (box-width box))
        (h (box-height box)))
        (* l w h)
    )
)

;-----Torus logic-----
(define (initialize-torus name small_radius big_radius)
    (list name 'torus (list small_radius big_radius))
)

;Get statements for torus dimensions
(define (torus-small-radius torus) (car (caddr torus))) ; the first item of the dimensions list
(define (torus-big-radius torus) (cadr (caddr torus))) ; the second item of the dimensions list

;Torus area and volume
(define (get-torus-area torus)
    (let ((s (torus-small-radius torus))
        (b (torus-big-radius torus)))
        (* (* 2 pi b) (* 2 pi s))
    )
)

(define (get-torus-volume torus)
    (let ((s (torus-small-radius torus))
        (b (torus-big-radius torus)))
        (* (* pi s s) (* 2 pi b))
    )
)

;-----Cylinder logic-----
(define (initialize-cylinder name radius height)
    (list name 'cylinder (list radius height))
)

;Get statements for cylinder dimensions
(define (cylinder-radius cylinder) (car (caddr cylinder))) ; the first item of the dimensions list
(define (cylinder-height cylinder) (cadr (caddr cylinder))) ; the second item of the dimensions list

;Cylinder area and volume
(define (get-cylinder-area cylinder)
    (let ((r (cylinder-radius cylinder))
        (h (cylinder-height cylinder)))
        (+ (* 2 pi r h) (* 2 pi r r))
    )
)

(define (get-cylinder-volume cylinder)
    (let ((r (cylinder-radius cylinder))
        (h (cylinder-height cylinder)))
        (* pi r r h)
    )
)

;-----Sphere logic-----
(define (initialize-sphere name radius)
    (list name 'sphere (list radius))
)

;Get statements for sphere dimensions
(define (sphere-radius sphere) (car (caddr sphere))) ; the first item of the dimensions list

;Sphere area and volume
(define (get-sphere-area sphere)
    (let ((r (sphere-radius sphere)))
        (* 4 pi r r)
    )
)

(define (get-sphere-volume sphere)
    (let ((r (sphere-radius sphere)))
        (* (/ 4 3) pi r r r)
    )
)

;-----Generic Helper Functions
(define (get-surface-area shape)
    (let ((type (cadr shape)))
        (cond ((eq? type 'box) (get-box-area shape))
            ((eq? type 'torus) (get-torus-area shape))
            ((eq? type 'cylinder) (get-cylinder-area shape))
            ((eq? type 'sphere) (get-sphere-area shape))
        )
    )
)

(define (get-volume shape)
    (let ((type (cadr shape)))
        (cond ((eq? type 'box) (get-box-volume shape))
            ((eq? type 'torus) (get-torus-volume shape))
            ((eq? type 'cylinder) (get-cylinder-volume shape))
            ((eq? type 'sphere) (get-sphere-volume shape))
        )
    )
)


;-----File Parsing-----
(define (read-file filename)
    (let ((port (open-input-file filename)))
        (let loop ((shapes '()) ;creates a loop with an empty shapes list
               (line (read-line port))) ;read in a single line
      (if (eof-object? line) ;if end of file, close the file and return shapes
          (begin
            (close-input-port port)
            (reverse shapes)
            ) 
          (loop (cons (parse-line (str-split line)) shapes)
                (read-line port)))))
)

(define (parse-line tokens)
    (let ((name (car tokens))
        (type (string->symbol (cadr tokens)))
        (dims (map string->number (cddr tokens))))
        
        (case type
            ((box)
                (initialize-box name (car dims) (cadr dims) (caddr dims))
            )
            ((torus)
                (initialize-torus name (car dims) (cadr dims))
            )
            ((cylinder)
                (initialize-cylinder name (car dims) (cadr dims))
            )
            ((sphere)
                (initialize-sphere name (car dims))
            )
        )
    )
)

;taken from my-str-split
(define (str-split-helper line str list)
    (cond
        ((string-null? line)
            (if (string-null? str)
		(reverse list)
		(reverse (cons str list))))
        ((char=? (string-ref line 0) #\space)
            (if (string-null? str)
                (str-split-helper (string-tail line 1) str list)
                (str-split-helper (string-tail line 1) "" (cons str list))))
        (else
            (str-split-helper (string-tail line 1)
                              (string-append str (string-head line 1))
                              list))))

(define (str-split line) (str-split-helper line "" '()))
;-----Actions-----

(define (count shapes)
    (cond 
    ((= (length shapes) 0)
        (display "There are no shapes.")
        (newline)
        '())
    (else 
        (display "There are ")
        (display (number->string (length shapes)))
        (display " shapes.")
        (newline)
        '())
    )
)
(define (print shapes)
    (for-each (lambda (shape)
        (let ((type (cadr shape)))
            (case type
                ((box)
                    (display "Box: ")
                    (display (car shape))
                    (display ", Length=")
                    (display (box-length shape))
                    (display ", Width=")
                    (display (box-width shape))
                    (display ", Height= ")
                    (display (box-width shape))
                )
                ((torus)
                    (display "Torus: ")
                    (display (car shape))
                    (display ", Small Radius=")
                    (display (torus-small-radius shape))
                    (display ", Big Radius=")
                    (display (torus-big-radius shape))
                )
                ((cylinder)
                    (display "Cylinder: ")
                    (display (car shape))
                    (display ", Radius=")
                    (display (cylinder-radius shape))
                    (display ", Height=")
                    (display (cylinder-height shape))
                )
                ((sphere)
                    (display "Sphere: ")
                    (display (car shape))
                    (display ", Radius=")
                    (display (sphere-radius shape))
                )
            )
        )

        (newline)
        (display "Surface Area: ")
        (display (get-surface-area shape))
        (display ", Volume: ")
        (display (get-volume shape))
        (newline)
        ) 
    shapes)
)
(define (minimum shapes)
    (if (null? shapes) ;print if there are no shapes in the list
        (begin
            (display "There are no shapes satisfying the condition(s)")
            (newline))
        (let loop ((remaining (cdr shapes)) ;loop through shapes to find minimum. sets 'remaining' to every shape after this one
                    (min-area (get-surface-area (car shapes))) ;initial minimums are first shape's values
                    (min-volume (get-volume (car shapes))))
            
            (if (null? remaining) ;base case
                (begin
                (display "min(Surface Area)=")
                (display min-area)
                (newline)
                (display "min(Volume)=")
                (display min-volume)
                (newline))
                (let* ((current (car remaining)) ;set current shape to next shape in the list
                        (current-area (get-surface-area current))
                        (current-volume (get-volume current)) 
                        (new-min-area (if (< current-area min-area) current-area min-area)) ;if current area is less than minimum area, current area is new minimum area. otherwise, new minimum area is minimum area
                        (new-min-volume (if (< current-volume min-volume) current-volume min-volume))
                        )
                    (loop (cdr remaining) new-min-area new-min-volume))))))

(define (maximum shapes)
    (if (null? shapes) ;print if there are no shapes in the list
        (begin
            (display "There are no shapes satisfying the condition(s)")
            (newline))
        (let loop ((remaining (cdr shapes)) ;loop through shapes to find maximum. sets 'remaining' to every shape after this one
                    (max-area (get-surface-area (car shapes))) ;initial maximum are first shape's values
                    (max-volume (get-volume (car shapes))))
            
            (if (null? remaining) ;base case
                (begin
                (display "max(Surface Area)=")
                (display max-area)
                (newline)
                (display "max(Volume)=")
                (display max-volume)
                (newline))
                (let* ((current (car remaining)) ;set current shape to next shape in the list
                        (current-area (get-surface-area current))
                        (current-volume (get-volume current)) 
                        (new-max-area (if (> current-area max-area) current-area max-area)) ;if current area is less than maximum area, current area is new maximum area. otherwise, new maximum area is maximum area
                        (new-max-volume (if (> current-volume max-volume) current-volume max-volume))
                        )
                    (loop (cdr remaining) new-max-area new-max-volume))))))

(define (total-helper shapes)
    (if (null? shapes) ;return a special symbol if there are no shapes
        (begin
            'empty 
            )
        (let loop ((remaining (cdr shapes)) ;loop through shapes to grow the total. sets 'remaining' to every shape after this one
                    (total-area (get-surface-area (car shapes)))
                    (total-volume (get-volume (car shapes))))
            
            (if (null? remaining) ;base case. return list of total-area and total-volume
                (list total-area total-volume)

                (let* ((current (car remaining)) ;set current shape to next shape in the list
                        (current-area (get-surface-area current))
                        (current-volume (get-volume current)) 
                        (new-total-area (+ total-area current-area)) ;add current shape values to running total
                        (new-total-volume (+ total-volume current-volume))
                        )
                    (loop (cdr remaining) new-total-area new-total-volume))))))

(define (total shapes)
    (let ((totals (total-helper shapes)))
        (if (eq? totals 'empty)
            (begin
                (display "There are no shapes satisfying the condition(s).")
                (newline))
            (begin
                (display "total(Surface Area)=")
                (display (car totals))
                (newline)
                (display "total(Volume)=")
                (display (cadr totals))
                (newline)))))

(define (avg shapes)
    (let ((totals (total-helper shapes)))
        (if (eq? totals 'empty)
            (begin
                (display "There are no shapes satisfying the condition(s).")
                (newline))
            (begin
                (let ((avg-area (/ (car totals) (length shapes)))
                        (avg-volume (/ (cadr totals) (length shapes))))
        
                    (display "avg(Surface Area)=")
                    (display avg-area)
                    (newline)
                    (display "avg(Volume)=")
                    (display avg-volume)
                    (newline))))))

;-----Condition Handling andPerform------
;(newline)
;(display "All shapes:")
;(newline)
;(for-each (lambda (shape)
;            (display-shape shape)
;            (newline))
;          shapes)

(define (filter-shapes shapes conditions)
    (if (null? conditions)
    shapes ;base case, when no conditions remain
    (let* ((name (car conditions)) ;extracts set of name, op, value from conditions
            (op (cadr conditions))
            (val (caddr conditions))
            (remaining (cdddr conditions)) ; stores remaining conditions
            (filtered-shapes (filter (lambda (shape) (test-shape shape name op val)) shapes))) ;sets filtered-shapes as a filtering of each shape based on passing or failing 'test-shape' with a given set of conditions
            
        (filter-shapes filtered-shapes remaining)))) ;recur

(define (test-shape shape name op val)
    (let ((type (symbol->string (cadr shape)))
            (area (get-surface-area shape))
            (volume (get-volume shape)))

        (cond 
            ((string=? name "type")
                (cond
                    ((string=? op "==") (string=? type val))
                    ((string=? op "!=") (not (string=? type val)))
                    ((string=? op ">") (string>? type val))
                    ((string=? op "<") (string<? type val))
                    ((string=? op ">=") (string>=? type val))
                    ((string=? op "<=") (string<=? type val))))

            ((string=? name "area")
                (cond
                    ((string=? op "==") (equal? area val))
                    ((string=? op "!=") (not (equal? area val)))
                    ((string=? op ">") (> area val))
                    ((string=? op "<") (< area val))
                    ((string=? op ">=") (>= area val))
                    ((string=? op "<=") (<= area val))))

            ((string=? name "volume")
                (cond
                    ((string=? op "==") (equal? volume val)))
                    ((string=? op "!=") (not (equal? volume val)))
                    ((string=? op ">") (> volume val))
                    ((string=? op "<") (< volume val))
                    ((string=? op ">=") (>= volume val))
                    ((string=? op "<=") (<= volume val))))))

(define (perform action filename . conditions)
    (if (file-exists? filename)
        (if (= (remainder (length conditions) 3) 0)
            (let* ((shapes (read-file filename))
                    (filtered-shapes (filter-shapes shapes conditions)))
                (cond ((string=? action "count") (count filtered-shapes))
                    ((string=? action "print") (print filtered-shapes))
                    ((string=? action "min") (minimum filtered-shapes))
                    ((string=? action "max") (maximum filtered-shapes))
                    ((string=? action "total") (total filtered-shapes))
                    ((string=? action "avg") (avg filtered-shapes)))
                (newline))

            (begin
                (display "Incorrect number of arguments.")
                (newline)
                (newline)))

    (begin
        (display "Unable to open ")
        (display filename)
        (display " for reading.")
        (newline)
        (newline))))

;(perform "count" "shapes.dat")
;(perform "print" "shapes.dat")
;(perform "min" "shapes.dat")
;(perform "max" "shapes.dat")
;(perform "print" "shapes.dat")
;(perform "total" "shapes.dat")
;(perform "avg" "shapes.dat")



