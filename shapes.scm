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


;-----File Parsing and Shape Display-----
(define (read-file filename)
    (if (file-exists? filename)
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
    
    (begin
        (display "Unable to open ")
        (display filename)
        (display " for reading.")
        (newline)
        '()))
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
        


(define (display-shape shape)
  (display "Shape: ")
  (display (car shape))
  (display " (Type: ")
  (display (cadr shape))
  (display ")")
  (newline)
  (display "  Dimensions: ")
  (display (caddr shape))
  (newline)
  (display "  Surface Area: ")
  (display (get-surface-area shape))
  (newline)
  (display "  Volume: ")
  (display (get-volume shape))
  (newline))




;Main and testing

;(define cube (initialize-box "Cube" 1 8 5))
;(define ball (initialize-sphere "Ball" 4))
;(define donut (initialize-torus "Donut" 4 8))
;(define can (initialize-cylinder "Can" 1 3))

;(define shapes (list cube ball donut can))

;(newline)
;(display "All shapes:")
;(newline)
;(for-each (lambda (shape)
;            (display-shape shape)
;            (newline))
;          shapes)

(let ((shapes (read-file "shapes.dat")))
    (newline)
    (display "All shapes:")
    (newline)
    (for-each (lambda (shape)
            (display-shape shape)
              (newline))
    shapes)
)