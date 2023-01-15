(defpackage #:masterblaster
  (:use #:cl))

(ql:quickload "sdl2")
(ql:quickload "cl-portaudio")

(load "font.lisp")
(load "sound.lisp")

(defconstant +SCREEN-WIDTH+ 320)
(defconstant +SCREEN-HEIGHT+ 240)
(defconstant +CHAR-WIDTH+ 8)
(defconstant +CHAR-HEIGHT+ 8)

(defparameter *bitmap-font* (gethash :CHARBITS (load-bdf-file "./Area51.bdf")))
(defparameter *window* nil)
(defparameter *renderer* nil)
(defparameter *window-zoom-level* 2)

(defun display-draw-point (x y)
  (let ((xp (* *window-zoom-level* x))
        (yp (* *window-zoom-level* y)))
    (when (not (or (< x 0) (< y 0) (> x +screen-width+) (> y +screen-height+)))
      (loop for xd from 0 to (1- *window-zoom-level*) do
        (loop for yd from 0 to (1- *window-zoom-level*) do
          (sdl2:render-draw-point *renderer* (+ xp xd) (+ yp yd)))))
    ))

;; (defun display-draw-line (x1 y1 x2 y2)
;;   (sdl2:render-draw-line *renderer* x1 y1 x2 y2))

(defun display-draw-line (x0 y0 x1 y1)
  "Draw a line using display-draw-point & Bresenham's algorithm"
  (let* ((dx (abs (- x1 x0)))
         (dy (abs (- y1 y0)))
         (sx (if (< x0 x1) 1 (if (> x0 x1) -1 -1)))
         (sy (if (< y0 y1) 1 (if (> y0 y1) -1 -1)))
         (err (- dy dx))
         e2)
    (loop
       (display-draw-point x0 y0)
       (when (and (= x0 x1) (= y0 y1)) (return))
       (setq e2 (ash err 1))
       (when (>= e2 (- dy))
         (when (= x0 x1) (return))
         (decf err dy)
         (incf x0 sx))
       (when (<= e2 dx)
         (when (= y0 y1) (return))
         (incf err dx)
         (incf y0 sy)))))
(display
 (lambda () (display-draw-line (/ +screen-width+ 2) 0 (/ +screen-width+ 2) +screen-height+)))

(defun display-draw-character (posx posy charnum)
  (when (or (> charnum (length *bitmap-font*)) (< charnum 0))
    (error "Character out of range"))
  ;; TODO replace charnum with character?
  (let ((bits (aref *bitmap-font* charnum)))
    (loop for i from 0 to (- (* +CHAR-WIDTH+ +CHAR-HEIGHT+) 1)
          do (let* ((y (floor i +CHAR-WIDTH+))
                    (x (- i (* +CHAR-WIDTH+ y))))
               (if (= 1 (aref bits i))
                   (display-draw-point (+ posx x) (+ posy  y)))))))

(defun charcode-from-string (str i)
  (char-code (schar str i)))

(defun display-draw-string (posx posy s)
  (loop for i from 0 to (- (length s) 1)
        do (display-draw-character (+ posx (* i +CHAR-WIDTH+)) posy (charcode-from-string s i))))

(defun display-init ()
  (sdl2:with-init (:everything)
    (format t "Using SDL Library Version: ~D.~D.~D~%"
            sdl2-ffi:+sdl-major-version+
            sdl2-ffi:+sdl-minor-version+
            sdl2-ffi:+sdl-patchlevel+)
    (finish-output)

    (let ((width (* +screen-width+ *window-zoom-level*))
          (height (* +screen-height+ *window-zoom-level*)))
      (multiple-value-bind (window renderer) (sdl2:create-window-and-renderer width height '(:opengl))
        (let ((tex (sdl2:create-texture renderer sdl2-ffi:+sdl-pixeltype-unknown+ sdl2-ffi:+sdl-textureaccess-streaming+ width height)))
          (let ((w (sdl2:texture-width tex))
                (h (sdl2:texture-height tex)))
            (print tex)
            (print w)
            (print h)))
        (setf *window* window)
        (setf *renderer* renderer)))

    ;; start event loop
    (sdl2:with-event-loop (:method :poll)
      ;; ESC will exit
      (:keyup (:keysym keysym)
              (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
                (sdl2:push-event :quit)))
      ;; re-renders / blit texture goes here
      ;;         (:idle ()
      ;;                )
      (:quit () (progn
                  (setf *window* nil)
                  (setf *renderer* nil)
                  t)))))

(defun display-clear (&optional (r 255) (g 255) (b 255) (a 255))
  (sdl2:set-render-draw-color *renderer* r g b a)
  (sdl2:render-clear *renderer*))

(defun display-set-draw-color (r g b a)
  (sdl2:set-render-draw-color *renderer* r g b a))

(defun display (fn)
  (when (not *window*)
    (display-init))
  (funcall fn)
  (sdl2:render-present *renderer*))

(defun launch ()
  "Open our first window and draw some pixels"
  (display
   (lambda ()
     (display-clear 0 0 0 0)
     (display-set-draw-color 255 255 255 255)

     (loop for c from 0 to (1- (length *bitmap-font*)) do
       (let* ((y (floor c 40))
              (x (- c (* 40 y))))
         (print (list  x y c))
         (display-draw-character (* x 8) (* y 8) c)))
     (display-draw-string 200 200 "Hello World!")
     (display-draw-line 0 0 +screen-width+ +screen-height+)
     (display-draw-line +screen-width+ 0 0 +screen-height+)
     (display-draw-line (/ +screen-width+ 2) 0 (/ +screen-width+ 2) +screen-height+)
     )))


(defun axis-translator (x11 x12 x21 x22)
  (let* ((x (/ (- x21 x22) (- x11 x12)))
         (b (- x22 (* x12 x))))
    (lambda (i)
      (+ (* i x) b))))

(= (funcall (axis-translator -1 1 240 0) -1) 240)
(= (funcall (axis-translator -1 1 240 0) 1) 0)
(= (funcall (axis-translator -1 1 240 0) 0) 120)
(= (funcall (axis-translator -1 1 240 0) 2) -120)

(= (funcall (axis-translator 0 2 160 320) 0) 160)
(= (funcall (axis-translator 0 2 160 320) 2) 320)
(= (funcall (axis-translator -1 1 0 320) -1) 0)

(= (funcall (axis-translator -1 1 0 320) 1) 320)

(defun plot (fn &optional (x1 -1.0) (x2 1.0) (y1 -1.0) (y2 1.0) (aspoints nil))
  (display
   (lambda ()
     (display-clear 0 0 0 0)
     (display-set-draw-color 255 255 255 255)

     (let* ((tn-x-inverse (axis-translator 0 +SCREEN-WIDTH+ x1 x2))
            (tn-y (axis-translator y1 y2 +SCREEN-HEIGHT+ 0))
            (tn-x (axis-translator x1 x2 0 +SCREEN-WIDTH+))
            (origin-x (floor (funcall tn-x 0)))
            (origin-y (floor (funcall tn-y 0)))
            (oldy nil)
            (x 0))
       ;; draw the origin
       (display-set-draw-color 255 255 0 255)
       (display-draw-line 0 origin-y +SCREEN-WIDTH+ origin-y)
       (display-draw-line origin-x 0 origin-x +SCREEN-HEIGHT+)

       ;; plot the fn
       (loop while (<= x +SCREEN-WIDTH+) do
         (let* ((origx (funcall tn-x-inverse x))
                (origy (funcall fn origx))
                (y (floor (funcall tn-y origy))))
           ;; (format t "~d,~d -> ~d,~d~%" origx origy x y)
           (display-set-draw-color 255 255 255 255)
           (if aspoints
               (display-draw-point x y)
               (display-draw-line (1- x) (if (not oldy) y oldy) x y))
           (setf oldy y)
           (incf x 1)))))))

(plot (lambda (x) (sin x)) 0 (* 2 PI) -1 1)
(plot (lambda (x) (cos x)) 0 (* 2 PI))


;; (plot (buffer->fn test-buffer) 0 (/ 44100 2))

;; (defun distance1d (x1 x2)
;;   (abs (- x1 x2)))

;; (defun midpoint (x1 x2)
;;   (let ((distance (distance1d x1 x2)))
;;     (if (> x1 x2)
;;         (- x1 (/ distance 2))
;;         (- x2 (/ distance 2)))))


;; (setf *window* nil)
(launch)

;; (display-draw-point 2 2)
;; (sdl2:render-present *renderer*)





