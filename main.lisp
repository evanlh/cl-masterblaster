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

(defun display-draw-point (x y)
  (sdl2:render-draw-point *renderer* x y))

(defun display-draw-line (x1 y1 x2 y2)
  (sdl2:render-draw-line *renderer* x1 y1 x2 y2))

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

    (multiple-value-bind (window renderer) (sdl2:create-window-and-renderer +screen-width+ +screen-height+ '(:opengl))
      (let ((tex (sdl2:create-texture renderer sdl2-ffi:+sdl-pixeltype-unknown+ sdl2-ffi:+sdl-textureaccess-streaming+ +screen-width+ +screen-height+)))
        (let ((w (sdl2:texture-width tex))
              (h (sdl2:texture-height tex)))
          (print tex)
          (print w)
          (print h)))
      (setf *window* window)
      (setf *renderer* renderer))

    ;; start event loop
    (sdl2:with-event-loop (:method :poll)
      ;; ESC will exit
      (:keyup (:keysym keysym)
              (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
                (sdl2:push-event :quit)))
      ;; re-renders / blit texture goes here
      ;;         (:idle ()
      ;;                )
      (:quit () t))))

(defun display-clear (&optional (r 255) (g 255) (b 255) (a 255))
  (sdl2:set-render-draw-color *renderer* r g b a)
  (sdl2:render-clear *renderer*))

(defun display-set-draw-color (r g b a)
  (sdl2:set-render-draw-color *renderer* r g b a))

(defun display-draw-point (x y)
  (sdl2:render-draw-point *renderer* 100 100))

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

     (loop for c from 0 to (- (length *bitmap-font*) 1) do
       (let* ((y (floor c 40))
              (x (- c (* 40 y))))
         (print (list  x y c))
         (display-draw-character (* x 8) (* y 8) c)))
     (display-draw-string 200 200 "Hello World!"))))


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

(plot (envelope-interpolator
       (make-envelope 0.0 0.0 0.5 1.0 1.0 0.0) :loop-p nil :loop-start 0.25 :loop-end 0.75))

(plot (envelope-interpolator (make-envelope 0.0 0.0 0.5 1.0 1.0 0.0)))

(plot (buffer-as-function test-buffer) 0 (/ 44100 2))

;; (defun distance1d (x1 x2)
;;   (abs (- x1 x2)))

;; (defun midpoint (x1 x2)
;;   (let ((distance (distance1d x1 x2)))
;;     (if (> x1 x2)
;;         (- x1 (/ distance 2))
;;         (- x2 (/ distance 2)))))



(launch)
