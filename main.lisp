(defpackage #:masterblaster
  (:use #:cl))

(ql:quickload "sdl2")
(ql:quickload "cl-portaudio")
(ql:quickload "series")

;; (ql:quickload "fset")

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

(defparameter *keymap* (make-hash-table))
(defparameter *keymap-events-list* '(:left :right :up :down :mod1-left :mod1-right :mod1-up :mod1-up :mod2-left :mod2-right :mod2-down :mod2-up :mod3-left :mod3-right :mod3-up :mod3-down :select :cancel :play))

(defvar *current-draw-color* (list 255 255 255 255))
(defconstant +color-white+ (list 255 255 255 255))
(defconstant +color-black+ (list 0 0 0 255))
(defconstant +color-silver+ (list #xc0 #xc0 #xc0 #xff))
(defconstant +color-grey+ (list #x80 #x80 #x80 #xff))
(defconstant +color-darkgrey+ (list #x30 #x30 #x30 #xff))
(defconstant +color-yellow+ (list #xff #xff 0 #xff))

;; EVENTS

(defun dispatch-key (key)
  (let ((fn (gethash key *keymap*)))
    (format t "~s~%" key)
    (when fn (funcall fn))))

;; TODO could maybe be a macro?
(defun keymap-update (plist)
  (dotimes (i (/ (length plist) 2))
    (let ((k (nth (* i 2) plist))
          (v (nth (1+ (* i 2)) plist)))
      (assert (typep v 'function))
      (setf (gethash k *keymap*) v))))

(defun keymap-clear () (clrhash *keymap*))

(keymap-update (list :left (lambda () (format t "left via keymap~%"))))


;;;; DISPLAY

(defun display-set-draw-color (r g b a)
  (declare (number r g b a))
  (sdl2:set-render-draw-color *renderer* r g b a)
  (setf (first *current-draw-color*) r
        (second *current-draw-color*) g
        (third *current-draw-color*) b
        (fourth *current-draw-color*) a))

;; TODO should this be a macro?
(defun display-set-draw-color-list (l)
  (display-set-draw-color (first l) (second l) (third l) (fourth l)))

(defun display-draw-point (x y)
  (let ((xp (* *window-zoom-level* x))
        (yp (* *window-zoom-level* y)))
    (when (not (or (< x 0) (< y 0) (> x +screen-width+) (> y +screen-height+)))
      (loop for xd from 0 to (1- *window-zoom-level*) do
        (loop for yd from 0 to (1- *window-zoom-level*) do
          (sdl2:render-draw-point *renderer* (+ xp xd) (+ yp yd)))))))

(defun display-draw-vline (x y0 y1)
  (let ((y y0)
        (incby (if (< y0 y1) 1 -1)))
    (loop
       (display-draw-point x y)
       (when (= y y1) (return))
       (incf y incby))))

(defun display-draw-hline (y x0 x1)
  (let ((x x0)
        (incby (if (< x0 x1) 1 -1)))
    (loop
       (display-draw-point x y)
       (when (= x x1) (return))
       (incf x incby))))

;; mostly ported from plotLine @ https://en.wikipedia.org/wiki/Bresenham's_line_algorithm
;; augmented with display-draw-vline/display-draw-hline calls
(defun display-draw-line (x0 y0 x1 y1)
  "Draw a line using display-draw-point & Bresenham's algorithm"
  (if (= x0 x1) (return-from display-draw-line (display-draw-vline x0 y0 y1)))
  (if (= y0 y1) (return-from display-draw-line (display-draw-hline y0 x0 x1)))
  (let* ((dx (abs (- x1 x0)))
         (dy (abs (- y1 y0)))
         (sx (if (< x0 x1) 1 -1))
         (sy (if (< y0 y1) 1 -1))
         (err (- dy dx))
         e2)
    (loop
       (display-draw-point x0 y0)
       (when (and (= x0 x1) (= y0 y1)) (return))
       (setq e2 (ash err 1))
       (when (>= e2 (- dy))
         (when (= x0 x1) (return (display-draw-vline x0 y0 y1)))
         (decf err dy)
         (incf x0 sx))
       (when (<= e2 dx)
         (when (= y0 y1) (return (display-draw-hline y0 x0 x1)))
         (incf err dx)
         (incf y0 sy)))))

(defun display-draw-fill-rect (x0 y0 x1 y1)
  (assert (and (typep x0 'integer) (typep y0 'integer)
               (typep x1 'integer) (typep y1 'integer)))
  (loop for i from x0 to x1 do
    (loop for j from y0 to y1 do
      (display-draw-point i j))))

(defun display-draw-border-rect (x0 y0 x1 y1)
  (assert (and (typep x0 'integer) (typep y0 'integer)
               (typep x1 'integer) (typep y1 'integer)))
  (display-draw-vline x0 y0 y1)
  (display-draw-vline x1 y0 y1)
  (display-draw-hline y0 x0 x1)
  (display-draw-hline y1 x0 x1))

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
      ;; re-renders / blit texture goes here TODO this crashes
      ;; (:idle ()
      ;;        (sdl2:render-present *renderer*))
      (:keydown
       (:keysym keysym)
       (let ((scancode (sdl2:scancode-value keysym))
             (sym (sdl2:sym-value keysym))
             (mod (sdl2:mod-value keysym)))
         (cond
           ((and (sdl2:scancode= scancode :scancode-left) (= mod 1)) (dispatch-key :mod1-left))
           ((and (sdl2:scancode= scancode :scancode-right) (= mod 1)) (dispatch-key :mod1-right))
           ((and (sdl2:scancode= scancode :scancode-down) (= mod 1)) (dispatch-key :mod1-down))
           ((and (sdl2:scancode= scancode :scancode-up) (= mod 1)) (dispatch-key :mod1-up))

           ((and (sdl2:scancode= scancode :scancode-left) (= mod 256)) (dispatch-key :mod2-left))
           ((and (sdl2:scancode= scancode :scancode-right) (= mod 256)) (dispatch-key :mod2-right))
           ((and (sdl2:scancode= scancode :scancode-down) (= mod 256)) (dispatch-key :mod2-down))
           ((and (sdl2:scancode= scancode :scancode-up) (= mod 256)) (dispatch-key :mod2-up))

           ((and (sdl2:scancode= scancode :scancode-left) (= mod 1024)) (dispatch-key :mod3-left))
           ((and (sdl2:scancode= scancode :scancode-right) (= mod 1024)) (dispatch-key :mod3-right))
           ((and (sdl2:scancode= scancode :scancode-down) (= mod 1024)) (dispatch-key :mod3-down))
           ((and (sdl2:scancode= scancode :scancode-up) (= mod 1024)) (dispatch-key :mod3-up))

           ((sdl2:scancode= scancode :scancode-left) (dispatch-key :left))
           ((sdl2:scancode= scancode :scancode-right) (dispatch-key :right))
           ((sdl2:scancode= scancode :scancode-down) (dispatch-key :down))
           ((sdl2:scancode= scancode :scancode-up) (dispatch-key :up))

           ((sdl2:scancode= scancode :scancode-return) (dispatch-key :select))
           ((sdl2:scancode= scancode :scancode-backspace) (dispatch-key :cancel))
           ((sdl2:scancode= scancode :scancode-space) (dispatch-key :play))
           )
         ;; (format t "Key sym: ~a, code: ~a, mod: ~a~%" sym scancode mod)
         ))
      (:quit () (progn
                  (setf *window* nil)
                  (setf *renderer* nil)
                  t)))))

(defun display-clear (&optional (r 255) (g 255) (b 255) (a 255))
  (display-set-draw-color r g b a)
  (sdl2:render-clear *renderer*))

(defun display (fn)
  (when (not *window*)
    (display-init))
  (funcall fn)
  (sdl2:render-present *renderer*))


(defun display-if (predfn dispatch-on-true-fn)
  (lambda ()
    (when (funcall predfn) (display dispatch-on-true-fn))))

(defun display-component (fn)
  (let* ((component-plist (funcall fn))
         ;; grab the display fn and remove it from the plist
         (displayfn (getf component-plist :display)))
    (remf component-plist :display)
    ;; all that remains are the event bindings
    (keymap-update
     (loop for (k v) on component-plist
           append (list k (display-if v displayfn))))
    (display displayfn)))


;;; LAUNCH & PLOT
(defun launch ()
  "Open our first window and draw some pixels"
  (display
   (lambda ()
     (display-clear 0 0 0 0)
     (display-set-draw-color-list +color-white+)

     (loop for c from 0 to (1- (length *bitmap-font*)) do
       (let* ((y (floor c 40))
              (x (- c (* 40 y))))
         (display-draw-character (* x 8) (* y 8) c)))
     (display-set-draw-color-list +color-yellow+)
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


;; (setf *window* nil)
(launch)


