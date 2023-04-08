(defpackage #:masterblaster
  (:use #:cl))

(ql:quickload "sdl2")
(ql:quickload "cl-portaudio")
(ql:quickload "portmidi")
(ql:quickload "series")
(ql:quickload "bordeaux-threads")
(ql:quickload "alexandria")


;; Below doesn't work, or works too well & deferring fix it for now....
(defmacro with-mainthread (form)
  `(progn ,form))
;; configure threads
;; (defconstant +MAIN-THREAD+ (bt:current-thread))
;; (defmacro with-mainthread (form)
;;   `(if (eq (bt:current-thread) +MAIN-THREAD+)
;;        (progn ,form)
;;        (error "This operation should only be executed on the main thread")))


(load "font.lisp")
(load "sound.lisp")
(load "midi.lisp")

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
            (format t "Display: Created texture width: ~d height: ~d~%" w h)))
        (setf *window* window)
        (setf *renderer* renderer)))

    (format t "Display: Entering event loop~%")
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
  (with-mainthread
    (progn
      (when (not *window*)
        (display-init))
      (funcall fn)
      (sdl2:render-present *renderer*))))


(defun display-if (predfn dispatch-on-true-fn)
  (lambda ()
    (when (funcall predfn) (display dispatch-on-true-fn))))

(defun display-component (fn)
  (let* ((component-plist (funcall fn))
         ;; grab the display fn and remove it from the plist
         ;; TODO copy & mutate copy
         (displayfn (getf component-plist :display)))
    (remf component-plist :display)
    ;; all that remains are the event bindings
    (keymap-update
     (loop for (k v) on component-plist
           append (list k (display-if v displayfn))))
    (display-clear 0 0 0 255)
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

;; (launch)

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

;; (plot (lambda (x) (sin x)) 0 (* 2 PI) -1 1)
;; (plot (lambda (x) (cos x)) 0 (* 2 PI))

(defconstant +track-grid-border-padding+ 2)
(defconstant +track-grid-border-thickness+ 1)

;; playing with track UI
(declaim (ftype (function (track fixnum fixnum fixnum fixnum fixnum list list list)) draw-note-track-lane-cell))
(defun draw-note-track-lane-cell (track index xb yb w h bg-color sel-color char-color)
  "Draw a single cell for specified TRACK including the note found at INDEX, at coordinates XB/YB with
  Width and Height. Outer border will be drawn in BG-COLOR, inner border in SEL-COLOR, chars in CHAR-COLOR"
  (display-set-draw-color-list bg-color)
  (display-draw-fill-rect xb yb (+ xb w) (+ yb h))
  (display-set-draw-color-list sel-color)
  (display-draw-border-rect xb yb (+ xb w) (+ yb h))
  (let* ((note (track-get-note track index))
         (note-str (note-string-value note)))
    (display-set-draw-color-list char-color)
    (when note-str
      (display-draw-string (+ xb +track-grid-border-padding+) (+ yb +track-grid-border-padding+) note-str))))

(defun compute-track-cell-dimensions (numchars)
  (let* ((inner-row-height +CHAR-HEIGHT+)
         (inner-row-width (+ (* +CHAR-WIDTH+ numchars)))
         (pad2 (* +track-grid-border-padding+ 2))
         (outer-row-height (+ inner-row-height pad2 +track-grid-border-thickness+))
         (outer-row-width (+ inner-row-width pad2 +track-grid-border-thickness+)))
    (values inner-row-height inner-row-width outer-row-height outer-row-width)))

(defun compute-track-tick-lcm (&rest tracks)
  (let* ((ticks (mapcar #'track-ticks-per-bar tracks))
         (tickslcm (apply #'lcm ticks)))
    tickslcm))

;; todo draw row labels
(defun draw-note-track-lane (track x0 y0 outerh outerw &key (row-selected nil) (fg-color +color-white+) (bg-color +color-black+) (alt-bg-color +color-darkgrey+) (row-selected-color +color-yellow+) (track-selected-color +color-darkgrey+) (inc-row-labelsp nil))
  (let* ((border2 (* +track-grid-border-thickness+ 2))
         (track-len (track-length track))
         (num-rows (min track-len))
         (y1 (+ y0 +track-grid-border-thickness+ (floor (* outerh num-rows))))
         (x1 (+ x0 outerw)))
    ;; draw the bg & track border
    (display-set-draw-color-list (if row-selected track-selected-color alt-bg-color))
    (display-draw-border-rect x0 y0 x1 y1)
    (display-set-draw-color-list bg-color)
    (multiple-value-bind (outerh-floored outerh-rem) (floor outerh)
      ;; draw the rows
      (dotimes (i num-rows)
        (let ((yb (+ y0 +track-grid-border-thickness+ (floor (* i outerh))))
              (xb (+ x0 +track-grid-border-thickness+))
              (rect-height (+ (if (integerp (* i outerh-rem)) 1 0) (- outerh-floored border2)))
              (rect-width (- outerw border2))
              ;; alternate bg colors
              (bg (if (= (mod i 2) 0) alt-bg-color bg-color))
              (selectedp (and row-selected (= row-selected i))))
          (draw-note-track-lane-cell track i xb yb rect-width rect-height bg
                                     (if selectedp row-selected-color bg)
                                     (if selectedp row-selected-color fg-color)))))
    (values x1 y1)))

;; Initialize some example tracks at different tpb using euclidean initializer
(progn
  (defparameter testtrack1 (make-instance 'track :length 4 :ticks-per-bar 4))
  (track-set-euclidean testtrack1 4 52)
  (slot-value testtrack1 'notes)

  (defparameter testtrack2 (make-instance 'track :length 8 :ticks-per-bar 8))
  (track-set-euclidean testtrack2 8 40)
  (track-set-note testtrack2 1 255)
  (slot-value testtrack2 'notes)

  (defparameter testtrack3 (make-instance 'track :length 6 :ticks-per-bar 6))
  (track-set-euclidean testtrack3 6 58)

  (defparameter testtrack4 (make-instance 'track :length 3 :ticks-per-bar 3))
  (track-set-euclidean testtrack4 3 28)
  )

(defun compute-tracks-outer-height (outerh &rest tracks)
  (let* ((lcm (apply #'lcm (mapcar #'track-ticks-per-bar tracks)))
         (theights (mapcar (lambda (track) (/ (* outerh lcm) (track-ticks-per-bar track))) tracks))
         (minheight (apply #'min theights))
         (ratio (/ outerh minheight)))
    (mapcar (lambda (h) (* ratio h)) theights)))

(defparameter +midi-thread+ nil)
(defparameter +midi-stop-playing-flag+ nil)
(defparameter +midi-playback-args+ '())
(defparameter +midi-playback-bpm+ 120)
(defparameter +midi-playback-tracks+ nil)
(defparameter +midi-is-playing+ nil)


(defun ms-per-tick (bpm ticks-per-bar)
  (floor (/ 1000 (/ bpm 60.0)) (/ ticks-per-bar 4)))

(defun midi-play-track (bpm track)
  (let* ((ms (ms-per-tick bpm (track-ticks-per-bar track)))
         ;; note: this technique won't work so well when we want to
         ;; adjust per note microtime
         (ms-rt (* ms +internal-time-units-per-millisecond+))
         (starttime (get-internal-real-time))
         (lastnote nil))
    (dotimes (i (track-length track))
      (let ((note (track-get-note track i)))
        (format t "~s~%" note)
        (when (and lastnote (> note 0))
          (midi-note-off lastnote))
        (when (> note 0)
          (midi-note-on note) ;; !! bug !! should be midi value
          (setf lastnote note))
        (spin-until (+ starttime (* i ms-rt)))))))

;; (midi-play-track 80 testtrack2)

(defun midi-play-tracks (bpm tracks &optional (loop-count 1))
  "Play the selected tracks `loop-count` times at the set BPM"
  (let* ((tpb-per-track (mapcar #'track-ticks-per-bar tracks))
         ;; zip together the tracks and their corresponding ticksperbar
         (tracks-and-tpb (mapcar #'list tracks tpb-per-track (alexandria:iota (length tracks))))
         ;; array of last played note per track
         (lastnotes (make-array (length tracks) :initial-element nil))
         ;; increment by the ratio of each track tick to the LCM
         (lcm (apply #'compute-track-tick-lcm tracks))
         (incby (/ 1 lcm))
         (ms (ms-per-tick bpm lcm))
         (ms-rt (* ms +internal-time-units-per-millisecond+))
         (start-time (get-internal-real-time))
         (nticks 0))
    ;; TODO should pause GC here?
    (loop
      for i from 0 upto (* loop-count lcm)
      while (not +midi-stop-playing-flag+)
      do
         ;; loop thru each of the tracks
         (dolist (l tracks-and-tpb)
           (let* ((track (first l))
                  (tpb (second l))
                  (track-index (third l))
                  (tick (* tpb nticks)))
             ;; if we're at a non-fractional tick for this track, play note if any
             (when (integerp tick)
               ;; TODO this sucks. Can we unify note-struct and note somehow?
               (let ((note (track-get-note track tick))
                     (note-struct (track-get-note-struct track tick)))
                 ;; this covers offing previous note & also 'note off'=255
                 (when (and (aref lastnotes track-index) (> note 0))
                   (midi-note-off (note-midi-value (aref lastnotes track-index)))
                   (setf (aref lastnotes track-index) nil))
                 ;; play new note
                 (when note-struct
                   (midi-note-on (note-midi-value note-struct))
                   (setf (aref lastnotes track-index) note-struct))))))
         (incf nticks incby)
          ;; spin until we reach the next tick or playback was stopped
         (spin-untilp (lambda ()
                        (or (>= (get-internal-real-time) (+ start-time (* (1+ i) ms-rt)))
                            +midi-stop-playing-flag+))))
    ;; whenever we stop playing (finished loop, stop requested)
    ;; off all previous notes if any
    (dotimes (i (length lastnotes))
      (when (aref lastnotes i)
        (midi-note-off (note-midi-value (aref lastnotes i)))))))



;; ;; TODO p sure it's looping on the TPB instead of the track-length
;; (defun midi-play-tracks (bpm tracks &optional (loop-count 1))
;;   "Play the selected tracks `loop-count` times at the set BPM"
;;   (let* ((tpb-per-track (mapcar #'track-ticks-per-bar tracks))
;;          ;; zip together the tracks and their corresponding ticksperbar
;;          (tracks-and-tpb (mapcar #'list tracks tpb-per-track (alexandria:iota (length tracks))))
;;          ;; array of last played note per track
;;          (lastnotes (make-array (length tracks) :initial-element nil))
;;          ;; increment by the ratio of each track tick to the LCM
;;          (lcm (apply #'compute-track-tick-lcm tracks))
;;          (incby (/ 1 lcm))
;;          (maxtracklen (max (mapcar #'track-length tracks)))
;;          (ms (ms-per-tick bpm lcm))
;;          (ms-rt (* ms +internal-time-units-per-millisecond+))
;;          (start-time (get-internal-real-time))
;;          (nticks 0))
;;     ;; TODO should pause GC here?
;;     (loop
;;       for i from 0 upto (* loop-count lcm)
;;       while (not +midi-stop-playing-flag+)
;;       do
;;          ;; loop thru each of the tracks
;;          (dolist (l tracks-and-tpb)
;;            (let* ((track (first l))
;;                   (tpb (second l))
;;                   (track-index (third l))
;;                   (tick (* tpb nticks)))
;;              ;; if we're at a non-fractional tick for this track, play note if any
;;              (when (integerp tick)
;;                ;; TODO this sucks. Can we unify note-struct and note somehow?
;;                (let ((note (track-get-note track tick))
;;                      (note-struct (track-get-note-struct track tick)))
;;                  ;; this covers offing previous note & also 'note off'=255
;;                  (when (and (aref lastnotes track-index) (> note 0))
;;                    (midi-note-off (note-midi-value (aref lastnotes track-index)))
;;                    (setf (aref lastnotes track-index) nil))
;;                  ;; play new note
;;                  (when note-struct
;;                    (midi-note-on (note-midi-value note-struct))
;;                    (setf (aref lastnotes track-index) note-struct))))))
;;          (incf nticks incby)
;;           ;; spin until we reach the next tick or playback was stopped
;;          (spin-untilp (lambda ()
;;                         (or (>= (get-internal-real-time) (+ start-time (* (1+ i) ms-rt)))
;;                             +midi-stop-playing-flag+))))
;;     ;; whenever we stop playing (finished loop, stop requested)
;;     ;; off all previous notes if any
;;     (dotimes (i (length lastnotes))
;;       (when (aref lastnotes i)
;;         (midi-note-off (note-midi-value (aref lastnotes i)))))))


(defun midi-thread-initialize ()
  (format t "Started MIDI thread~%")
  (apply #'midi-play-tracks +midi-playback-args+))

(defun midi-play-tracks-own-thread (bpm tracks &optional (loop-count 1))
  (progn
    (when (not bt:*supports-threads-p*)
      (error "No  thread support with this version of CL!"))
    (when (not tracks)
      (error "No tracks supplied"))
    (when +midi-thread+
      (bt:destroy-thread +midi-thread+))
    (midi-initialize-stream)
    (setf +midi-stop-playing-flag+ nil)
    (setf +midi-playback-args+ (list bpm tracks loop-count))
    (setf +midi-is-playing+ t)
    ;; MIDI thread only writes to this, display thread only reads from
    ;; (setf +midi-tracks-current-tick+ (make-list (length tracks) :initial-element 0))
    ;; (setf +midi-playback-bpm+ bpm)
    ;; (setf +midi-playback-tracks+ tracks)
    (setf +midi-thread+
          (bt:make-thread #'midi-thread-initialize :name "Masterblaster MIDI thread"))))

(defun midi-stop-own-thread ()
  (progn
    (format t "Stopping MIDI thread playback...~%")
    (if (bt:thread-alive-p +midi-thread+)
        (progn
          (setf +midi-stop-playing-flag+ t)
          (bt:join-thread +midi-thread+)
          (setf +midi-stop-playing-flag+ nil)
          (setf +midi-thread+ nil)
          (format t "MIDI thread stopped and re-joined~%"))
        (progn
          (format t "MIDI thread already dead~%")
          (setf +midi-thread+ nil)))
    (setf +midi-is-playing+ nil)))


;; (midi-play-tracks 80
;;                   (list testtrack1
;;                         testtrack2
;;                         testtrack3
;;                         testtrack4)
;;                   10)

;; (midi-play-tracks-own-thread
;;  120
;;  (list
;;   testtrack1
;;   testtrack2
;;   testtrack3
;;   testtrack4)
;;  10)

;; (midi-stop-own-thread)
;; (bt:all-threads)
;; (bt:destroy-thread (second (bt:all-threads)))
;; (bt:thread-alive-p +midi-thread+)
;; (bt:join-thread (second (bt:all-threads)))
;; (equalp +midi-thread+ (third (bt:all-threads)))
;; (setf +midi-thread+ nil)

;; TODO
;; MIDI thread sends info on what track/tick is being played, main thread updates display


(defun render-tracks (tracks &optional (bpm 120))
  ;; TODO this is a nice & quick hack but just based on the verbosity of the approach (lambda,etc..)
  ;; probably ought to convert it to sth CLOS-based?
  ;; TODO dispatch on a set of keys? or toggle mod values so you can be in the UP handler and check mod values? (ex: shift modifier always just increases the increment of the thing you'd be doing anyway, UP = go up, UP+shift = go up one bar)
  (display-component
   (lambda ()
     (let* ((selected-track 0)
            (row 0))
       (list
        :down
        (lambda ()
          (setf row (mod (1+ row) (track-length (nth selected-track tracks))))
          t)
        :up
        (lambda ()
          (setf row (mod (1- row) (track-length (nth selected-track  tracks))))
          t)
        :right
        (lambda ()
          (let* ((next-track (mod (1+ selected-track) (length tracks)))
                 (lratio (/ (track-ticks-per-bar (nth next-track tracks)) (track-ticks-per-bar (nth selected-track tracks))))
                 (new-row (min (1- (track-length (nth next-track tracks))) (round (* lratio row)))))
            (setf selected-track next-track)
            (setf row new-row)
            t))
        :left
        (lambda ()
          (let* ((next-track (mod (1- selected-track) (length tracks)))
                 (lratio (/ (track-ticks-per-bar (nth next-track tracks)) (track-ticks-per-bar (nth selected-track tracks))))
                 (new-row (min (1- (track-length (nth next-track tracks))) (round (* lratio row)))))
            (setf selected-track next-track)
            (setf row new-row)
            t))
        :cancel
        (lambda ()
          (let* ((track (nth selected-track tracks))
                 (note (track-get-note track row)))
            (track-set-note track row 0)
            t))
        :play
        (lambda ()
          (if +midi-is-playing+
              (midi-stop-own-thread)
              (midi-play-tracks-own-thread bpm tracks 10000)))
        :mod1-right
        (lambda ()
          (let* ((track (nth selected-track tracks))
                 (note (track-get-note track row)))
            (track-set-note track row (1+ note))
            t))
        :mod1-left
        (lambda ()
          (let* ((track (nth selected-track tracks))
                 (note (track-get-note track row)))
            (track-set-note track row (1- note))
            t))
        :mod1-up
        (lambda ()
          (let* ((track (nth selected-track tracks))
                 (note (track-get-note track row)))
            (format t "note ~d next note ~d~%" note (+ note 12))
            (track-set-note track row (+ note 12))
            t))
        :mod1-down
        (lambda ()
          (let* ((track (nth selected-track tracks))
                 (note (track-get-note track row)))
            (format t "note ~d next note ~d~%" note (- note 12))
            (track-set-note track row (- note 12))
            t))
        :display
        (lambda ()
          (display-clear 0 0 0 255)
          (multiple-value-bind (innerh innerw outerh outerw) (compute-track-cell-dimensions 3)
            (let* ((track-heights (apply #'compute-tracks-outer-height outerh tracks))
                   (i 0))
              (dolist (track tracks)
                (draw-note-track-lane track (* i outerw) 0 (nth i track-heights) outerw
                                      :row-selected (if (= selected-track i) row nil))
                (incf i))
              ))))))))



;; BEWARE!!! PLAYING WITH SOME MUSIC HERE

(let ((track1 (make-instance 'track :length 8 :ticks-per-bar 4))
      (track2 (make-instance 'track :length 16 :ticks-per-bar 8))
      (track3 (make-instance 'track :length 15 :ticks-per-bar 8))
      (track4 (make-instance 'track :length 11 :ticks-per-bar 6)))

  (track-set-euclidean track1 2 (n :c2))
  (track-set-euclidean track2 1 (n :e4))
  (track-set-note track2 8 (n :g4))
  (track-set-euclidean track3 1 (n :b4))
  (track-rotate track3 1)
  (track-set-euclidean track4 1 (n :d5))

  (render-tracks (list track1 track2 track3 track4)))


(let ((track1 (make-instance 'track :length 8 :ticks-per-bar 4))
      (track2 (make-instance 'track :length 16 :ticks-per-bar 8))
      (track3 (make-instance 'track :length 12 :ticks-per-bar 6))
      (track4 (make-instance 'track :length 12 :ticks-per-bar 6)))

  (track-set-euclidean track1 2 (n :c3))
  (track-set-euclidean track4 3 (n :c2))
  (track-set-euclidean track2 4 (n :e4))
  (track-set-euclidean track3 3 (n :d4))
  (track-rotate track3 2)
  (render-tracks (list track1 track2 track3 track4)))


(let ((track1 (make-instance 'track :length 8 :ticks-per-bar 8))
      (track2 (make-instance 'track :length 7 :ticks-per-bar 8))
      (track3 (make-instance 'track :length 6 :ticks-per-bar 8))
      (track4 (make-instance 'track :length 6 :ticks-per-bar 6)))

  (track-set-euclidean track1 1 (n :c2))
  (track-set-note track1 3 (n :e4))
  (track-set-euclidean track2 1 (n :e4))
  (track-set-euclidean track3 1 (n :c4))
  (track-set-euclidean track4 1 (n :a4))
  (track-rotate track4 2)
  (render-tracks (list track1 track2 track3 track4)))

(let ((track1 (make-instance 'track :length 16 :ticks-per-bar 8))
      (track2 (make-instance 'track :length 16 :ticks-per-bar 8))
      (track3 (make-instance 'track :length 16 :ticks-per-bar 8))
      (track4 (make-instance 'track :length 15 :ticks-per-bar 8))
      (track5 (make-instance 'track :length 14 :ticks-per-bar 8))
      (track6 (make-instance 'track :length 2 :ticks-per-bar 1)))

  (track-set-euclidean track1 11 (n :c3))
  (track-set-euclidean track2 2 (n :d#3))
  (track-set-euclidean track3 2 (n :g3))

  (track-set-euclidean track4 1 (n :d5))
  (track-set-euclidean track5 2 (n :a#4))

  (track-set-note track6 0 (n :c2))
  (track-set-note track6 1 255)

  (render-tracks (list track1 track2 track3 track4 track5 track6) 80))
