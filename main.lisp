;; Normally the init files loads this
;; (load "~/quicklisp/setup.lisp")

(ql:quickload "sdl2")
;; (ql:system-apropos "midi")
;; (ql:quickload "portmidi")

(defconstant +SCREEN-WIDTH+ 320)
(defconstant +SCREEN-HEIGHT+ 240)

(defvar *bitmap-font* (read-bdf-file-into-bit-array "./Bauhaus.bdf"))
(defconstant +CHAR_WIDTH+ 8)
(defconstant +CHAR_HEIGHT+ 8)

(defun draw-character (renderer posx posy charnum)
  ;; TODO assert charnum is in range
  ;; TODO replace charnum with character?
  (let ((bits (aref *bitmap-font* charnum)))
    (loop for i from 0 to (- (* +CHAR_WIDTH+ +CHAR_HEIGHT+) 1)
          do (let* ((y (floor i +CHAR_WIDTH+))
                    (x (- i (* +CHAR_WIDTH+ y))))
               (if (= 1 (aref bits i))
                   (sdl2:render-draw-point renderer (+ posx x) (+ posy  y)))
               ))))

(defun charcode-from-string (str i)
  (char-code (schar str i)))
;; (charcode-from-string "ab" 1)

(defun draw-string (renderer posx posy s)
  ;; TODO assert enough screen space?
  (loop for i from 0 to (- (length s) 1)
        do (draw-character renderer (+ posx (* i +CHAR_WIDTH+)) posy (charcode-from-string s i))))


(defun init-display ()
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

      (sdl2:set-render-draw-color renderer 0 0 0 0)
      (sdl2:render-clear renderer)
      (sdl2:set-render-draw-color renderer 255 255 255 255)
      (sdl2:render-draw-point renderer 100 100)
      (loop for c from 0 to (- (length *bitmap-font*) 1)
            do (let* ((y (floor c 40))
                      (x (- c (* 40 y))))
                 (print (list  x y c))
                 (draw-character renderer (* x 8) (* y 8) c)))
      ;; (draw-string renderer 200 200 "Hello World!")
      (sdl2:render-present renderer)

      ;; start event loop
)
    (sdl2:with-event-loop (:method :poll)
      ;; ESC will exit
      (:keyup (:keysym keysym)
              (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
                (sdl2:push-event :quit)))
      ;; re-renders / blit texture goes here
      ;;         (:idle ()
      ;;                )
      (:quit () t))
      ))

(defun launch ()
  "Open our first window and draw some pixels"
  (init-display))

(launch)
