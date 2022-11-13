(ql:quickload "sdl2")
(ql:quickload :sdl2/examples)

(ql:system-apropos "midi")
;; (ql:quickload "portmidi")

;; We should be able to run examples as normal on CCL
#-sbcl (sdl2-examples:basic-test)

#+czl (+ 1 256)

(defconstant +SCREEN-WIDTH+ 320)
(defconstant +SCREEN-HEIGHT+ 240)

(defvar *bitmap-font* (bdf-list-to-bit-array (read-bdf-file-into-list "./Bauhaus.bdf")))

(defun draw-character (renderer posx posy charnum)
  ;; TODO assert charnum is in range
  ;; TODO replace charnum with character?
  (let ((bits (aref *bitmap-font* charnum)))
    (loop for i from 0 to 63
          do (let* ((y (floor i 8))
                    (x (- i (* 8 y))))
               (if (= 1 (aref bits i))
                   (sdl2:render-draw-point renderer (+ posx x) (+ posy  y)))
               )))
  )

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
      (sdl2:render-present renderer)

      ;; start event loop
      (sdl2:with-event-loop (:method :poll)
        ;; ESC will exit
        (:keyup (:keysym keysym)
                (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
                  (sdl2:push-event :quit)))
        ;; re-renders / blit texture goes here
        ;;         (:idle ()
        ;;                )
        (:quit () t))
      )))

(defun launch ()
  "Open our first window and draw some pixels"
  (init-display))

(trace init-display)
(launch)
