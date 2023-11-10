;; Tiny test file for an isolated repro of the conflicts between portmidi & SDL

;; BUG -- commenting below line fixes issue
(ql:quickload "portmidi")

(ql:quickload "sdl2")

(defconstant +SCREEN-WIDTH+ 320)
(defconstant +SCREEN-HEIGHT+ 240)
(defparameter *window* nil)
(defparameter *renderer* nil)
(defparameter *window-zoom-level* 2)

(sdl2:with-init (:everything)
    (format t "Using SDL Library Version: ~D.~D.~D~%"
            sdl2-ffi:+sdl-major-version+
            sdl2-ffi:+sdl-minor-version+
            sdl2-ffi:+sdl-patchlevel+)
    (finish-output)

    (let ((width (* +screen-width+ *window-zoom-level*))
          (height (* +screen-height+ *window-zoom-level*)))
      (multiple-value-bind (window renderer) (sdl2:create-window-and-renderer width height '(:opengl))
        ;; BUG -- should just show a blank window but dies here instead
        ;; (sleep (* 60 3))
        (format t "Display: Created window and renderer...")
        (let ((tex (sdl2:create-texture renderer sdl2-ffi:+sdl-pixeltype-unknown+ sdl2-ffi:+sdl-textureaccess-streaming+ width height)))
          (let ((w (sdl2:texture-width tex))
                (h (sdl2:texture-height tex)))
            (format t "Display: Created texture width: ~d height: ~d~%" w h)
            ;; (print tex)
            ;; (print w)
            ;; (print h)
            ))
        (setf *window* window)
        (setf *renderer* renderer)))

  ;; This is one possible fix. But it's... weird.
  ;; (ql:quickload "portmidi")

  (format t "Display: Entering event loop~%")
  (sdl2:with-event-loop (:method :poll)
    ;; ESC will exit
    (:keyup (:keysym keysym)
            (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
              (sdl2:push-event :quit)))
    (:quit () (progn
                  (setf *window* nil)
                  (setf *renderer* nil)
                  t)))
  (sdl2:render-present *renderer*)

  ;; it works if you put it here too
  ;; (ql:quickload "portmidi")
  )
