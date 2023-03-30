(defparameter *default-midi-out* nil)
(defparameter *default-midi-in* nil)
(defparameter *stream* nil)

;; Generic MIDI functions
(defun midi-initialize-stream ()
  (format t "Initializing MIDI, choosing default devices~%")
  (when *stream*
    (portmidi:close-midi *stream*)
    (setf *stream* nil))

  (portmidi:initialize)
  (format t "Found ~d MIDI devices~%" (portmidi:count-devices))
  (let* ((devlist (portmidi:list-devices))
         (default-out (portmidi:get-default-output-device-id))
         (default-in (portmidi:get-default-input-device-id))
        (stream (portmidi:open-output default-out 1024 10)))
    (format t "Using port: ~d name: ~s for MIDI OUT~%Using port: ~d, name: ~s for MIDI IN~%"
            default-out
            (cdr (nth default-out devlist))
            default-in
            (cdr (nth default-in devlist)))
    (setf *default-midi-out* default-out)
    (setf *default-midi-in* default-in)
    (setf *stream* stream)
    (values default-in default-out)))

(defun midi-note-on (note &optional (velocity 80) (channel 0))
  (portmidi:write-short-midi *stream* 0 (portmidi:note-on channel note velocity)))

(defun midi-note-off (note &optional (channel 0))
  (portmidi:write-short-midi *stream* 0 (portmidi:note-off channel note)))

;; PRECISE TIMING FUNCTIONS
(defparameter *midi-start-clock-time* (get-internal-real-time))
(defparameter +internal-time-units-per-millisecond+ (float (/ internal-time-units-per-second 1000)))

(defun spin-wait (duration-in-ms)
  (let* ((now (get-internal-real-time))
        (then (+ now (* duration-in-ms +internal-time-units-per-millisecond+)))
        (loops 0))
    (loop
      (incf loops)
      (when (>= (get-internal-real-time) then)
        (return loops)))))

;; TODO Minor improvements we could make here:
;; - calculate the mean loop time, break when >= future - meanlooptime
(defun spin-until (future-internal-real-time)
  (assert (typep future-internal-real-time 'integer))
  (let ((loops 0))
    (loop
      (when (>= (get-internal-real-time) future-internal-real-time)
        (return loops))
      (incf loops))
    loops))

(defun spin-untilp (predicate)
  "Spin in a busy loop until the `predicate` function returns t"
  (loop
    (when (funcall predicate)
      (return))))


;; CRUFT BELOW HERE--
;; (portmidi:get-device-info *default-midi-out*)
;; (portmidi:get-device-info *default-midi-in*)
;; (portmidi:terminate)


;; (portmidi:close-midi stream)

;; hoozah!!
;; (loop for i from 21 to 108
;;       do (progn (portmidi:write-short-midi stream 0 (portmidi:note-on 0 i))
;;                 (sleep 0.1)
;;                 (portmidi:write-short-midi stream 0 (portmidi:note-off 0 i))
;;                 (sleep 0.1)))

;; (portmidi:write-short-midi stream 0 (portmidi:note-on 0 44))
;; (portmidi:write-short-midi stream 0 (portmidi:note-off 0 44))

;; should maybe be arrays or hashmaps for speed?
(defvar *notes* '(:c 60 :c# 61 :d 62 :d# 63 :e 64 :f 65 :f# 66 :g 67 :g# 68 :a 69 :a# 70 :b 71))

(defvar *scales* '(:major (0 2 2 1 2 2 2 1)
                   :minor (0 2 1 2 2 1 2 2)))

(defvar *chords* '(:maj (0 4 3)
                   :min (0 3 4)
                   :dim (0 3 3)
                   :aug (0 4 4)))

(defun iternote (note seq)
  (let ((lastnote note))
    (loop for x in seq
          do (setf lastnote (+ x lastnote))
          collect lastnote)))

(defun translate-chromatic (increment note)
  (+ increment note))

;; (iternote (getf *notes* :c) (getf *scales* :minor))

;; (portmidi:write-short-midi stream 0 (portmidi:note-on 0 50))
;; (portmidi:write-short-midi stream 0 (portmidi:note-off 0 50))

;; (loop for i in (iternote (getf *notes* :c) (getf *chords* :maj))
;;       do (progn (portmidi:write-short-midi stream 0 (portmidi:note-on 0 i))
;;                 (sleep 0.1)
;;                 (portmidi:write-short-midi stream 0.1 (portmidi:note-off 0 i))
;;                 (sleep 0.1)
;;                 ))
