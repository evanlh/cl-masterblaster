(ql:quickload "portmidi")

(portmidi:initialize)
(portmidi:count-devices)
(portmidi:list-devices)
(portmidi:close-midi)

(defvar *default-midi-out* (portmidi:get-default-output-device-id))
(defvar *default-midi-in* (portmidi:get-default-input-device-id))

(defparameter stream (portmidi:open-output *default-midi-out* 1024 10))
;; todo something like defvar i can redefine? have to setf here but should close the old one first,
;; otherwise you have to terminate the whole thing
(setf stream (portmidi:open-output *default-midi-out* 1024 0))

(portmidi:close-midi stream)


;; hoozah!!
(loop for i from 21 to 108
      do (progn (portmidi:write-short-midi stream 0 (portmidi:note-on 0 i))
                (sleep 0.1)
                (portmidi:write-short-midi stream 0 (portmidi:note-off 0 i))
                (sleep 0.1)))

(portmidi:write-short-midi stream 0 (portmidi:note-on 0 44))
(portmidi:write-short-midi stream 0 (portmidi:note-off 0 44))

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

(iternote (getf *notes* :c) (getf *scales* :minor))

(portmidi:write-short-midi stream 0 (portmidi:note-on 0 50))
(portmidi:write-short-midi stream 0 (portmidi:note-off 0 50))

(loop for i in (iternote (getf *notes* :c) (getf *chords* :maj))
      do (progn (portmidi:write-short-midi stream 0 (portmidi:note-on 0 i))
                (sleep 0.1)
                (portmidi:write-short-midi stream 0.1 (portmidi:note-off 0 i))
                (sleep 0.1)
                ))

;; just returns a pointer? not super useful
;; (portmidi:get-device-info *default-midi-out*)
;; (portmidi:get-device-info *default-midi-in*)
;; (portmidi:terminate)
;; (format nil "hit gc notification hook ~s" "testtest")



;; timing related funcs
internal-time-units-per-second

(defparameter *midi-start-clock-time* (get-internal-run-time))
(defconstant +seconds-to-usecs+ (float (/ 1 1000000)))
(defconstant +rtime-to-usecs+ (float ()))
