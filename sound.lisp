;; (ql:quickload "cl-portaudio")

(load "envelope.lisp")
(load "track.lisp")

;; (use-package :portaudio)

(defun play-sound (buf)
  (portaudio:with-audio
    ;; do audio stuff
    (portaudio:get-default-host-api)
    (portaudio:get-host-api-info (portaudio:get-default-host-api))
    (portaudio:get-default-output-device)
    (let* (
          ;; (channels 1)
          (sample-rate 44100d0)
          ;; (sample-format :float)
          (sample-buffer (make-array 1024 :element-type 'float))
          (frames-per-buffer (length sample-buffer))
          (input-params (portaudio:make-stream-parameters))
          (output-params (portaudio:make-stream-parameters))
          (pos 0))
      (setf
       (portaudio:stream-parameters-device input-params) 0 ;; (portaudio:device-info-host-api (portaudio:get-device-info (portaudio:get-default-input-device)))
       (portaudio:stream-parameters-channel-count input-params) 1 ;; (portaudio:device-info-max-input-channels (portaudio:get-device-info (portaudio:get-default-input-device))) ;; 1
       (portaudio:stream-parameters-suggested-latency input-params) 0d1291
       (portaudio:stream-parameters-sample-format input-params) :float

       (portaudio:stream-parameters-device output-params) 1 ;; (portaudio:device-info-host-api (portaudio:get-device-info (portaudio:get-default-output-device)))
       (portaudio:stream-parameters-channel-count output-params) 1 ;; (portaudio:device-info-max-output-channels (portaudio:get-device-info (portaudio:get-default-output-device))) ;; 1
       (portaudio:stream-parameters-suggested-latency output-params) 0d1046
       (portaudio:stream-parameters-sample-format output-params) :float)

      (format t "output:~%")
      (log-stream-parameters output-params)
      (format t "input:~%")
      (log-stream-parameters input-params)
      (portaudio:with-audio-stream (my-stream input-params output-params :sample-rate sample-rate :frames-per-buffer frames-per-buffer)

        ;; loop over buffer, copying from buf to sample-buffer in chunks, write-stream the chunks
        (dotimes (j (ceiling (/ (length buf) (length sample-buffer))))
          (loop for i from 0 to (1- (length sample-buffer))
                do (setf (aref sample-buffer i)
                         (if (>= (+ i pos 1) (length buf))
                             0
                             (aref buf (+ i pos)))))
          (portaudio:write-stream my-stream sample-buffer)
          (incf pos (length sample-buffer)))
        ;; (pa-sleep 2000)
    ))))

(defun log-stream-parameters (p)
  (format t "device: ~s~%" (portaudio:stream-parameters-device p))
  (format t "channel count: ~s~%" (portaudio:stream-parameters-channel-count p))
  (format t "sample-format: ~s~%" (portaudio:stream-parameters-sample-format p))
  (format t "suggested latency: ~s~%" (portaudio:stream-parameters-suggested-latency p)))

(portaudio:with-audio
  (portaudio:print-devices)
  (portaudio:get-device-info (portaudio:get-default-output-device)))


(portaudio:with-audio
  (portaudio:device-info-max-output-channels (portaudio:get-device-info (portaudio:get-default-output-device))))
(portaudio:with-audio
  (portaudio:device-info-max-input-channels (portaudio:get-device-info (portaudio:get-default-input-device))))
(portaudio:with-audio
  (portaudio:device-info-host-api (portaudio:get-device-info (portaudio:get-default-output-device))))
(portaudio:with-audio
  (portaudio:device-info-host-api (portaudio:get-device-info (portaudio:get-default-input-device))))
(portaudio:with-audio
  (portaudio:device-info-name (portaudio:get-device-info (portaudio:get-default-output-device))))
(portaudio:with-audio
  (portaudio:device-info-name (portaudio:get-device-info (portaudio:get-default-input-device))))
(portaudio:with-audio
  (portaudio:device-info-default-sample-rate (portaudio:get-device-info (portaudio:get-default-output-device))))
(portaudio:with-audio
  (portaudio:device-info-default-low-output-latency (portaudio:get-device-info (portaudio:get-default-output-device))))

(defvar test-buffer (make-array (* 44100 1) :element-type 'float))
(setf test-buffer (make-array (* 44100 1) :element-type 'float))

(loop for i from 0 to (- (length test-buffer) 1) do (setf (aref test-buffer i) (sin (/ i 24))))
(length test-buffer)
(play-sound test-buffer)

(defun make-sample-buffer (time-in-ms sample-rate)
  (make-array (ceiling (* time-in-ms (/ sample-rate 1000))) :element-type 'float))

(defun make-track-sample-buffer (track bpm sample-rate)
  (let* ((l (track-length track))
         (bufsize (* (samples-per-tick bpm sample-rate (track-ticks-per-bar track)) l)))
    (make-array (ceiling bufsize) :element-type 'float)))

(defun buffer-as-function (buffer)
  (lambda (x)
    (if (or (< x 0) (>= x (length buffer)))
        0.0
        (aref buffer (floor x)))))

(defclass instrument () ())
(defclass sine-instrument (instrument) ())
(defclass env-instrument (instrument) ())

(defun samples-per-tick (bpm sample-rate ticks-per-bar)
  (floor (/ sample-rate (/ bpm 60.0)) (/ ticks-per-bar 4)))

(defmethod play-to-buffer ((instr sine-instrument) (track track) bpm sample-rate buffer)
  (let ((spt (samples-per-tick bpm sample-rate (track-ticks-per-bar track)))
        (last-note nil))
    (loop for ti from 0 below (track-length track) do
      (let* ((note (track-get-note track ti))
            (note-struct (track-get-note-struct track ti))
            (freq (and note-struct (note-freq note-struct))))
        (loop for si from 0 below spt do
          (if (not note-struct)
              (setf (aref buffer (+ si (* ti spt))) 0)
              (setf (aref buffer (+ si (* ti spt)))
                    (sin (* si 2.0 PI (/ freq sample-rate))))))))))

(defmethod play ((instr instrument) (track track) bpm sample-rate)
  (let* ((buffer (make-track-sample-buffer track bpm sample-rate)))
    (play-to-buffer instr track bpm sample-rate buffer)
    (play-sound buffer)))
