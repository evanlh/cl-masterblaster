(ql:quickload "cl-portaudio")
(use-package :portaudio)

(defun play-sound (buf)
  (with-audio
    ;; do audio stuff
    (get-default-host-api)
    (get-host-api-info (get-default-host-api))
    (get-default-output-device)
    (let* (
          ;; (channels 1)
          (sample-rate 44100d0)
          ;; (sample-format :float)
          (sample-buffer (make-array 1024 :element-type :float))
          (frames-per-buffer (length sample-buffer))
          (input-params (make-stream-parameters))
          (output-params (make-stream-parameters))
          (pos 0))
      (setf
       (stream-parameters-device input-params) 0
       (stream-parameters-channel-count input-params) 1
       (stream-parameters-suggested-latency input-params) 0d1291
       (stream-parameters-sample-format input-params) :float

       (stream-parameters-device output-params) 1
       (stream-parameters-channel-count output-params) 1
       (stream-parameters-suggested-latency output-params) 0d1046
       (stream-parameters-sample-format output-params) :float)
      (with-audio-stream (my-stream input-params output-params :sample-rate sample-rate :frames-per-buffer frames-per-buffer)
      ;;; do audio stuff

        ;; loop over buffer, copying from buf to sample-buffer in chunks, write-stream the chunks
        (dotimes (j (ceiling (/ (length buf) (length sample-buffer))))
                (loop for i from 0 to (- (length sample-buffer) 1)
                      do (setf (aref sample-buffer i) (if (> (+ i pos 1) (length buf)) 0 (aref buf (+ i pos)))))
                (write-stream my-stream sample-buffer)
                (incf pos (length sample-buffer)))
        (pa-sleep 2000)
    ))))

;; (with-audio
;;   (print-devices)
;;   (get-device-info (get-default-output-device)))


(defvar test-buffer (make-array (* 44100 1) :element-type :float))
(setf test-buffer (make-array (* 44100 4) :element-type :float))

(loop for i from 0 to (- (length test-buffer) 1) do (setf (aref test-buffer i) (sin (/ i 24))))
(length test-buffer)
(play-sound test-buffer)

