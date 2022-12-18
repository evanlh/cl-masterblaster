;; (ql:quickload "cl-portaudio")
;; (use-package :portaudio)
(load "envelope.lisp")
(load "track.lisp")

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
          (sample-buffer (make-array 1024 :element-type 'float))
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

;; (plot (buffer-as-function test-buffer) 0 200)

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

;; scratch

(defparameter my-sine-instr (make-instance 'sine-instrument))
(play my-sine-instr test-metronome 120 44100)

(defparameter tb (make-track-sample-buffer test-metronome 120 44100))
(play-to-buffer my-sine-instr test-metronome 120 44100 tb)

(length tb)
(track-length test-etrack)
(play-sound tb)
(* 44100 (/ 119.0 60.0))

;; Imagining what I want the instrument interface to look like
(definstrument sine-instrument (sample-rate)
  (let ((last-note nil)
        ())
    (lambda (sample-index note note-struct bpm)
         (cond ((= note +NOTE-NO-VALUE+)
                (if last-note
                    ;; sustain phase?
                    (out-sample (* sample-index 2.0 PI (/ (note-freq last-note) sample-rate)))
                    ;; nada
                    (out-sample 0)))
               ((= note +NOTE-OFF-VALUE+)
                ;; TODO release phase
                (out-sample 0)
                (setf last-note nil))
               (t
                (out-sample (* sample-index 2.0 PI (/ (note-freq note-struct) sample-rate)))
                (setf last-note note-struct)
                )))))

;; not sure we really need the logic around note value if something later in
;; the pipeline applies the envelope to the in-sample. i.e:
(definstrument sine-instrument (sample-rate)
  (let ((last-note nil))
    (lambda (sample-index note note-struct bpm)
         (if (or note-struct last-note)
             (progn
               (out-sample (* sample-index 2.0 PI
                              (/ (note-freq (or note-struct last-note)) sample-rate)))
               (setf last-note note-struct))
             (out-sample 0)))))

;; chained with
(deffx volume-envelope (sample-rate env)
  (let ((last-note nil)
        (last-note-index 0)
        (envlen (length env))
        (sustain-start (envelope-x-at env (- envlen 3)))
        (release-start (envelope-x-at env (- envlen 2)))
        (ad-interpolator (envelope-interpolator-looping env :loop-p nil :loop-end sustain-start))
        (sustain-interpolator (envelope-interpolator-looping env :loop-start sustain-start :loop-end release-start))
        (release-interpolator (envelope-interpolator-looping env :loop-p nil :loop-start release-start))
         )
    (lambda (sample-index in-sample note note-struct bpm)
      ;; choose envelope phase based on note/note-struct val
      (cond
        ;; start A phase
        ((/= note +NOTE-NO-VALUE+)
         (setf interpolator ad-interpolator)
         (setf last-note-index sample-index))
        ;; release phase
        ((= note +NOTE-OFF-VALUE+)
         (setf interpolator release-interpolator)
         (setf last-note-index sample-index))
        ;; sustain phase
        ((> (- sample-index offset-index) sustain-start)
         (setf interpolator sustain-interpolator)
         (setf last-note-index sample-index)))
      
         )))

;; C-5 - note on/attack phase begins
;; ... - no-note / attack phase cont/decay phase begins/sustain begin depending on envelope
;; ... - ditto
;; off - note off / release phase begins

(track-get-note test-etrack 0)
(track-get-note test-etrack 1)
(track-get-note-struct test-etrack 0)

(defeffect delay-effect (sample-rate)
  (run (sample-index in-sample)))

;; for reference, this is what a simple sin instrument looks like in CLM:
;; https://ccrma.stanford.edu/software/snd/snd/clm.html

;; (definstrument simp (start-time duration frequency amplitude)
;;   (let* ((beg (floor (* start-time *srate*)))
;; 	     (end (+ beg (floor (* duration *srate*))))
;; 	     (j 0))
;;     (run
;;      (loop for i from beg below end do
;;        (outa i (* amplitude (sin (* j 2.0 pi (/ frequency *srate*)))))
;; 	   (incf j)))))
