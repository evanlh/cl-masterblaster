;; scratch

(plot (envelope-interpolator (make-envelope 0.0 1.0 1.0 0.0 2.0 1.0 3.0 0.0 4.0 1.0) :loop-start 2.0 :loop-end 3.0) 0.0 4.0)

(plot (buffer-as-function test-buffer) 0 PI)


(defparameter my-sine-instr (make-instance 'sine-instrument))
(play my-sine-instr test-metronome 120 44100)

(defparameter tb (make-track-sample-buffer test-metronome 120 44100))
(play-to-buffer my-sine-instr test-metronome 120 44100 tb)

(length tb)
(track-length test-etrack)
(play-sound tb)
(* 44100 (/ 119.0 60.0))

;; Imagining what I want the instrument interface to look like
;; (definstrumen sine-instrument (sample-rate)
;;   (let ((last-note nil)
;;         ())
;;     (lambda (sample-index note note-struct bpm)
;;          (cond ((= note +NOTE-NO-VALUE+)
;;                 (if last-note
;;                     ;; sustain phase?
;;                     (out-sample (* sample-index 2.0 PI (/ (note-freq last-note) sample-rate)))
;;                     ;; nada
;;                     (out-sample 0)))
;;                ((= note +NOTE-OFF-VALUE+)
;;                 ;; TODO release phase
;;                 (out-sample 0)
;;                 (setf last-note nil))
;;                (t
;;                 (out-sample (* sample-index 2.0 PI (/ (note-freq note-struct) sample-rate)))
;;                 (setf last-note note-struct)
;;                 )))))

;; not sure we really need the logic around note value if something later in
;; the pipeline applies the envelope to the in-sample. i.e:
;; (definstrument sine-instrument (sample-rate)
;;   (let ((last-note nil))
;;     (lambda (sample-index note note-struct bpm)
;;          (if (or note-struct last-note)
;;              (progn
;;                (out-sample (* sample-index 2.0 PI
;;                               (/ (note-freq (or note-struct last-note)) sample-rate)))
;;                (setf last-note note-struct))
;;              (out-sample 0)))))


;; chained with
(defun volume-envelope (sample-rate env)
  (let ((last-note-index 0)
        (envlen (length env))
        (sustain-start (envelope-x-at env (- envlen 3)))
        (release-start (envelope-x-at env (- envlen 2)))
        (ad-interpolator (envelope-interpolator env :loop-p nil :loop-end sustain-start))
        (sustain-interpolator (envelope-interpolator env :loop-start sustain-start :loop-end release-start))
        (release-interpolator (envelope-interpolator env :loop-p nil :loop-start release-start))
         )
    (lambda (sample-index in-sample note note-struct)
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
      (let ((sample-x-envelope (* (interpolator (- sample-index last-note-index)) in-sample)))
        sample-x-envelope))))

(defun simple-volume-envelope-fx (env)
  (let ((interpolator (envelope-interpolator env :loop-p nil))
        (last-note-index 0))
    (lambda (sample-index in-sample note note-struct)
      (cond
        ;; ((and (= last-note-index 0) (= note +NOTE-NO-VALUE+))
        ;;  ;; nothing to do
        ;;  in-sample)
        (note-struct
         ;; new note, start the envelope anew
         (setf last-note-index sample-index)
         (* (funcall interpolator (- sample-index last-note-index)) in-sample))
        (t
         (* (funcall interpolator (- sample-index last-note-index)) in-sample))
        ))))

;; contract with instruments vs fx is different in terms of the note + note-struct? no. note/note-struct
;; should only be passed at the specific sample index/tick when the value occurs. otherwise up to the
;; instr/fx to save the last value if needed

(defun simple-sine-instrument (sample-rate)
  (let ((last-note nil)
        (sr-inverse (/ 1.0 sample-rate)))
    (lambda (sample-index note note-struct)
         (if (or note-struct last-note)
             (progn
               (setf last-note (or note-struct last-note))
               (sin (* sample-index 2.0 PI (note-freq (or note-struct last-note)) sr-inverse))
               )
             0))))

(defun simple-sine-instrument2 (&key (sample-rate 44100))
  (let ((sr-inverse (/ 1.0 sample-rate)))
    (lambda (sample-index &key (freq 0.0) (gate 0.0) (trigger 0.0) &allow-other-keys)
      (sin (* sample-index 2.0 PI freq sr-inverse)))))

(defun simple-saw-instrument (&key (sample-rate 44100))
  (lambda (sample-index &key (freq 0.0) (gate 0.0) (trigger 0.0) &allow-other-keys)
    (let ((t-of-p (/ sample-index (/ sample-rate freq))))
      (* 2.0 (- t-of-p (floor (+ 0.5 t-of-p)))))
    ))

(defun simple-triangle-instrument (&key (sample-rate 44100))
  (lambda (sample-index &key (freq 0.0) (gate 0.0) (trigger 0.0) &allow-other-keys)
    (let ((t-of-p (/ sample-index (/ sample-rate freq))))
      (1- (* 2.0 (abs (* 2.0 (- t-of-p (floor (+ 0.5 t-of-p))))))))))


(defun simple-square-instrument (&key (sample-rate 44100))
  (let ((sr-inverse (/ 1.0 sample-rate)))
    (lambda (sample-index &key (freq 0.0) (gate 0.0) (trigger 0.0) &allow-other-keys)
      (if (> 0.0  (sin (* sample-index 2.0 PI freq sr-inverse)))
          0.99
          -.99))))

(defun instrument->fn (instr &key (sample-rate 44100) (freq 440.0) (gate 1.0) (trigger 1.0))
  (let ((i (funcall instr :sample-rate sample-rate)))
    (lambda (x)
      (funcall i x :freq freq :gate gate :trigger trigger))))


(plot (instrument->fn #'simple-sine-instrument2 :freq 440.0) 0 (/ 44100 440))
(plot (instrument->fn #'simple-saw-instrument :freq 440.0) 0 (/ 44100 440))
(plot (instrument->fn #'simple-triangle-instrument :freq 440.0) 0 (/ 44100 440))
(plot (instrument->fn #'simple-square-instrument :freq 440.0) 0 (/ 44100 440))

;; (defun testsaw (i)
;;   (* 32000 (1- (/ i 1024.0))))
;; (testsaw 200)
;; float(32000*(i/1024.0-1.0))

(defun chain-sine-volume (sample-rate envelope)
  (let ((instr (simple-sine-instrument sample-rate))
        (fx (simple-volume-envelope-fx envelope)))
    (lambda (sample-index note note-struct)
      (let ((o (funcall fx sample-index (funcall instr sample-index note note-struct) note note-struct)))
        (format t "~d ~%" o)
        o)
      ;; (let ((o (funcall instr sample-index note note-struct)))
      ;;   (format t "~d ~%" o)
      ;;   o)
      )))

(defun track-note-data-at-sample-offset (track sample-index bpm sample-rate)
  (let* ((ticklength (samples-per-tick bpm sample-rate (track-ticks-per-bar track)))
         (tracklen (track-length track)))
    (multiple-value-bind (div rem) (floor (/ sample-index ticklength))
      (let ((pos (mod div tracklen)))
        (values (track-get-note track pos) (track-get-note-struct track pos) rem)))))

(track-note-data-at-sample-offset test-etrack 11050 120 44100)

(track-length test-etrack)
(defparameter testenv (scale-envelope-x! (make-envelope 0.0 0.0 0.001 1.0 0.5 1.0 1.0 0.0) (/ 44100 2)))

(defun track-play-loop (track generator bpm sample-rate)
  ;; TODO don't just alloc a big buffer
  (let ((out-buffer (make-track-sample-buffer track bpm sample-rate))
        (instr-lambda (funcall generator sample-rate testenv)))
    (dotimes (i (length out-buffer))
      (multiple-value-bind (note note-struct rem) (track-note-data-at-sample-offset track i bpm sample-rate)
        (setf (aref out-buffer i) (if (= 0 rem)
                                      (progn
                                        (format t "~s ~s ~%" note note-struct)
                                        (funcall instr-lambda i note note-struct))
                                      (funcall instr-lambda i +NOTE-NO-VALUE+ nil)))))
    out-buffer))

(defparameter b (track-play-loop test-metronome #'chain-sine-volume 120 44100))
(plot (buffer-as-function b) 0 (length b))
(play-sound b)
(/ (length b) 5)

(track-get-note-struct test-etrack 0)


;; C-5 - note on/attack phase begins
;; ... - no-note / attack phase cont/decay phase begins/sustain begin depending on envelope
;; ... - ditto
;; off - note off / release phase begins

(track-get-note test-etrack 0)
(track-set-note test-etrack 4 40)
(track-get-note-struct test-etrack 0)

(defeffect delay-effect (sample-rate)
  (run (sample-index in-sample)))

;; (envelope-x-at (make-envelope 0.0 0.0 0.5 1.0 1.0 0.0) 0)

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


