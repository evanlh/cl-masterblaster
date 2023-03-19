;; scratch

;; (defun distance1d (x1 x2)
;;   (abs (- x1 x2)))

;; (defun midpoint (x1 x2)
;;   (let ((distance (distance1d x1 x2)))
;;     (if (> x1 x2)
;;         (- x1 (/ distance 2))
;;         (- x2 (/ distance 2)))))


;; (defvar test-buffer (make-array (* 44100 1) :element-type 'float))
;; (setf test-buffer (make-array (* 44100 1) :element-type 'float))

;; (loop for i from 0 to (- (length test-buffer) 1) do (setf (aref test-buffer i) (sin (/ i 24))))
;; (length test-buffer)
;; (play-sound test-buffer)

(defun make-sample-buffer (time-in-ms sample-rate)
  (make-array (ceiling (* time-in-ms (/ sample-rate 1000))) :element-type 'float))

(defun make-track-sample-buffer (track bpm sample-rate)
  (let* ((l (track-length track))
         (bufsize (* (samples-per-tick bpm sample-rate (track-ticks-per-bar track)) l)))
    (make-array (ceiling bufsize) :element-type 'float)))

(defun buffer->fn (buffer)
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


(defparameter my-sine-instr (make-instance 'sine-instrument))
(play my-sine-instr test-metronome 120 44100)

(defparameter tb (make-track-sample-buffer test-metronome 120 44100))
(play-to-buffer my-sine-instr test-metronome 120 44100 tb)

(defun simple-volume-envelope (&key (sample-rate 44100) &allow-other-keys)
  "A simple ADR envelope"
  (let* ((envelope (scale-envelope-x! (make-envelope 0.0 0.0 0.1 1.0 0.25 1.0 0.75 0.75 1.0 0.0) (* 441 40)))
         (interpolator (envelope-interpolator envelope :loop-p nil))
         (last-note-index 0))
    (format t "envelope ~s~%" envelope)
    (lambda (sample-index in-sample &key (freq 0.0) (gate 0.0) (trigger 0.0) &allow-other-keys)
      (if (> trigger 0.0)
          (progn
            (setf last-note-index sample-index)
            (* (funcall interpolator (- sample-index last-note-index)) in-sample))
          (* (funcall interpolator (- sample-index last-note-index)) in-sample)))))

(defun adsr-envelope (effect-fn &key (modulated :volume))
  "ADSR envelope with parameters editable via function keywords ATTACK, DECAY, SUSTAIN, RELEASE. Units are in milliseconds. Keyword parameter specified by MODULATED will modulated in the call to EFFECT-FN."
  (lambda (&rest other-keys &key (sample-rate 44100) &allow-other-keys)
    (let* ((last-note-index 0)
           (pvalue 0.0)
           (held 0)
           (msmult (/ sample-rate 100))
           (fn (apply effect-fn other-keys)))
      (lambda (x in-sample &rest other-inner-keys &key (gate 0.0) (trigger 0.0) (attack 10.0) (decay 50.0) (sustain 0.5) (release 100.0) &allow-other-keys)
        (let ((xdelta (- x last-note-index)))
          (cond ((and (> gate 0.0) (<= last-gate 0.0))
                 ;; note off->note on
                 (setf last-note-index x)
                 (setf held 0)
                 (setf pvalue 0.0))
                ((and (> gate 0.0) (> xdelta (* msmult (+ attack decay))))
                 ;; sustain phase
                 (setf pvalue sustain)
                 (incf held))
                ((and (<= gate 0.0) (> xdelta (* msmult (+ attack decay))))
                 ;; release phase
                 (setf pvalue (lerp-clamp sustain 0.0  (/ (* release msmult) (- xdelta held (* msmult (+ attack decay)))))))
                ((> xdelta (* msmult attack))
                 ;; decay phase
                 (setf pvalue (lerp-clamp 1.0 sustain (/ (- xdelta (* msmult attack)) (* msmult (+ attack decay))))))
                ((> xdelta 0)
                 ;; attack
                 (setf pvalue (lerp-clamp 0.0 1.0 (/ xdelta (* msmult attack))))))
          (setf other-inner-keys (append other-inner-keys (list modulated pvalue)))
          (apply fn x in-sample other-inner-keys))))))


;; yn = b0*x(n) - a1*y(n-1)
(defun one-pole-filter (&key (sample-rate 44100) &allow-other-keys)
  (let ((lasty 0.0))
    ;; q values approaching -1.0=> LP, q approaching 1.0 => HP
    (lambda (sample-index in-sample &key (q -0.986) &allow-other-keys)
      ;; let a1 = q
      (let* ((b0 (- 1.0 (abs q)))
             (yn (-  (* b0 in-sample) (* q lasty))))
        (setf lasty yn)
        yn))))

;; yn = b0*x(n) - a1*y(n-1) - a2*y(n-2)
(defun two-pole-filter (&key (sample-rate 44100) &allow-other-keys)
  (let ((yn1 0.0)
        (yn2 0.0))
    ;; q values approaching -1.0=> LP, q approaching 1.0 => HP
    (lambda (sample-index in-sample &key (q -0.98) &allow-other-keys)
      (let* ((a2 0.4)
             (b0 -0.2)
             (yn (-  (* b0 in-sample) (* q yn1) (* a2 yn2))))
        (setf yn2 yn1)
        (setf yn1 yn)
        yn))))

(defun envelope-wrapper-hof (effect-to-wrap &key (modulated :volume))
  "A simple ADR envelope as a higher-order function"
  (lambda (&rest other-keys &key (sample-rate 44100) &allow-other-keys)

    (let* ((envelope (scale-envelope-x! (make-envelope 0.0 -1.0 0.1 -0.9 0.25 -0.8 0.75 -0.98 1.0 -1.0) (* 441 40)))
           (interpolator (envelope-interpolator envelope :loop-p nil))
           (last-note-index 0)
           (fn (apply effect-to-wrap other-keys)))
      (lambda (sample-index in-sample &rest other-inner-keys &key (trigger 0.0) &allow-other-keys)
        (when (> trigger 0.0)
          (setf last-note-index sample-index))
        (setf other-inner-keys (append other-inner-keys (list modulated (funcall interpolator (- sample-index last-note-index)))))
        (apply fn sample-index in-sample other-inner-keys)))))

;; TODO how do we reset these to their initial phase on trigger?
(defun simple-sine-instrument (&key (sample-rate 44100) &allow-other-keys)
  (let ((sr-inverse (/ 1.0 sample-rate)))
    (lambda (sample-index &key (freq 0.0) (gate 0.0) (trigger 0.0) &allow-other-keys)
      (sin (* sample-index 2.0 PI freq sr-inverse)))))

(defun simple-saw-instrument (&key (sample-rate 44100) &allow-other-keys)
  (lambda (sample-index &key (freq 0.0) (gate 0.0) (trigger 0.0) &allow-other-keys)
    (if (= 0.0 freq)
        0.0
        (let ((t-of-p (/ sample-index (/ sample-rate freq))))
          (* 2.0 (- t-of-p (floor (+ 0.5 t-of-p))))))))

(defun simple-triangle-instrument (&key (sample-rate 44100) &allow-other-keys)
  (lambda (sample-index &key (freq 0.0) (gate 0.0) (trigger 0.0) &allow-other-keys)
    (if (= 0.0 freq)
        0.0
        (let ((t-of-p (/ sample-index (/ sample-rate freq))))
          (1- (* 2.0 (abs (* 2.0 (- t-of-p (floor (+ 0.5 t-of-p)))))))))))

(defun simple-square-instrument (&key (sample-rate 44100) &allow-other-keys)
  (let ((sr-inverse (/ 1.0 sample-rate)))
    (lambda (sample-index &key (freq 0.0) (gate 0.0) (trigger 0.0) &allow-other-keys)
      (if (= 0.0 freq)
          0.0
          (if (> 0.0 (sin (* sample-index 2.0 PI freq sr-inverse)))
              0.99
              -.99)))))

(defun instrument->fn (instr &key (sample-rate 44100) (freq 440.0) (gate 1.0) (trigger 0.0))
  "Treat INSTR as a normal function, hardcoding the sample-rate, freq, gate, and trigger params"
  (let ((i (funcall instr :sample-rate sample-rate)))
    (lambda (x)
      (funcall i x :freq freq :gate gate :trigger trigger))))

(plot (instrument->fn #'simple-sine-instrument :freq 440.0) 0 (/ 44100 440))
(plot (instrument->fn #'simple-saw-instrument :freq 440.0) 0 (/ 44100 440))
(plot (instrument->fn #'simple-triangle-instrument :freq 440.0) 0 (/ 44100 440))
(plot (instrument->fn #'simple-square-instrument :freq 440.0) 0 (/ 44100 440))

(defun instrument+fx (instrument effect)
  "Chain an INSTRUMENT and an EFFECT together, so the former will be processed by the latter"
  (lambda (&rest other-keys &key (sample-rate 44100) &allow-other-keys)
    (let ((instr (apply instrument other-keys))
          (fx (apply effect other-keys)))
      (lambda (x &rest rest-keys)
        (let ((out (apply instr x rest-keys)))
          (apply fx x out rest-keys))))))

(defun mxdmx (&rest fns)
  "Multiplex/demultiplex a list of instruments or effects. Returns a function which forwards all arguments to the provided list of fns and mixes together their output."
  (lambda (&rest other-keys)
    (let ((multiplier (/ 1.0 (length fns)))
          (lambdas (mapcar (lambda (f) (apply f other-keys)) fns)))
      (lambda (&rest other-inner-keys)
        (let ((out 0.0))
          (dolist (fn lambdas)
            (incf out (* multiplier (apply fn other-inner-keys))))
          out)))))

(plot (instrument->fn (mxdmx #'simple-sine-instrument #'simple-square-instrument) :freq 440.0) 0 (/ 44100 440))

(defun transform-param (fn transformerfn &key (parameter :q) (default-if-nil 0.0))
  (lambda (&rest other-keys)
    (let ((l (apply fn other-keys)))
      (lambda (x &rest other-inner-keys)
        (let ((p (or (getf other-inner-keys parameter) default-if-nil)))
          (setf (getf other-inner-keys parameter) (funcall transformerfn p))
          (apply l x other-inner-keys))))))

(defun print-params-instrument (&key (sample-rate 44100) &allow-other-keys)
  (lambda (sample-index &rest other-keys)
    (format t "i: ~d, ~s~%" sample-index other-keys)
    0.0))

(plot (instrument->fn (transform-param #'print-params-instrument (lambda (x) (+ x 3.0)) :parameter :freq ) :freq 440.0) 0 10)

(defun track->freq-gate-trigger (track instrument)
  "Map the TRACK note data to :freq, :gate, and :trigger values the INSTRUMENT understands"
  (lambda (&rest other-keys &key (sample-rate 44100) (bpm 120.0) &allow-other-keys)
    (let ((ticklength (samples-per-tick bpm sample-rate (track-ticks-per-bar track)))
          (tracklen (track-length track))
          (lastnote +NOTE-NO-VALUE+)
          (lastfreq 0.0)
          (instr (apply instrument other-keys)))
      (lambda (x &rest rest-keys)
        (multiple-value-bind (div rem) (floor (/ x ticklength))
          (let* ((pos (mod div tracklen))
                 (note (track-get-note track pos))
                 (note-struct (track-get-note-struct track pos))
                 (freq (if note-struct (note-freq note-struct) 0.0))
                 (at-tick-crossingp (= 0 rem))
                 (trigger (if (and note-struct at-tick-crossingp) 1.0 0.0)))
            (when (> 0 note) (setf lastnote note))
            (when (> 0 freq (setf lastfreq freq)))
            (apply instr x
                   :freq (or freq lastfreq)
                   :trigger trigger
                   ;; subtracting trigger under the assumption that we want gate to quickly toggle
                   ;; off/on on note boundaries. results in being off by 1 sample but we don't
                   ;; need to lookahead this way
                   :gate (if (and (<= lastnote +NOTE-MAX-VALUE+) (>= lastnote +NOTE-MIN-VALUE+))
                             (if trigger 0.0 1.0)
                             0.0)
                   rest-keys)))))))

;; (defun instrument+instrument (i1 i2 &key (balance-param :balance)))

(plot (instrument->fn (instrument+fx #'simple-triangle-instrument #'simple-volume-envelope) :freq 440.0) 0 (/ 44100 440))

(plot (instrument->fn (track->freq-gate-trigger test-metronome
                                                (instrument+fx #'simple-triangle-instrument #'simple-volume-envelope)) :freq 440.0) 0 (/ 44100 4))

(pl (track->buffer test-metronome (track->freq-gate-trigger test-metronome (mxdmx #'simple-triangle-instrument #'simple-square-instrument #'simple-saw-instrument))))

(defun track-note-data-at-sample-offset (track sample-index bpm sample-rate)
  (let* ((ticklength (samples-per-tick bpm sample-rate (track-ticks-per-bar track)))
         (tracklen (track-length track)))
    (multiple-value-bind (div rem) (floor (/ sample-index ticklength))
      (let ((pos (mod div tracklen)))
        (values (track-get-note track pos) (track-get-note-struct track pos) rem)))))


(defun track->buffer (track instrument &rest other-keys &key (sample-rate 44100) (bpm 120.0) &allow-other-keys)
  "Allocate a buffer, play INSTRUMENT on TRACK at SAMPLE-RATE and BPM and return the buffer"
  ;; TODO don't just alloc a big buffer
  (let ((out-buffer (make-track-sample-buffer track bpm sample-rate))
        (instr (apply instrument other-keys)))
    (dotimes (i (length out-buffer))
      (setf (aref out-buffer i) (apply instr i other-keys)))
    out-buffer))

(envelope-wrapper-hof #'one-pole-filter :modulated :q)

(pl (track->buffer test-metronome
                   (track->freq-gate-trigger
                    test-metronome
                    ;;(instrument+fx #'simple-triangle-instrument #'simple-volume-envelope)
                    ;; (instrument+fx #'simple-square-instrument (envelope-wrapper-hof #'one-pole-filter :modulated :q))
                    ;; (instrument+fx #'simple-saw-instrument #'two-pole-filter)
                    ;; #'simple-saw-instrument
                    (instrument+fx
                     (mxdmx #'simple-square-instrument #'simple-saw-instrument #'simple-triangle-instrument)
                     (adsr-envelope #'one-pole-filter :modulated :q))
                    )
                   :attack 10.0
                   ))

(defparameter short-track (make-instance 'track :length 2))
(slot-value short-track 'notes)
(track-set-note short-track 1 40)
(track-get-note-struct short-track 1)

(defun pl (buffer)
  (play-sound buffer)
  (plot (buffer->fn buffer) 0 (length buffer)))

(pl (track->buffer short-track
                   (track->freq-gate-trigger
                    short-track
                    (instrument+fx #'simple-saw-instrument #'simple-volume-envelope)
                    ;; #'simple-saw-instrument
                    )
                   44100 120))



;; C-5 - note on/attack phase begins
;; ... - no-note / attack phase cont/decay phase begins/sustain begin depending on envelope
;; ... - ditto
;; off - note off / release phase begins


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



;; define the gabor transform
;; define the fft transform
;; define the inverse fft transform
;; make a pink noise generator


;; make a white noise generator

;; Naive Park & Miller random number generator
;; from Numerical Recipes in C, 7.1.2
(defparameter my-random-seed 4357834578)
(defun my-random-generator (&optional (seed my-random-seed))
  (when (= seed 0) (setf seed my-random-seed))
  (let* ((a 16807)
         (m 2147483647)
         (new-i (mod (* a seed) m)))
    (setf my-random-seed new-i)
    new-i))
(my-random-generator)

(defun my-random-clamped ()
  (/ 1.0 (my-random-generator)))
(my-random-clamped)

(defun rand0 (seed)
  (when (= seed 0) (setf seed my-random-seed))
  (let* ((IA 16807)
         (IM 2147483647)
         (AM (/ 1.0 IM))
         (IQ 127773)
         (IR 2836)
         (k (/ seed IQ)))
    (setf seed (-  (* IA (- seed (* k IQ))) (* IR k)))
    (when (< 0 seed) (setf seed (+ IM seed)))
    (setf seed (* seed AM))
    (setf my-random-seed seed)
    seed))

(rand0 0)
my-random-seed

(defconstant +track-grid-border-padding+ 2)
(defconstant +track-grid-border-thickness+ 1)

;; playing with track UI
(declaim (ftype (function (track fixnum fixnum fixnum fixnum fixnum list list list)) draw-note-track-lane-cell))
(defun draw-note-track-lane-cell (track index xb yb w h bg-color sel-color char-color)
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

(display-component
 (lambda ()
   (let* ((selected-track 0)
          (row 0)
          (tracks (list testtrack1 testtrack2 testtrack3 testtrack4)))
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
          (track-set-note track row (+ note 12))
          t))
      :mod1-down
      (lambda ()
        (let* ((track (nth selected-track tracks))
               (note (track-get-note track row)))
          (track-set-note track row (- note 12))
          t))
      :display
      (lambda ()
        (display-clear 0 0 0 255)
        (multiple-value-bind (innerh innerw outerh outerw) (compute-track-cell-dimensions 3)
          (let* ((track-heights (compute-tracks-outer-height outerh testtrack1 testtrack2 testtrack3 testtrack4))
                 (i 0))
            (dolist (track tracks)
              (draw-note-track-lane track (* i outerw) 0 (nth i track-heights) outerw
                                    :row-selected (if (= selected-track i) row nil))
              (incf i))
            )))))))

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

(midi-play-track 80 testtrack2)

(defun midi-play-tracks (bpm &rest tracks)
  (let* ((tpb-per-track (mapcar #'track-ticks-per-bar tracks))
         (tracks-and-tpb (mapcar #'list tracks tpb-per-track))
         (lastnotes (make-array (length tracks) :initial-element nil :element-type :note))
         (lcm (apply #'compute-track-tick-lcm tracks))
         (incby (/ 1 lcm))
         (ms (ms-per-tick bpm lcm))
         (ms-rt (* ms +internal-time-units-per-millisecond+))
         (start-time (get-internal-real-time))
         (n 0)
         (track-index 0))
    ;; TODO should pause GC here?
    (format t "~s~%" tracks-and-tpb)
    (dotimes (i lcm)
      (setf track-index 0)
      (dolist (l tracks-and-tpb)
        (let* ((track (first l))
               (tpb (second l))
               (tick (* tpb n)))
          (when (integerp tick)
            ;; this sucks. we should unify note-struct and note somehow.
            (let ((note (track-get-note track tick))
                  (note-struct (track-get-note-struct track tick)))
              (when (and (aref lastnotes track-index) (> note 0))
                (midi-note-off (note-midi-value (aref lastnotes track-index)))
                (format t "playing midi off ~s tick ~d ~%" (note-midi-value (aref lastnotes track-index)) tick)

                (setf (aref lastnotes track-index) nil))
              (when note-struct
                (midi-note-on (note-midi-value note-struct))
                (format t "playing midi note ~s tick ~d ~%" (note-midi-value note-struct) tick)
                (setf (aref lastnotes track-index) note-struct)))))
        (incf track-index))
      (incf n incby)
      (format t "~d ~d ~%" n i)
      ;; spin until we reach the next tick
      (spin-until (+ start-time (* (1+ i) ms-rt)))
      )
    (dotimes (i (length lastnotes))
      (midi-note-off (note-midi-value (aref lastnotes i))))))

(midi-play-tracks 120
                  testtrack1
                  ;; testtrack2
                  testtrack3
                  ;; testtrack4
                  )

(track-ticks-per-bar testtrack1)
(track-length testtrack1)
(slot-value testtrack2 'notes)
(note-midi-value (track-get-note-struct testtrack1 0))
(track-get-note-struct testtrack2 1)
