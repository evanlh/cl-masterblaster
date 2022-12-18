(load "arrays.lisp")
(load "euclidean.lisp")
(load "notes.lisp")

(defclass track ()
  ((length :initarg :length
           :initform (error "Must supply track length"))
   (notes)
   (ticks-per-bar :initarg :ticks-per-bar
                  :initform 4)))

(defmethod initialize-instance :after ((track track) &key)
  (let ((track-length (slot-value track 'length)))
    (setf (slot-value track 'notes)
          (make-array track-length :initial-element 0 :element-type 'integer))))

;; TODO replace with defmethod? does it matter yet?
(defun track-length (track)
  (slot-value track 'length))

(defun track-ticks-per-bar (track)
  (slot-value track 'ticks-per-bar))

(defun track-set-note (track index note)
  "Set the TRACK's note value at INDEX to NOTE.
   NOTE can take values 0=no note (sustain), 1-108 note on, 255 note off"
  (unless (or (and (>= note +NOTE-MIN-VALUE+) (<= note +NOTE-MAX-VALUE+))
              (= note +NOTE-NO-VALUE+) (= note +NOTE-OFF-VALUE+))
    (error "NOTE should be in the range 1-108, 255 for note off, 0 for no note"))
  (setf (aref (slot-value track 'notes) (mod index (track-length track))) note))

(defun track-get-note (track index)
  "Get the TRACK's note value at INDEX"
  (aref (slot-value track 'notes) (mod index (track-length track))))

(defun track-get-note-struct (track index)
  "Get the note struct corresponding to the note value at INDEX in TRACK"
  (note (track-get-note track index)))

(defun track-rotate (track n)
  "Rotate TRACK note data N ticks (forward if positive, backwards if negative)"
  (array-rotate (slot-value track 'notes) n))

(defun track-set-euclidean (track pulses note)
  "Use the euclidean rhythm algorithm to evenly distributed PULSES with note value NOTE over TRACK"
  ;; ? should we use the TPB instead of track-length? So it could repeat?
  (let ((rhythm (euclidean-rhythm pulses (track-length track))))
    (dotimes (i (length rhythm))
      (when (> (aref rhythm i) 0)
        (track-set-note track i note)))))

;; TODO (make-euclidean-track 8 5)
;; TODO (make-track-from-list (list :A-5 nil :A-5 :nil :C-5 :nil))


(defparameter test-track (make-instance 'track :length 64))
(slot-value test-track 'notes)
(= (track-set-note test-track 122 0) 0)
(= (track-get-note test-track 122) 0)
(track-get-note-struct test-track 122)
(track-rotate test-track 5)

(defparameter test-etrack (make-instance 'track :length 5 :ticks-per-bar 5))
(track-set-euclidean test-etrack 2 39)
(slot-value test-etrack 'notes)
(track-get-note test-etrack 0)
(track-get-note-struct test-etrack 0)
(equalp (note-id #S(NOTE :ID "B 3" :OCTAVE 3 :NOTE :B :MIDI-VALUE 59 :FREQ 246.94165 :INDEX 38)) "B 3")

(track-get-note test-etrack 1)
(track-get-note-struct test-etrack 1)
(track-ticks-per-bar test-etrack)

(defparameter test-metronome (make-instance 'track :length 4 :ticks-per-bar 4))
(track-set-euclidean test-metronome 4 39)
(track-set-euclidean test-metronome 1 51)
(slot-value test-metronome 'notes)

;; Archived thinking out loud....

;; Am a little unsure on how we want to represent note values inside this array. Currently it's an
;; integer of some type, should maybe constrain to fixnum, but integer/fixnum makes sense from a
;; storage and fast arithmetic POV. We will want to be able to convert notes to/from:
;; - CV+Gate (1V/octave)
;; - MIDI value (21-108 ?)
;; - Frequency? (Hz)
;; For purposes of display we will want to be able to say a note is "C-4" or "G#2".
;; Display only makes sense at the sample rate of the ticks-per-bar at whatever the set bpm is
;; CV+Gate & Frequency are essentially at an "audio rate" sample rate.
;; So I think it makes sense to think of this sytem as having two different rates, the "audio/CV rate"
;; and the "control/ticks sample rate". We should be able to convert losslessly from control->audio
;; but risk losing artifacts on audio->control. Any tick-based UI is @ the control rate, and
;; during or before rendering will be converted to the audio rate-- modulations & fx & etc
;; will occur at the audio rate.

;; 1) The `track` class only needs to represent the necessary data at the control rate,
;; there will be a preprocessing step which converts it.
;; 2) For now, store notes as integer MIDI values where 21 = C-0(?)
;; 3) When we get into wanting to be able to do things like modulate note values using envelopes
;; it will have to become obvious that the displayed tick data is just one (convenient) among many
;; ways of generating the later control data
