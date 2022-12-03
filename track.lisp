(load "arrays.lisp")
(load "euclidean.lisp")

(defclass track ()
  ((length :initarg :length
           :initform (error "Must supply track length"))
   (notes)
   (ticks-per-bar :initarg :ticks-per-bar
                  :initform 4)))

(defmethod initialize-instance :after ((track track) &key)
  (let ((track-length (slot-value track 'length)))
    (setf (slot-value track 'notes) (make-array track-length :initial-element 0 :element-type 'integer))))

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

(defun track-length (track)
  (slot-value track 'length))

(defun track-ticks-per-bar (track)
  (slot-value track 'ticks-per-bar))

(defun track-set-note (track index note)
  (setf (aref (slot-value track 'notes) (mod index (track-length track))) note))

(defun track-get-note (track index)
  (aref (slot-value track 'notes) (mod index (track-length track))))

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

(defparameter test-track (make-instance 'track :length 64))
(slot-value test-track 'notes)
(track-set-note test-track 122 122)
(track-get-note test-track 122)
(track-rotate test-track 5)

(defparameter test-etrack (make-instance 'track :length 5 :ticks-per-bar 5))
(track-set-euclidean test-etrack 2 122)
(slot-value test-etrack 'notes)
(track-ticks-per-bar test-etrack)

