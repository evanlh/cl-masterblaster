(defconstant +NOTE-NAMES+ (list :A :A# :B :C :C# :D :D# :E :F :F# :G :G#))
(defconstant +NOTE-MAX-VALUE+ 108)
(defconstant +NOTE-MIN-VALUE+ 1)
(defconstant +NOTE-NO-VALUE+ 0)
(defconstant +NOTE-OFF-VALUE+ 255)
(defconstant +NOTE-OFF-STRING+ "off")

(defstruct note
  (id nil :type string)
  (octave 0 :type fixnum)
  (note nil :type keyword)
  (midi-value 0 :type fixnum)
  (freq 0.0 :type single-float)
  (index 0 :type fixnum))


;; https://en.wikipedia.org/wiki/Piano_key_frequencies was helpful for infering these functions
(defun frequency-of-nth-key (n)
  (* 440.0 (expt 2 (/ (- n 49) 12))))

;; (frequency-of-nth-key 49)

(defun octave-of-nth-key (n)
  (1+ (floor (/ (- n 4) 12))))

;; (octave-of-nth-key 49)

(defun name-of-nth-key (n)
  (nth (mod (1- n) (length +NOTE-NAMES+)) +NOTE-NAMES+))

;; (name-of-nth-key 49) ;; => :A

(defun id-of-nth-key (n)
  (format nil "~2a~d"  (name-of-nth-key n) (octave-of-nth-key n)))

;; (id-of-nth-key 49) ;; "A 4"

(defparameter *notes-array* (make-array 108 :element-type 'note))

(dotimes (i 108)
  (let* ((n (1+ i))
        (freq (frequency-of-nth-key n))
        (midi-value (+ n 20))
        (octave (octave-of-nth-key n))
        (id (id-of-nth-key n))
        (note (name-of-nth-key n)))
    (setf (aref *notes-array* i)
          (make-note :id id :octave octave :note note :midi-value midi-value :freq freq :index i))))

(equalp (aref *notes-array* 50) #S(NOTE :ID "B 4" :OCTAVE 4 :NOTE :B :MIDI-VALUE 71 :FREQ 493.8833 :INDEX 50))
(equalp (aref *notes-array* 48) #S(NOTE :ID "A 4" :OCTAVE 4 :NOTE :A :MIDI-VALUE 69 :FREQ 440.0 :INDEX 48))
(equalp (aref *notes-array* 51) #S(NOTE :ID "C 5" :OCTAVE 5 :NOTE :C :MIDI-VALUE 72 :FREQ 523.2511 :INDEX 51))
(equalp (aref *notes-array* 87) #S(NOTE :ID "C 8" :OCTAVE 8 :NOTE :C :MIDI-VALUE 108 :FREQ 4186.009 :INDEX 87))
(equalp (aref *notes-array* 0) #S(NOTE :ID "A 0" :OCTAVE 0 :NOTE :A :MIDI-VALUE 21 :FREQ 27.5 :INDEX 0))

(defparameter *note-symbols-to-note-hash* (make-hash-table))

(dotimes (i 108)
  (let* ((n (1+ i))
         (key (format nil "~a~d" (name-of-nth-key n) (octave-of-nth-key n)))
         (keysym (intern key "KEYWORD") ))
    ;;(format t "~s  ~s ~%" keysym (aref *notes-array* i))
    (setf (gethash keysym *note-symbols-to-note-hash*) n))
  (setf (gethash :off *note-symbols-to-note-hash*) 255)
  (setf (gethash :non *note-symbols-to-note-hash*) 0))

;; TODO inline? macro?
(defun note (i)
  (if (= 0 i) nil
      (aref *notes-array* (1- i))))

;; Macro to convert a symbol, :d9, to its appropriate index
(defmacro n (note)
  `(if (keywordp ,note)
       (gethash ,note *note-symbols-to-note-hash*)
       (and (assert-valid-note ,note) ,note)))

(defmacro ns (note)
  `(aref *notes-array* (1- (n ,note))))

(defun assert-valid-note (n)
  (assert (and (integerp n) (not (or (> n 255) (< n 0) (and (> n 108) (< n 255)))))))

;; (assert-valid-note 255) ;; -> nil
;; (assert-valid-note 109) ;; -> fails!
;; (assert-valid-note 'test) ;; -> fails!
;; (assert-valid-note 1) ;; -> nil

(defun note-string-value (i)
  (cond ((= i 255) +NOTE-OFF-STRING+)
        ((and (> i 0) (<= i 108)) (note-id (note i)))
        (t nil)))

(defun note-integer-to-valid-index-with-coercion (note)
  (assert (typep note 'integer))
  (let ((coerced-note note))
    (cond
      ((> note +NOTE-OFF-VALUE+) (setf coerced-note +NOTE-NO-VALUE+))
      ((> note +NOTE-MAX-VALUE+) (setf coerced-note +NOTE-OFF-VALUE+))
      ((< note +NOTE-NO-VALUE+) (setf coerced-note +NOTE-OFF-VALUE+))
      ((< note +NOTE-MIN-VALUE+) (setf coerced-note +NOTE-NO-VALUE+)))
    coerced-note))

(assert (= (note-integer-to-valid-index-with-coercion 256) 0))
(assert (= (note-integer-to-valid-index-with-coercion -1) 255))
(assert (= (note-integer-to-valid-index-with-coercion 3) 3))
