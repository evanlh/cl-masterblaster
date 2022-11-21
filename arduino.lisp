(defvar dc 16)
(defvar cs 18)

(defvar CASET #x2A)
(defvar RASET #x2B)
(defvar RAMWR #x2C)

(defvar yoff 0)
(defvar xoff 0)
(defvar xsize 320)
(defvar ysize 240)

(defun rgb (r g b)
 (logior (ash (logand r #xf8) 8) (ash (logand g #xfc) 3) (ash b -3)))

(defun cmd (c &rest data)
  (with-spi (str cs)
    (digitalwrite dc 0)
    (write-byte c str)
    (digitalwrite dc 1)
    (dolist (d data)
      (write-byte d str))))

(defun init ()
  (pinmode dc t)
  (cmd #x01)      ; Software reset
  (delay 150)     ; delay 150 ms
  (cmd #x11)      ; Out of sleep mode
  (delay 500)     ; delay 500 ms
  (cmd #x3A #x05) ; Set color mode - 16-bit color
  (cmd #x29)      ; Display on
  (delay 100))

(defun clear (&optional j)
  (let ((c (if j #xFF 0)))
    (cmd CASET 0 yoff 0 (+ yoff ysize -1))
    (cmd RASET 0 xoff 0 (+ xoff xsize -1))
    (cmd #x3A #x03)
    (cmd RAMWR)
    (with-spi (str cs)
      (dotimes (p (* (/ xsize 2) ysize 3))
        (write-byte c str)))
    (cmd #x3A #x05)))

(defun point (x y c)
  (cmd CASET 0 (+ yoff y) 0 (+ yoff y))
  (cmd RASET 0 (+ xoff x) 0 (+ xoff x))
  (cmd RAMWR (ash c -8) (logand c #xff)))
