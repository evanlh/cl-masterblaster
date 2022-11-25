(defvar *bit-map* #(#*0000
                    #*0001
                    #*0010
                    #*0011
                    #*0100
                    #*0101
                    #*0110
                    #*0111
                    #*1000
                    #*1001
                    #*1010
                    #*1011
                    #*1100
                    #*1101
                    #*1110
                    #*1111))

(defun replace-bitvec (bitvec num pos)
  (unless (<= num 255)
    (error "Number must be <= 8 bits wide"))
  (let* ((up (ash num -4))
         (lo (- num (ash up 4))))
    (replace bitvec (aref *bit-map* up) :start1 pos :start2 0)
    (replace bitvec (aref *bit-map* lo) :start1 (+ pos 4) :start2 0)))

;; (defvar b (make-array (* 8 8) :element-type 'bit))
;; (replace-bitvec b 0 0)
;; (replace-bitvec b #xff 8)
;; (replace-bitvec b 256 0) ;; errors

(defun make-bitvec-from-bitlist (bitlist)
  (let* ((numlist (mapcar (lambda (x) (read-from-string (concatenate 'string "#X" x))) bitlist))
         (bitvec (make-array (* 8 (length numlist)) :element-type 'bit)))
    (dotimes (i (length numlist))
      (replace-bitvec bitvec (nth i numlist) (* 8 i)))
    bitvec))

;; (make-bitvec-from-bitlist '("ff" "00" "00" "00" "00" "00" "f0" "ff")) ;; => #*1111111100000000000000000000000000000000000000001111000011111111
;; (make-bitvec-from-bitlist '("ff" "00" "ff" "00")) ;; => #*11111111000000001111111100000000

(defun read-bdf-line (line)
  (let* ((tline (string-trim '(#\Space #\Tab #\Newline) line))
         (whitespace (search '(#\Space) tline)))
    (if whitespace
        (let* ((lhs (subseq tline 0 whitespace))
               (rhs (subseq tline (+ 1 whitespace) (length tline)))
               (sym (intern lhs "KEYWORD")))
          (list sym rhs))
        (if (> (length tline) 2)
            (intern tline "KEYWORD")
            (read-from-string (concatenate 'string "#X" tline))))))

;; (read-bdf-line "STARTFONT 2.1")
;; (read-bdf-line "DEFAULT_CHAR 255 ")
;; (read-bdf-line "BITMAP ")
;; (read-bdf-line "0f")

;; DEPRECATED ********************************
;; (defun bdf-list-to-bit-array (all-lines)
;;   (let ((result nil)
;;         (num-chars 0)
;;         (cur-char 0)
;;         (bitmap-index 0))
;;     (loop for line in all-lines do
;;       (cond ((typep line 'list)
;;              (cond ((eq (car line) :CHARS)
;;                     (setf num-chars (read-from-string (second line)))
;;                     (setf result (make-array num-chars :element-type 'array)))
;;                    ((eq (car line) :STARTCHAR)
;;                     (setf cur-char (read-from-string (concatenate 'string "#X" (subseq (second line) 1))))
;;                     (setf (aref result cur-char) (make-array (* 8 8) :element-type 'bit))
;;                     (setf bitmap-index 0))))
;;             ((and (typep line 'number))
;;              (setf (aref result cur-char) (replace-bitvec (aref result cur-char) line bitmap-index))
;;              (incf bitmap-index 8))))
;;     (values result num-chars cur-char)))


;; (defun read-bdf-file-into-list (filename)
;;   (with-open-file (in filename)
;;     (with-standard-io-syntax
;;       (let ((all-lines (list)))
;;         (loop for l = (read-line in nil nil)
;;               while l do (setf all-lines (append all-lines (list (read-bdf-line l)))))
;;         all-lines))))

;; (defvar fl (read-bdf-file-into-list "./Bauhaus.bdf"))
;; (setf fl (read-bdf-file-into-list "./Bauhaus.bdf"))

;; (defun read-bdf-file-into-bit-array (filename)
;;   "Extremely naive parser to convert BDF file specified by FILENAME into an array of bit vectors"
;;   (bdf-list-to-bit-array (read-bdf-file-into-list filename)))
;; END DEPRECATION


;; TODO move these two into string utils
;; find index of str in buffer
(defun find-position (buffer str)
  (position-if (lambda (s) (search str s)) buffer))

(defun split-on-space (str)
  (loop for i = 0 then (1+ j)
        as j = (position #\Space str :start i) ;; only works for non-repeats
        collect (subseq str i j)
        while j))

;; (split-on-space "THIS IS A TEST") ;; => ("THIS" "IS" "A" "TEST")

;; parse a single line: 1) split along spaces. 2) first element is always the prop name,
;; rest of elements are numbers or strings, unless only element is 2 digit hex number.
(defun parse-bdf-line (l)
  (let ((first-item (first l)))
    (cond ((> (length first-item) 2)
           (cons (intern first-item "KEYWORD") (mapcar #'read-from-string (rest l))))
          (t (list (read-from-string (concatenate 'string "#X" first-item)))))))

(defun linelist-to-bdf-list (raw-line-list)
  ;; TODO function composition
  (mapcar 'parse-bdf-line (mapcar #'split-on-space raw-line-list)))

;; (linelist-to-bdf-list '("STARTFONT 2.1" "FONT Bauhaus" "SIZE 8 75 75")) ;; => ((:STARTFONT 2.1) (:FONT BAUHAUS) (:SIZE 8 75 75))

;; turn the nested sortof-plists into a hash table, first element as key, remaining as val
(defun bdf-list-to-hash (bdf-list)
  (reduce (lambda (h l)
            (setf (gethash (first l) h) (if (> (length l) 2) (rest l) (second l))) h)
          bdf-list :initial-value (make-hash-table)))


(defun body-list-to-char-list (body-list numchars character-props character-bits)
  ;; a copy of the body list, already split to start at STARTCHAR
  (let* ((body-copy (subseq body-list 0 (length body-list)))
         (bitmap-start 0)
         (end 0))
    ;; for each character (STARTCHAR/ENDCHAR block)
    (dotimes (i numchars)
      ;; find the first occurrence of the BITMAP string
      (setf bitmap-start (find-position body-copy "BITMAP"))
      (when (eq nil bitmap-start)
        (error "Missing BITMAP block"))
      ;; parse the lines between STARTCHAR and BITMAP into a hash
      (let ((charhash (bdf-list-to-hash
                       (linelist-to-bdf-list (subseq body-copy 0 bitmap-start)))))
        ;; "BBX 8 16 0 -2" declares a bounding box that is 8 pixels wide and 16 pixels tall.
        ;; The lower left-hand corner of the character is offset by 0 pixels on the X-axis and
        ;; -2 pixels on the Y-axis.
        ;; "ENCODING 65" declares the decimal code point for this glyph in the font.

        ;; pull the BBX x/y coords and ENCODING values out of the hash,
        ;; since they determine how many lines after BITMAP should occur
        (let ((y (second (gethash :BBX charhash)))
              (encoding (gethash :ENCODING charhash)))
          ;; store the hash @ the character-props array index corresponding to ENCODING value
          (setf (aref character-props encoding) charhash)
          ;; parse y lines after BITMAP into a bitvector &
          ;; put that bitvector into character-bits @ ENCODING index
          ;; TODO there are some dependencies on x being 8 wide here
          (setf (aref character-bits encoding)
                (make-bitvec-from-bitlist (subseq body-copy (1+ bitmap-start) (+ 1 bitmap-start y))))))

      ;; next find the first occurrence of the ENDCHAR string
      (setf end (find-position body-copy "ENDCHAR"))
      ;; return or bail if not found
      (when (eq end nil)
        (if (eq i numchars)
            (return (list character-props character-bits))
            (error "Terminating ENDCHAR not found")))
      ;; fast-forward body-copy to one after the current end for the next iteration
      (setf body-copy (nthcdr (1+ end) body-copy)))
    (list character-props character-bits)))


(defun load-bdf-file (filename)
  "Loads and parses BDF file specified by FILENAME, returning a hash-table key/value pairs
of the properties provided in the header, an array of hash tables at :CHARPROPS and
an equal sized array of bitvectors at :CHARBITS"
  (let* ((filebuf (with-open-file (stream filename)
                    (loop for line = (read-line stream nil)
                          while line
                          collect line)))
         (startchar-pos (find-position filebuf "STARTCHAR"))
         (header-list (subseq filebuf 0 startchar-pos))
         (body-list (subseq filebuf startchar-pos (length filebuf)))
         (header-hash (bdf-list-to-hash (linelist-to-bdf-list header-list)))
         (num-chars (gethash :CHARS header-hash))
         (character-bits (make-array num-chars))
         (character-props (make-array num-chars))
         (parsed-body-list (body-list-to-char-list body-list num-chars character-props character-bits)))
    (setf (gethash :CHARPROPS header-hash) (first parsed-body-list))
    (setf (gethash :CHARBITS header-hash) (second parsed-body-list))
    header-hash))


;; (defparameter bauhausfont (load-bdf-file "Bauhaus.bdf"))
;; (equal (aref (gethash :CHARBITS bauhausfont) 72) #*0110011001100110011001100111011001110110011001100110011001100110) ;; => T
;; (equal (gethash :CHARS bauhausfont) 298) ;; => T
;; ;; kinda weird it's a symbol? artifact of using read-from-string
;; (equal (gethash :FONT bauhausfont) 'BAUHAUS) ;; => T
;; (equal (gethash :BBX (aref (gethash :CHARPROPS bauhausfont) 72)) '(8 8 0 -1)) ;; => T



