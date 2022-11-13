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
  ;; TODO assert int is 8 bits or less
  (let* ((up (ash num -4))
         (lo (- num (ash up 4))))
    (replace bitvec (aref *bit-map* up) :start1 pos :start2 0)
    (replace bitvec (aref *bit-map* lo) :start1 (+ pos 4) :start2 0)))

;; (defvar b (make-array (* 8 8) :element-type 'bit))
;; (replace-bitvec b 0 0)
;; (replace-bitvec b #xff 8)

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

#+nil (defun add-line (all-lines next-line)
  (let ((last-pair] (last all-lines 2))) ;; last is inefficient but not sure it matters here
    (cond ((= (length last-pair) 0) (append all-lines next-line))
          ((and (= (length last-pair) 2) (= (length next-line) 2))
           (cond ((eq (first next-line) :STARTCHAR)
                  (append all-lines next-line (list :CHAR_LIST (list))))
                 ((eq (first next-line) :BITMAP)
                  (append all-lines ))
                 (t (if (typep (second last-pair) 'list)
                        ;; append to last inner list
                        ;; append to end
                        (append all-lines next-line))
                    )))
          (()))))

#+nil (defun bdf-list-to-treeq (all-lines)
  (let ((result (list)))
    (loop for line in all-lines do
      (let ((key (second result)))
        (cond ((eq nil key)
               (push (car line) result)
               (push (second line) result))
              ((and (eq key :CHAR_LIST) (typep line 'list))
               (push (car line) (car result))
               (push (second line) (car result)))
              ((typep line 'list)
               (print line)
               (cond ((eq (car line) :STARTCHAR)
                      (push (car line) result)
                      (push (second line) result)
                      (push :CHAR_LIST result)
                      (push '() result))
                     )))))
    (nreverse result)))

(defun bdf-list-to-bit-array (all-lines)
  (let ((result nil)
        (num-chars 0)
        (cur-char 0)
        (bitmap-index 0))
    (loop for line in all-lines do
      (cond ((typep line 'list)
             (cond ((eq (car line) :CHARS)
                    (setf num-chars (read-from-string (second line)))
                    (setf result (make-array num-chars :element-type 'array)))
                   ((eq (car line) :STARTCHAR)
                    (setf cur-char (read-from-string (concatenate 'string "#X" (subseq (second line) 1))))
                    (setf (aref result cur-char) (make-array (* 8 8) :element-type 'bit))
                    (setf bitmap-index 0))))
            ((and (typep line 'number))
             (setf (aref result cur-char) (replace-bitvec (aref result cur-char) line bitmap-index))
             (incf bitmap-index 8)

             (print "cur char")
             (print cur-char)
             ;; (if (> line 0)
             ;;     (progn
             ;;       (print "line ")
             ;;       (print line)
             ;;       (print "vec ")
             ;;       (print (aref result cur-char))
             ;;       (print "bitmap index ")
             ;;       (print bitmap-index)))
             )
            ))
    (values result num-chars cur-char)))


(defun read-bdf-file-into-list (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (let ((all-lines (list)))
        (loop for l = (read-line in nil nil)
              while l do (setf all-lines (append all-lines (list (read-bdf-line l)))))
        all-lines))))

(defvar fl (read-bdf-file-into-list "./Bauhaus.bdf"))
(setf fl (read-bdf-file-into-list "./Bauhaus.bdf"))


#+nil (defun collect-lines (line globalprops chars context)
  (let ((parsed (read-bdf-line line)))
    (cond ((typep parsed 'keyword)
           (cond ((eq parsed :BITMAP) (list globalprops chars :BITMAP))
                 ((eq parsed :ENDCHAR) (list globalprops chars nil))
                 (t (list globalprops chars context))))
          ((typep parsed 'list)
           (cond ((eq (first parsed) :STARTFONT) (list globalprops chars :STARTFONT))
                 ((eq (first parsed) :STARTCHAR)
                  (setf chars (cons (make-hash-table) chars))
                  (print (second parsed))
                  (list globalprops chars :STARTCHAR))
                 (t (cond ((eq context :STARTFONT)
                           (setf (gethash (first parsed) globalprops) (second prsed))
                           (list globalprops chars :STARTFONT))
                          ((eq context :STARTCHAR)
                           (setf (gethash (first parsed) (car chars)) (second parsed))
                           (list globalprops chars :STARTCHAR))
                          ;; TODO throw an error here?
                          (t (print "got a list outside of STARTFONT/STARTCHAR"))
                          ))))
          ((and (typep parsed 'number) (eq context :BITMAP))
           ;; initialize BITMAP value to an 8x8 bit array & 0 the index
           (if (not (gethash :BITMAP (car chars)))
               (progn
                 (setf (gethash :BITMAP (car chars)) (make-array (* 8 8) :element-type 'bit))
                 (setf (gethash :BITMAPINDEX (car chars)) 0)

                 ))
           ;; copy the integer value as bits into the array
           (replace-bitvec (gethash :BITMAP (car chars)) parsed (gethash :BITMAPINDEX (car chars)))
           ;; increment the index
           (incf (gethash :BITMAPINDEX (car chars)))
           ;; (print "incd hash")
           ;; (print (gethash :BITMAPINDEX (car chars) 0))
           ;; (setf (gethash :BITMAP (car chars)) (append (or (gethash :BITMAP (car chars)) (list)) parsed))
           (list globalprops chars context))
          (t (print "Unknown type, aborting")))))

;; loop over all lines of input
;; when you're between the string matching STARTFONT and CHARS ("global properties") collect all properties into a plist (converting the LHS string to a keyword)
;; when you're between STARTCHAR and ENDCHAR collect all props into a per char plist
;; when you're between BITMAP and ENDCHAR read the numbers as hex and store them in an array at the position of the character indicated by STARTCHAR

#+nil (defun collect (line output)
  (print "collect")
  (print line)
  (print output)
  (let* ((g (or (and output (first output)) globalprops))
         (c (or (and output (nth 1 output)) chars))
         (context (and output (nth 2 output)))
         (output (collect-lines line g c context)))
    output))

#+nil (defun load-font (filename)
  (let* ((globalprops (make-hash-table))
         (chars (list))
         )
    (with-open-file (in filename)
      (with-standard-io-syntax
        (loop
          for l = (read-line in nil nil)
          for out = (funcall collect l out)
;;            then (collect-lines (read-line in nil nil) (first l) (nth 1 l) (nth 2 l))
          while l do (let ((*print-readably* nil))
                       (print out)
                       (print (nth 1 out))
                       )
          (return out))))))



;; (defun load-font (filename)
;;   (let ((globalprops (make-hash-table))
;;         (chars (list)))
;;     (print globalprops)
;;     (print chars)

;;     (with-open-file (in filename)
;;       (with-standard-io-syntax
;;         (loop
;;           for l = (collect-lines (read-line in nil nil) globalprops chars nil)
;;             then (collect-lines (read-line in nil nil) (first l) (nth 1 l) (nth 2 l))
;;           while l do (let ((*print-readably* nil))
;;                        ;; (print (nth 1 l))
;;                        )
;;           )))
;;     (print globalprops)
;;     (print chars)
;;     (list globalprops chars)))


;; Substring compare, more or less
;; (string-equal "te" "test" :end1 2 :end2 2)

;; split string on first whitespace

;; convert string to symbol? create a plist from "FONT Bauhaus" => :FONT "Bauhaus"
;; (intern "mysym" "KEYWORD")
;; get an element from a plist
;; (getf (list :a 1 :b 2 :c 3) :a) => 1

;; Get a subsequence
;; (subseq "testing" 0 2) => "te"
;; search for a substring
;; (search "we" "if we can't be free") => 3

;; number from string rep:
;; (read-from-string "#X23")
;; format number as binary:
;; (format nil "~,'0b" 5)

