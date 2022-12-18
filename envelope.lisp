(defstruct point
  (x 0 :type single-float)
  (y 0 :type single-float))

(defparameter env2 (make-array 3 :element-type 'point))
(setf (aref env2 0) (make-point :x 0.0 :y 0.0))
(setf (aref env2 1) (make-point :x 0.01 :y 1.0))
(setf (aref env2 2) (make-point :x 1.0 :y 0.0))

(defun envelopep (env)
  (and (typep env '(array 'point)) (>= (length env) 2)))

(defun make-envelope (&rest coords)
  "Create an envelope structure from the list of coordinate pairs x1, y1, x2, y2, ... xn, yn.
Coordinates must be listed in ascending order along the X axis and should not overlap."
  (assert (= 0 (mod (length coords) 2)))
  (let ((env (make-array (/ (length coords) 2) :element-type 'point))
        (i 0)
        (last-x nil))
    (loop while coords do
      (let ((x (pop coords))
            (y (pop coords)))
        (when (= 0 i)
          (assert (= x 0.0)))
        (assert (and (>= y 0.0) (<= y 1.0)))
        (assert (or (not last-x) (> x last-x)))
        (setf (aref env i) (make-point :x x :y y))
        (setf last-x x)
        (incf i)))
    env))

(make-envelope 0.0 0.0 0.1 1.0 0.5 1.0 1.0 0.0)

(envelopep (make-envelope 0.0 0.0 0.1 1.0 0.5 1.0 1.0 0.0))

(defun scale-envelope-x! (env xfactor)
  (dotimes (i (length env))
    (setf (point-x (aref env i)) (* xfactor (point-x (aref env i)))))
  env)

(equalp (scale-envelope-x! (make-envelope 0.0 0.0 0.1 1.0 0.5 1.0 1.0 0.0) 10)
        #(#S(POINT :X 0.0 :Y 0.0) #S(POINT :X 1.0 :Y 1.0) #S(POINT :X 5.0 :Y 1.0) #S(POINT :X 10.0 :Y 0.0)))

(defun envelope-length (env)
  (point-x (aref env (1- (length env)))))

(= (envelope-length (make-envelope 0.0 1.0 2.0 1.0 3.0 0.0)) 3.0)

(defun envelope-x-at (env i)
  (point-x (aref env i)))

(= (envelope-x-at (make-envelope 0.0 1.0 2.0 1.0 3.0 0.0) 1) 2.0)

(defun envelope-interpolator (env)
  (let* ((apos 0)
         (p1 (aref env apos))
         (lastpos (- (length env) 2))
         (x1 (point-x p1))
         (y1 (point-y p1))
         (p2 (aref env (1+ apos)))
         (x2 (point-x p2))
         (y2 (point-y p2))
         (rise (- y2 y1))
         (run (- x2 x1))
         (slope (/ rise run))
         (lastx 0))
    (lambda (px)
      ;; (format t "x1 ~d x2 ~d y1 ~d y2 ~d~%" x1 x2 y1 y2)
      (when (< px lastx)
        (error "This is a forward-only interpolator. Successive calls must be >= prior calls."))
      (setf lastx px)
      (if (> px x2)
        (if (= apos lastpos)
            y2
            (progn
              (loop until (or (= apos lastpos) (and (>= px x1) (<= px x2))) do
                (incf apos)
                (setf p1 (aref env apos))
                (setf x1 (point-x p1))
                (setf y1 (point-y p1))
                (setf p2 (aref env (1+ apos)))
                (setf x2 (point-x p2))
                (setf y2 (point-y p2))
                (setf rise (- y2 y1))
                (setf run (- x2 x1))
                (setf slope (/ rise run))
                ;; (format t "loop x1 ~d x2 ~d y1 ~d y2 ~d~%" x1 x2 y1 y2)
                ;; (format t "retval ~d~%" (+ y1 (* (- px x1) slope)))
                    )
              (+ y1 (* (- px x1) slope))))
        (+ y1 (* (- px x1) slope))))))


(defparameter ei (envelope-interpolator (scale-envelope-x! (make-envelope 0.0 0.0 0.1 1.0 0.5 1.0 1.0 0.0) 100)))
(equalp (mapcar ei (list 0 1 10 20 60 70 90 100 101 110 120))
        '(0.0 0.1 1.0 1.0 0.8 0.6 0.20000005 0.0 0.0 0.0 0.0))

(mapcar (envelope-interpolator (make-envelope 0.0 1.0 1.0 0.0)) '(0.0 0.25 0.5 0.75 1.0 1.25))
(mapcar (envelope-interpolator (make-envelope 0.0 1.0 0.5 0.8 1.0 0.0)) '(0.0 0.25 0.5 0.75 1.0 1.25))


(defun envelope-interpolator-looping (env &key (loop-p t) (loop-start 0) (loop-end (point-x (aref env (1- (length env))))))
  (let* ((endpos (1- (length env)))
         (end-x (point-x (aref env endpos)))
         (lastpos (1- endpos)))
    (assert (and (< loop-start end-x) (<= loop-end end-x)
                 (>= loop-start 0) (> loop-end 0)))
    (lambda (px)
      (let* ((adjustedpx  (if loop-p
                              (mod (+ loop-start px) (- loop-end loop-start))
                              (min (+ loop-start px) loop-end)))
             (apos 0)
             (p1 (aref env apos))
             (x1 (point-x p1))
             (p2 (aref env (1+ apos)))
             (x2 (point-x p2)))
        ;; fast-forward array position until we're between x1&x2 or we're at the end of the array
        (loop until (or (= apos lastpos) (and (>= adjustedpx x1) (<= adjustedpx x2))) do
          (incf apos)
          (setf p1 (aref env apos))
          (setf x1 (point-x p1))
          (setf p2 (aref env (1+ apos)))
          (setf x2 (point-x p2)))
        ;; now that we're in the right spot, calculate rise/run/slope & interpolate
        (let* ((y1 (point-y p1))
               (y2 (point-y p2))
               (rise (- y2 y1))
               (run (- x2 x1))
               (slope (/ rise run)))
          (+ y1 (* (- adjustedpx x1) slope)))))))

(equalp (mapcar (envelope-interpolator-looping (make-envelope 0.0 0.0 0.5 1.0 1.0 0.0)) '(0.0 0.25 0.5 0.75 1.0 1.25 1.5 1.75 2.0)) '(0.0 0.5 1.0 0.5 0.0 0.5 1.0 0.5 0.0))

(equalp (mapcar (envelope-interpolator-looping (make-envelope 0.0 0.0 0.5 1.0 1.0 0.0) :loop-start 0.5) '(0.0 0.25 0.5 0.75 1.0 1.25 1.5 1.75 2.0)) '(1.0 0.5 1.0 0.5 1.0 0.5 1.0 0.5 1.0))

(equalp (mapcar (envelope-interpolator-looping (make-envelope 0.0 0.0 0.5 1.0 1.0 0.0) :loop-end 0.5) '(0.0 0.25 0.5 0.75 1.0 1.25 1.5 1.75 2.0)) '(0.0 0.5 0.0 0.5 0.0 0.5 0.0 0.5 0.0))

(equalp (mapcar (envelope-interpolator-looping (scale-envelope-x! (make-envelope 0.0 1.0 1.0 0.0) 100)) '(0 10 20 30 40 50 60 70 80 90 100)) '(1.0 0.9 0.8 0.70000005 0.6 0.5 0.40000004 0.3 0.20000005 0.100000024 1.0))

(equalp (mapcar (envelope-interpolator-looping (make-envelope 0.0 0.0 0.5 1.0 1.0 0.0) :loop-start 0.25 :loop-end 0.75) '(0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0)) '(0.5 0.7 0.9 0.100000024 0.29999995 0.5 0.70000005 0.9 0.099999905 0.29999995 0.5))

(equalp (mapcar (envelope-interpolator-looping (make-envelope 0.0 0.0 0.5 1.0 1.0 0.0) :loop-p nil :loop-start 0.25 :loop-end 0.75) '(0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0)) '(0.5 0.7 0.9 0.9 0.70000005 0.5 0.5 0.5 0.5 0.5 0.5))

;; (mapcar (envelope-interpolator-looping (scale-envelope-x! (make-envelope 0.0 1.0 1.0 0.0) 100) :loop-start 25) '(0 10 20 30 40 50 60 70 80 90 100)) '(0.75 0.65 0.55 0.45 0.35000002 1.0 0.9 0.8 0.70000005 0.6 0.5)

(equalp (mapcar (envelope-interpolator-looping (make-envelope 0.0 1.0 1.0 0.0 2.0 1.0 3.0 0.0 4.0 1.0) :loop-start 2.0 :loop-end 3.0) '(0.0 0.25 0.5 0.75 1.0 1.25 1.5 1.75 2)) (mapcar (envelope-interpolator-looping (make-envelope 0.0 1.0 1.0 0.0 2.0 1.0 3.0 0.0 4.0 1.0) :loop-start 1.0 :loop-end 2.0) '(0.0 0.25 0.5 0.75 1.0 1.25 1.5 1.75 2)))


(plot (envelope-interpolator-looping (make-envelope 0.0 1.0 1.0 0.0 2.0 1.0 3.0 0.0 4.0 1.0) :loop-start 2.0 :loop-end 3.0) 0.0 4.0)

(plot (envelope-interpolator-looping (make-envelope 0.0 0.0 0.5 1.0 1.0 0.0)
                                     :loop-start 0.25 :loop-end 0.75
                                     ))

(defun interpolate-line-at-point (y1 y2 x1 x2 px)
  (let* ((rise (- y2 y1))
         (len (- x2 x1))
         (slope (/ rise len))
         (py (+ y1 (* px slope))))
    py))

(= (interpolate-line-at-point 0 10 0 5 10) 20)

(defun line-interpolator (y1 y2 x1 x2)
  (let* ((rise (- y2 y1))
         (len (- x2 x1))
         (slope (/ rise len)))
    (lambda (px)
      (+ y1 (* px slope)))))

(= (funcall (line-interpolator 0 10 0 5) 10) 20)
(= (funcall (line-interpolator 0 10 0 10) 8) 8)


(defun interpolate-line-over-buffer (buffer y1 y2 x1 x2)
  (let* ((rise (- y2 y1))
         (len (- x2 x1))
         (slope (/ rise len)))
    (dotimes (i len)
      (let ((y3 (+ y1 (* i slope)))
            (v (aref buffer (+ x1 i))))
        (setf (aref buffer (+ x1 i)) (* v y3))))))

;; (interpolate-line-over-buffer test-buffer 1.0 0.0 0 (length test-buffer))
;; (interpolate-line-over-buffer test-buffer 0.0 1.0 0 (length test-buffer))

(defun interpolate-envelope-over-buffer (env buffer &optional (start 0) (end (length buffer)))
  (let* ((len (- end start)))
    (dotimes (i (1- (length env)))
      (let* ((x1 (point-x (aref env i)))
             (y1 (point-y (aref env i)))
             (epos (1+ i))
             (x2 (point-x (aref env epos)))
             (y2 (point-y (aref env epos))))
        (interpolate-line-over-buffer buffer y1 y2 (+ start (floor (* x1 len))) (+ start (floor (* x2 len))))))))

;; (interpolate-envelope-over-buffer env2 test-buffer)
;; (interpolate-envelope-over-buffer env2 test-buffer 0 22050)
;; (interpolate-envelope-over-buffer env2 test-buffer 22050 44100)

