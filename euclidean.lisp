(load "arrays.lisp")

;; more or less copied from https://github.com/brianhouse/bjorklund/blob/master/__init__.py
(defun bjorklund (pulses steps)
  "Create and return an array of PULSES evenly distributed over STEPS according to Bjorklund's SNS
Timing Systems algorithm."
  (when (> pulses steps)
    (error "pulses must be <= steps"))
  (let ((bitmap (make-array 0 :initial-element 0 :element-type 'integer :fill-pointer t :adjustable t))
        (counts (make-array 0 :initial-element 0 :element-type 'integer :fill-pointer t :adjustable t))
        (remainders (make-array 0 :initial-element 0 :element-type 'integer :fill-pointer t :adjustable t))
        (divisor (- steps pulses))
        (level 0))
    (vector-push-extend pulses remainders)
    (loop
       (vector-push-extend (floor (/ divisor (aref remainders level))) counts)
       (vector-push-extend (mod divisor (aref remainders level)) remainders)
       (setf divisor (aref remainders level))
       (incf level)
       (when (<= (aref remainders level) 1)
         (return)))
    (vector-push-extend divisor counts)
    (labels ((build (lvl)
               (if (= lvl -1)
                   (vector-push-extend 0 bitmap)
                   (if (= lvl -2)
                       (vector-push-extend 1 bitmap)
                       (progn
                         (dotimes (i (aref counts lvl))
                           (build (1- lvl)))
                         (when (/= 0 (aref remainders lvl))
                           (build (- lvl 2))))))))
      (build level)
      bitmap)))

(bjorklund 5 8) ;; => #(1 0 1 1 0 1 1 0)
(bjorklund 3 8) ;; => #(0 1 0 0 1 0 0 1)
(bjorklund 2 4) ;; => #(0 1 0 1)
(bjorklund 5 13)  ;; => #(0 1 0 0 1 0 1 0 0 1 0 1 0)
(bjorklund 7 16) ;; => #(0 1 0 1 0 1 0 0 1 0 1 0 1 0 0 1)

(defun euclidean-rhythm (pulses steps)
  "This calls bjorklund and adjusts the output so the first step is always a beat"
  (let ((out (bjorklund pulses steps)))
    (loop (when (= 1 (aref out 0)) (return))
          (array-rotate-l out))
    out))

(euclidean-rhythm 5 13) ;; => #(1 0 0 1 0 1 0 0 1 0 1 0 0)
(euclidean-rhythm 3 8) ;; => #(1 0 0 1 0 0 1 0)
