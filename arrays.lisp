(defun array-rotate-l (arr)
  "Destructively rotate array values left 1 position"
  (when (= 0 (length arr))
    arr)
  (let ((tmp (aref arr 0))
        (l (length arr)))
    (dotimes (i (1- l))
      (setf (aref arr i) (aref arr (1+ i))))
    (setf (aref arr (1- l)) tmp))
  arr)

(equalp (array-rotate-l #(0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 0)) #(0 1 0 0 1 0 0 1 0 0 1 0 0 1 0 0))
(equalp (array-rotate-l #(0 1)) #(1 0))
(equalp (array-rotate-l #(0 0 1)) #(0 1 0))

(defun array-rotate-r (arr)
  "Destructively rotate array values right 1 position"
  (when (= 0 (length arr))
    arr)
  (let* ((l (length arr))
         (tmp (aref arr (1- l))))
    (dotimes (i (1- l))
      (setf (aref arr (- l i 1)) (aref arr (- l i 2))))
    (setf (aref arr 0) tmp))
  arr)

(equalp (array-rotate-r #(0 0 1)) #(1 0 0))
(equalp (array-rotate-r #(0 0 0 1 0)) #(0 0 0 0 1))
(equalp (array-rotate-r #(0 0 1 0 0 1 0 0 1 0 0 1 0 0 1 0)) #(0 0 0 1 0 0 1 0 0 1 0 0 1 0 0 1))

(defun array-rotate (arr n)
  (let ((rotations (mod n (length arr))))
    (when (or (= 0 (length arr)) (= 0 rotations))
      arr)
    (if (< rotations 0)
        (dotimes (i (- rotations)) (array-rotate-l arr))
        (dotimes (i rotations) (array-rotate-r arr)))
    arr))