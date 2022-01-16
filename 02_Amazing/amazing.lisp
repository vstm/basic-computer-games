(setf *random-state* (make-random-state T))

(defparameter *directions*
  '(go-up go-down go-left go-right))

(defconstant +exit-down+ 1)
(defconstant +exit-right+ 2)

(defun invalid-directions (col row width height used)
  (remove nil
          (list
           (when (or (= col 0) (/= (aref used row (1- col)) 0)) 'go-left)
           (when (or (= row 0) (/= (aref used (1- row) col) 0)) 'go-up)
           (when (or (= col (1- width)) (/= (aref used row (1+ col)) 0)) 'go-right)
           (when (or (= row (1- height)) (/= (aref used (1+ row) col) 0)) 'go-down))))

(defun valid-directions (col row width height used)
  (set-difference *directions* (invalid-directions col row width height used) :test 'equal))

(defun pick-direction (directions)
  (nth (random (length directions)) directions))

(defun next-used-cell (row-in col-in width height used)
  (destructuring-bind (row col)
      (cond
       ((/= col-in (1- width)) (list row-in (1+ col-in)))
       ((/= row-in (1- height)) (list (1+ row-in) 0))
       (t (list 0 0)))
    (if (/= (aref used row col) 0)
        (list row col)
        (next-used-cell row col width height used))))

(defun build-maze-step-go-direction (direction row-in col-in used walls step-count)
  (assert (member direction *directions*))
  (destructuring-bind (post-op? new-row new-col arg)
      (case direction
        (go-left (list T row-in (1- col-in) +exit-right+))
        (go-up (list T (1- row-in) col-in +exit-down+))
        (go-right (list nil row-in (1+ col-in) +exit-right+))
        (go-down (list nil (1+ row-in) col-in +exit-down+)))
    (if post-op?
        (setf (aref walls new-row new-col) arg)
        (incf (aref walls row-in col-in) arg))
    (setf (aref used new-row new-col) step-count)
    (list new-row new-col)))

(defun build-maze-step (col-in row-in width height walls used step-count)
  (let ((directions (valid-directions col-in row-in width height used)))
    (if directions
        (cons (1- step-count) (build-maze-step-go-direction (pick-direction directions) row-in col-in used walls step-count))
        (cons step-count (next-used-cell row-in col-in width height used)))))

(defun build-maze-loop (col row width height walls used step-count)
  (destructuring-bind (next-step-count next-row next-col)
      (build-maze-step col row width height walls used step-count)
    (when (> next-step-count 0)
          (build-maze-loop next-col next-row width height walls used next-step-count))))

(defun build-maze (width height entry-col exit-col)
  (let ((used (make-array (list height width) :element-type 'integer))
        (walls (make-array (list height width) :element-type 'integer))
        (step-count (* width height)))
    (setf (aref walls 0 entry-col) 1)
    (build-maze-loop entry-col 0 width height walls used step-count)
    (incf (aref walls (1- height) exit-col))
    walls))

(defun print-maze (width height entry-col walls)
  (format t "~{~A~}.~&" (loop for col from 0 below width collect (if (= col entry-col) ".  " ".--")))
  (loop for row from 0 below height do
          (format t "I~{~A~}~&" (loop for col from 0 below width collect (if (< (aref walls row col) 2) "  I" "   ")))
          (format t "~{~A~}.~&" (loop for col from 0 below width collect (if (or (= (aref walls row col) 0) (= (aref walls row col) 2)) ":--" ":  ")))))

(defun build-and-print-maze (width height entry-col exit-col)
  (print-maze width height entry-col (build-maze width height entry-col exit-col)))

(defun tab (n)
  (format nil "~v@{~A~:*~}" n " "))

(defun print-header ()
  (format t "~aAMAZING PROGRAM~&" (tab 28))
  (format t "~aCREATIVE COMPUTING  MORRISTOWN, NEW JERSEY~&" (tab 15)))

(defun read-number ()
  (let* ((line (read-line *query-io*))
         (parsed-int (parse-integer line :junk-allowed T))
         (number-value parsed-int))
    (if number-value
        number-value
        (progn (format t "~a IS NOT A NUMBER~&" line) (read-number)))))

(defun query-dimensions ()
  (format *query-io* "WHAT ARE YOUR WIDTH AND LENGTH ")
  (finish-output *query-io*)
  (let ((width (read-number))
        (height (read-number)))
    (if (or (<= width 1) (<= height 1))
        (progn
         (format *query-io* "MEANINGLESS DIMENSIONS.  TRY AGAIN.~&")
         (query-dimensions))
        (list width height))))

(defun main ()
  (print-header)
  (destructuring-bind (width height) (query-dimensions)
    (let ((entry-col (random width))
          (exit-col (random width)))
      (build-and-print-maze width height entry-col exit-col))))