(defun tab (n)
  (format nil "~v@{~A~:*~}" n " "))

(defun print-header ()
  (format t "~aANIMAL~&" (tab 32))
  (format t "~aCREATIVE COMPUTING  MORRISTOWN, NEW JERSEY~&" (tab 15)))

(defun make-node (text &optional (yes-node nil) (no-node nil))
  (list :text text :yes-node yes-node :no-node no-node))

(defun yes-node (node)
  (getf node :yes-node))
(defun no-node (node)
  (getf node :no-node))

(defun node-text (node)
  (getf node :text))

(defun node-is-leaf-p (node)
  (and (not (yes-node node)) (not (no-node node))))

(defun animal-step-ask-question (node)
  (let ((answer (y-or-n-p "~a" (node-text node))))
    (if answer :yes-node :no-node)))

(defun query-yes-no-list (query &rest args)
  (loop
        (apply #'format *query-io* query args)
        (format *query-io* " (y[es], n[o] OR list) ")
        (force-output *query-io*)
        (let ((answer (string-downcase (string-trim " " (read-line *query-io*)))))
          (cond
           ((or (string-equal answer "yes") (string-equal answer "y")) (return t))
           ((or (string-equal answer "no") (string-equal answer "n")) (return nil))
           ((string-equal answer "list") (return :list))
           (t (format *query-io* "INPUT MUST BE yes, no OR list, ~a GIVEN~&" answer))))))

(defun query-non-empty-string (query &rest args)
  (loop
        (apply #'format *query-io* query args)
        (force-output *query-io*)
        (let ((answer (string-trim " ?" (read-line *query-io*))))
          (when (string-not-equal answer "") (return answer)))))

(defun query-new-animal (node rpath)
  (let* ((new-animal (query-non-empty-string "THE ANIMAL YOU WERE THINKING OF WAS A "))
         (new-question (query-non-empty-string "PLEASE TYPE IN A QUESTION THAT WOULD DISTINGUISH A ~a FROM A ~a: " new-animal (node-text node)))
         (new-question-answer (y-or-n-p "FOR A ~a THE ANSWER WOULD BE" new-animal)))
    (let ((new-animal-node (make-node new-animal))
          (old-animal-node (make-node (node-text node))))
      (list :path (reverse rpath)
            :old-node node
            :new-node (make-node (concatenate 'string new-question "?")
                                 (if new-question-answer new-animal-node old-animal-node)
                                 (if new-question-answer old-animal-node new-animal-node))))))

(defun animal-step-identify-animal (node rpath)
  (let ((answer (y-or-n-p "IS IT A ~a?" (getf node :text))))
    (if answer
        nil
        (query-new-animal node rpath))))

(defun animal-step (node &optional (rpath (list)))
  (if (node-is-leaf-p node)
      (animal-step-identify-animal node rpath)
      (let ((next-node (animal-step-ask-question node)))
        (animal-step (getf node next-node) (cons next-node rpath)))))

(defun update-node-step (node path new-node)
  (let ((path-elem (car path))
        (path-rest (cdr path)))
    (if path-rest
        (update-node-step (getf node path-elem) path-rest new-node)
        (setf (getf node path-elem) new-node))))

(defun update-node (root-node update)
  (update-node-step root-node (getf update :path) (getf update :new-node))
  root-node)

(defun animal-loop (root-node)
  (let ((node-update (animal-step root-node)))
    (if node-update (update-node root-node node-update) root-node)))

(defun list-animals-step (node)
  (cond
   ((not node) nil)
   ((node-is-leaf-p node) (cons (node-text node) nil))
   (t (concatenate
       'list
       (list-animals-step (yes-node node))
       (list-animals-step (no-node node))))))

(defun list-animals (root-node)
  (let ((animals (list-animals-step root-node)))
    (format t "~{~a~&~}" animals)))

(defun game-loop (root-node)
  (let ((action (query-yes-no-list "ARE YOU THINKING OF AN ANIMAL?")))
    (cond
     ((eq action :list) (list-animals root-node) (game-loop root-node))
     ((eq action t) (game-loop (animal-loop root-node)))
     (t nil))))

(defun make-initial-game ()
  (make-node "DOES IT SWIM?" (make-node "FISH") (make-node "BIRD")))

(defun animal-game ()
  (game-loop (make-initial-game)))

(defun main ()
  (animal-game))
