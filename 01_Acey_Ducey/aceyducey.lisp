#|
0..12
Card X -> X {2, 3, 4, 5, 6, 7, 8, 9, 10, 11 (Jack), 12 (Queen), 13 (King), 14 (Ace)}
Card A (Random), Card B (Random)
-> A < B

Card C (Random)
C <= A -> You Lose
C >= B -> You Lose
A < C < B -> You Win
|#

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:with-user-abort) :silent t))

(setf *random-state* (make-random-state T))

(defun make-spaces (n)
  (make-string n :initial-element #\Space))

(defun random-card (&key (start 2) (stop 14))
  (+ start (random (- stop start))))

(defun random-pair ()
  (let* ((a (random-card :stop 13))
         (b (random-card :start (+ a 1))))
    (list a b)))

(defun card-value (card)
  (cond
   ((and (>= card 2) (< card 11)) card)
   ((= card 11) "JACK")
   ((= card 12) "QUEEN")
   ((= card 13) "KING")
   ((= card 14) "ACE")
   (T "XX")))

(defun print-card (card)
  (format t "~a~a~&" (make-spaces 4) (card-value card)))

(defun print-pair (pair)
  (destructuring-bind (a b) pair
    (format t "~a~a~&~a~a~&" (make-spaces 4) (card-value a) (make-spaces 4) (card-value b))))

(defun read-number ()
  (let ((num (read)))
    (typecase num
              (number num)
              (t (format t "~a IS NOT A NUMBER~&" num) (read-number)))))

(defun read-bet (maxbet)
  (format t "WHAT IS YOUR BET? ")
  (let ((bet (read-number)))
    (cond
     ((> bet maxbet)
      (format t "SORRY, MY FRIEND, BUT YOU BET TOO MUCH~&")
      (format t "YOU HAVE ONLY ~a DOLLARS TO BET. ~&" maxbet)
      (read-bet maxbet))
     (t bet))))

(defun winning-card-p (card pair)
  (destructuring-bind (a b) pair
    (< a card b)))

(defun print-header ()
  (format t "~aACEY DUCEY CARD GAME~&" (make-spaces 26))
  (format t "~aCREATIVE COMPUTING  MORRISTOWN, NEW JERSEY~&" (make-spaces 15))
  (format t "ACEY-DUCEY IS PLAYED IN THE FOLLOWING MANNER ~&")
  (format t "THE DEALER (COMPUTER) DEALS TWO CARDS FACE UP~&")
  (format t "YOU HAVE AN OPTION TO BET OR NOT BET DEPENDING~&")
  (format t "ON WHETHER OR NOT YOU FEEL THE CARD WILL HAVE~&")
  (format t "A VALUE BETWEEN THE FIRST TWO.~&")
  (format t "IF YOU DO NOT WANT TO BET, INPUT A 0~&"))

(defun acey-ducey-round (balance)
  (let ((pair (random-pair)))

    (format t "~&~&HERE ARE YOUR NEXT TWO CARDS: ~&")
    (print-pair pair)

    (let* ((bet (read-bet balance)))
      (if (= bet 0)
          (progn (format t "CHICKEN!!~&") (list balance nil))
          (let ((draw (random-card)))
            (print-card draw)
            (if (winning-card-p draw pair)
                (progn (format t "YOU WIN!!!~&") (list (+ balance bet) t))
                (progn (format t "SORRY, YOU LOSE~&") (list (- balance bet) t))))))))

(defun acey-ducey-session (balance &optional (print-balance T))
  (when print-balance
        (format t "YOU NOW HAVE ~a DOLLARS.~&" balance))
  (destructuring-bind (new-balance new-print-balance) (acey-ducey-round balance)
    (if (= 0 new-balance)
        (format t "SORRY, FRIEND, BUT YOU BLEW YOUR WAD~&")
        (acey-ducey-session new-balance new-print-balance))))

(defun acey-ducey-game (balance)
  (acey-ducey-session balance)
  (if (yes-or-no-p "TRY AGAIN")
      (acey-ducey-game balance)
      (format t "O.K., HOPE YOU HAD FUN!~&")))

(defun main (&optional (balance 100))
  (print-header)
  (acey-ducey-game balance))
