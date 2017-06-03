(defvar played-cards (make-hash-table))
(defvar clues 8)
(defvar strikes 0)
(defvar discard-pile '())

; OBJECTS

(defclass clue-information ()
  ((positive
    :initform '()
    :accessor positive)
   (negative
    :initform '()
    :accessor negative)))

(defclass card ()
  ((suit-value
    :initarg :suit
    :initform 'unknown
    :accessor suit-value)
   (rank-value
    :initarg :rank
    :initform 'unknown
    :accessor rank-value)
   (suit-information
    :initform (make-instance 'clue-information)
    :accessor suit-information)
   (rank-information
    :initform (make-instance 'clue-information)
    :accessor rank-information)
   (safe-discard :initform nil)))

; VARIABLE MANAGEMENT

(defun add-clue ()
  (setf clues (+ clues 1)))

(defun spend-clue ()
  (setf clues (- clues 1)))

(defun add-strike ()
  (setf strikes (+ strikes 1)))

(defun remove-and-draw (hand slot new-card)
  (setf hand (remove (elt hand slot) hand))
  (push new-card hand))

(defun put-in-discard (card discard-pile)
  (push card discard-pile))

(defun attempt-play (card played-cards discard-pile)
  (if (is-card-playable card played-cards)
      (progn
        (add-to-played-cards card)
        (if (eq (card-rank card) 5)
            (add-clue)))
    (progn
      (put-in-discard card discard-pile)
      (add-strike))))

(defun add-to-played-cards (card played-cards)
  (setf (gethash (suit-value card) played-cards) (rank-value card)))

(defun is-card-playable (card played-cards)
  (eq (gethash (suit-value card) played-cards) (- (rank-value card) 1)))


; CLUE STUFF

(defun accept-clue (hand clue-type clue-value card-slots)
  (loop for card in hand
        for i from 0 upto (length hand)
        do (let ((nature (if (find i card-slots) 'positive 'negative)))
              (clue-card card clue-type clue-value nature))))

(defun clue-card (card clue-type clue-value clue-nature)
  (if (eq clue-type 'rank)
      (clue-information (rank-information card) clue-nature clue-value)
    (clue-information (suit-information card) clue-nature clue-value)))
   
(defun clue-information (information clue-nature clue-value)
  (if (eq clue-nature 'positive)
      (push clue-value (positive information))
    (push clue-value (negative information))))

; OTHER ACTIONS

(defun discard (hand slot discard-pile &optional (drawn-card (new-unknown-card)))
  (put-in-discard (elt hand slot) discard-pile)
  (remove-and-draw hand slot drawn-card))

(defun play (hand slot played-cards &optional (drawn-card (new-unknown-card)))
  (let ((card (elt hand slot)))
    (if (and (not (eq (suit-value card) 'unknown)) 
             (not (eq (rank-value card) 'unknown)))
        (attempt-play card played-cards discard-pile))
    (remove-and-draw hand slot drawn-card)))

; INSTATIATING CARDS AND HANDS

(defun new-unknown-card ()
  (make-instance 'card))

(defun new-known-card (card-suit card-rank)
  (make-instance 'card :suit card-suit :rank card-rank))

(defun make-hand (&optional (hand-size 4))
  (loop repeat hand-size collect (make-instance 'card)))

    