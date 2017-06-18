;Global variables

(defvar played-cards (make-hash-table))
(defvar clues 8)
(defvar strikes 0)
(defvar discard-pile '())
(defvar played-pile  '())
(defvar player-hands '())
(defvar own-index 0) ;always, index is play order from me

(defvar valid-suits '(red blue green yellow purple))
(defvar valid-ranks '(1 2 3 4 5))

(progn
  (defvar rank-distribution (make-hash-table))
  (setf (gethash 1 rank-distribution) 3)
  (setf (gethash 2 rank-distribution) 2)
  (setf (gethash 3 rank-distribution) 2)
  (setf (gethash 4 rank-distribution) 2)
  (setf (gethash 5 rank-distribution) 1)
  'rank-distribution)

; OBJECTS

(defclass clue-information ()
  ((positive
    :initform '()
    :accessor positive)
   (negative
    :initform '()
    :accessor negative)
   (implied
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
   (safe-discard :initform nil :accessor safe-discard)
   (save-clued :initform nil :accessor save-clued)
   (play-clued :initform nil :accessor play-clued)))

; INSTATIATING CARDS AND HANDS

(defun new-unknown-card ()
  (make-instance 'card))

(defun new-known-card (card-suit card-rank)
  (make-instance 'card :suit card-suit :rank card-rank))

(defun make-hand (&optional (hand-size 4))
  (loop repeat hand-size collect (make-instance 'card)))

(defun generate-player-hands (&optional (num-players 4) (hand-size 4))
  (loop repeat num-players collect (make-hand hand-size)))

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

(defun add-to-played-cards (card)
  (setf (gethash (suit-value card) played-cards) (rank-value card))
  (push card played-pile))

(defun is-card-playable (card)
  (eq (gethash (suit-value card) played-cards 0) (- (rank-value card) 1)))

(defun chop-card-of (hand)
  (loop for card in (reverse hand)
    until (not (save-clued card))))

(defun chop-index (hand)
  (position (chop-card-of hand) hand))

(defun eq-card (card-a card-b)
  (and (eq (suit-value card-a) (suit-value card-b))
       (eq (rank-value card-a) (rank-value card-b))))

(defun set-card (card suit rank)
  (setf (suit-value card) suit)
  (setf (rank-value card) rank)
  card)

(defun attempt-play (card played-cards discard-pile)
  (if (is-card-playable card)
      (progn
        (add-to-played-cards card)
        (if (eq (rank-value card) 5)
            (add-clue)))
    (progn
      (put-in-discard card discard-pile)
      (add-strike))))

; CLUE STUFF

(defun unseen-remaining (test-card)
  (let ((played-cards (append played-pile discard-pile (apply #'append player-hands)))
        (total-rank (gethash (rank-value test-card) rank-distribution)))
    (loop for card in played-cards
      do (if (eq-card card test-card)
             (setf total-rank (1- total-rank))))
    total-rank))

(defun unplayed-remaining (test-card)
  (let ((played-cards (append played-pile discard-pile))
        (total-rank (gethash (rank-value test-card) rank-distribution)))
    (loop for card in played-cards
      do (if (eq-card card test-card)
             (setf total-rank (1- total-rank))))
    total-rank))

(defun is-visible (test-card)
  (loop for card in (apply #'append player-hands)
        until (eq-card card test-card)))

(defun is-visible-exclude (hand test-card)
  (loop for card in (apply #'append 
                          (remove-if
                            (lambda (el) (eq el hand))
                            player-hands))
        until (eq-card card test-card)))

(defun is-played-card (test-card)
  (loop for card in played-pile
    until (eq-card card test-card)))

(defun is-save-clue (hand card)
  (and (not (is-played-card card))
  ;only one copy remains (e.g. 5s or unplayed cards in the trash)
       (or (eq (unplayed-remaining card) 1)
  ;it is currently playable and not visible elsewhere in players' hands
           (and (is-card-playable card) (not (is-visible-exclude hand card)))
  ;it is a 2 and not visible elsewhere in players' hands
           (and (eq (rank-value card) 2) (not (is-visible-exclude hand card))))))

(defun clue-information (information clue-nature clue-value)
  (if (eq clue-nature 'positive)
      (push clue-value (positive information))
    (push clue-value (negative information))))

(defun clue-card (hand card clue-type clue-value clue-nature)
  (if (eq clue-type 'rank)
      (clue-information (rank-information card) clue-nature clue-value)
    (clue-information (suit-information card) clue-nature clue-value))
  (if (is-save-clue hand card)
      (setf (save-clued card) t)
    (setf (play-clued card) t)))

(defun accept-clue (hand clue-type clue-value card-slots)
  (loop for card in hand
        for i from 0 upto (length hand)
        do (let ((nature (if (find i card-slots) 'positive 'negative)))
              (clue-card hand card clue-type clue-value nature)
              (setf (safe-discard card) nil))))

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

; AI LOGIC

;;;Safe discard is: copy of a card that has already been played
;;;                 copy of a card that already has a clue on it
;current implementation is "card with positive clue is not safe discard
(defun set-safe-discard (card)
  (setf (safe-discard card) 
    (and (not (positive (suit-information card)))
         (not (positive (rank-information card))))))

;;Global variables

(defvar recieved-clue-last-turn nil)

(defclass hana-action ()
  ((action-type
    :accessor action-type
    :initarg :type)
   (target
    :accessor target
    :initarg :target)
   (data
    :accessor data
    :initarg :data))) ;for clue data

(defun play-action (index)
  (make-instance 'hana-action
    :type 'play
    :target index))

(defun clue-action (player clue)
  (make-instance 'hana-action
    :type 'clue
    :target player
    :data clue))

(defun discard-action (index)
  (make-instance 'hana-action
    :type 'discard
    :target index))

;; Clue choosing

;;; Save Clue

(defun determine-save (hand)
  (let ((card (chop-card-of hand)))
  ;only one copy remains (e.g. 5s or unplayed cards in the trash)
    (if (and hand (not (save-clued card))
             (or (eq (unplayed-remaining card) 1)
  ;it is currently playable and not visible elsewhere in players' hands
                 (and (is-card-playable card) 
                      (not (is-visible-exclude hand card)))
  ;it is a 2 and not visible elsewhere in players' hands
                 (and (eq (rank-value card) 2) 
                      (not (is-visible-exclude hand card)))))
        card
      nil)))

(defun playable-cards-in (hand)
  (remove-if-not #'is-card-playable hand))

(defun not-play-clue-marked-in (hand)
  (remove-if #'play-clued hand))

;only take action on a hand, pass everything else through
(defmacro on-hand (hand &rest body)
  `(if (not (listp ,hand))
      ,hand
    (progn ,@body)))

;takes hand returns action or hand
(defun determine-best-save-clue (hand)
  (on-hand hand
    (let ((save-card (determine-save hand)))
      (clue-action nil (rank-value save-card)))))
;right now we're always cluing the number

;;; Play Clue

(defun shares-color (card-a card-b)
  (eq (suit-value card-a) (suit-value card-b)))

(defun shares-rank (card-a card-b)
  (eq (rank-value card-a) (rank-value card-b)))

(defun has-play-action (hand)
  (loop for card in hand
    until (play-clued card)))

(defun clue-eval-order (hand)
  (cons (chop-card-of hand) hand))

(defun play-card-priority (hand)
  (playable-cards-in (clue-eval-order hand)))

(defun suit-matches-before (test-card hand)
  (loop for card in hand repeat (position test-card hand)
    until (eq (suit-value card) (suit-value test-card))))

(defun suit-clue-for (card hand)
  (if (suit-matches-before card hand)
      nil
    (clue-action nil (suit-value card))))

(defun rank-matches-before (test-card hand)
  (loop for card in hand repeat (position test-card hand)
    until (eq (rank-value card) (rank-value test-card))))

(defun rank-clue-for (card hand)
  (if (rank-matches-before card hand)
      nil
    (clue-action nil (rank-value card))))

;takes hand returns action or hand
(defun determine-best-play-clue (hand)
  (on-hand hand
    (if (not (has-play-action hand))
        (loop for card in (play-card-priority hand)
          until (or (suit-clue-for card hand) (rank-clue-for card hand)))
      nil)))
;indicate sufficient information for one or more previously unclued cards to be played
;best play clue is not: already clued card in someone else's hand
;best play clue is: card that can be played, (finesse???)

(defun determine-clue ()
  (let ((action nil)
    ;find saves if needed on each player
        (actions (mapcar #'determine-best-save-clue 
                   (mapcar #'determine-best-play-clue player-hands))))
;indicate sufficient information for one or more previously unclued cards to be played
;prevent possible discard of card that needs to be saved
;giving a play clue on the same person can act as a save
    (setf (nth 0 actions) nil)
;pick person closest to us 
    (setf action (find-if (lambda (entry) 
                                  (eq (type-of entry) 'hana-action))
                          actions))
    (if action
      (setf (target action) (position-if (lambda (entry) 
                                                 (eq (type-of entry) 'hana-action))
                                         actions)))
    action))
      

;; General Action choosing

(defun first-playclue-card (hand)
  (if (play-clued (chop-card-of hand))
      (chop-card-of hand)
    (loop for card in hand
      until (play-clued card))))

(defun determine-action (hand)
  (let ((action nil))
    ;Princple 9, assume clue is highest value
    (if recieved-clue-last-turn
        (let ((play-card-index (first-playclue-card hand)))
          (if (and (not action) play-card-index)
            (setf action (play-action play-card-index)))))
    ;if we weren't told to do something, check for possible good clues
    (if (and (> clues 0) (not action))
        (setf action (determine-clue)))
    ;if we don't want to clue or play, then we'll discard
    (if (not action)
        (setf action (discard-action (chop-index hand))))
    (setf recieved-clue-last-turn nil)
    action))
