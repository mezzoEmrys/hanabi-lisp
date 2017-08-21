;Global variables

(defun flat (mapping-function function &rest lists)
  (apply #'concatenate 'list (apply mapping-function function lists)))

(defmacro remove-if-any (functions list)
  (if (eq functions nil)
      `,list
    `(remove-if ,(first functions)
       (remove-if-any ,(rest functions) ,list))))

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
    :accessor implied)))

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
   (save-clued :initform nil :accessor save-clued)
   (play-clued :initform nil :accessor play-clued)
   (delayed-play-blockers :initform '() :accessor delayed-play-blockers)))

(defclass hana-action ()
  ((sender
    :accessor sender
    :initarg :sender
    :initform 0)
   (action-type
    :accessor action-type
    :initarg :type)
   (target
    :accessor target
    :initarg :target)
   (data
    :accessor data
    :initarg :data ;for clue data, or new card drawn
    :initform nil)
   (slots
    :accessor slots
    :initarg :slots ;for clue data, specifying which slots are targeted
    :initform nil)))

(defun clued (card)
  (or (positive (suit-information card))
      (positive (rank-information card))))

(defmacro or-else (form else-form)
  `(let ((nn-value ,form))
     (if nn-value
         nn-value
       ,else-form)))

(defmethod print-object ((info clue-information) stream)
  (format stream "positive[~{ ~a~}] negative[~{ ~a~}] implied [~{ ~a~}]"
    (positive info)
    (negative info)
    (implied info)))

(defmethod print-object ((card card) stream)
  (print-unreadable-object (card stream :type t :identity t)
    (format stream "[~a ~a~:[~; saved~]~:[~; play~] ~a ~a]" 
      (suit-value card) 
      (rank-value card)
      (save-clued card)
      (play-clued card)
      (suit-information card)
      (rank-information card))))

(defmethod print-object ((action hana-action) stream)
  (print-unreadable-object (action stream :type t :identity t)
    (format stream "~a ~a ~a ~a~@[ (~{~a~^, ~})~]"
      (sender action)
      (action-type action)
      (target action)
      (data action)
      (slots action))))
    
(defmacro clue-is (clue &key rank suit)
  `(if (find ,clue valid-suits) ,suit  (if (find ,clue valid-ranks) ,rank nil)))

; INSTATIATING CARDS AND HANDS

(defun new-card ()
  (make-instance 'card))

(defun new-known-card (card-suit card-rank)
  (make-instance 'card :suit card-suit :rank card-rank))

(defun make-hand (&optional (hand-size 4))
  (loop repeat hand-size collect (make-instance 'card)))

(defun generate-player-hands (&optional (num-players 4) (hand-size 4))
  (loop repeat num-players collect (make-hand hand-size)))

(defun player-hand-from (list)
  (mapcar (lambda (pair) (apply #'new-known-card pair)) list))

(defun player-hands-from (list)
  (mapcar #'player-hand-from list))

; VARIABLE MANAGEMENT

(defun is-delayed-play-blocker (query-card blocked-card)
  (find query-card (delayed-play-blockers blocked-card)))

(defun add-clue ()
  (setf clues (+ clues 1)))

(defun spend-clue ()
  (setf clues (- clues 1)))

(defun add-strike ()
  (setf strikes (+ strikes 1)))

(defun remove-and-draw (hand slot new-card)
  (setf hand (remove (elt hand slot) hand))
  (push new-card hand))

(defun put-in-discard (card)
  (push card discard-pile))

(defun add-to-played-cards (card)
  (setf (gethash (suit-value card) played-cards) (rank-value card))
  (push card played-pile))

(defun is-playable (card)
  (and (not (eq (rank-value card) 'unknown))
       (eq (gethash (suit-value card) played-cards 0) (- (rank-value card) 1))))

(defun chop-card-of (hand)
  (find-if-not #'save-clued (reverse hand)))

(defun card-index (card hand)
  (position card hand))

(defun chop-index (hand)
  (card-index (chop-card-of hand) hand))

(defun eq-card (card-a card-b)
  (and (eq (suit-value card-a) (suit-value card-b))
       (eq (rank-value card-a) (rank-value card-b))))

(defun set-card (card suit rank)
  (setf (suit-value card) suit)
  (setf (rank-value card) rank)
  card)

(defun attempt-play (card)
  (if (is-playable card)
      (progn
        (add-to-played-cards card)
        (if (eq (rank-value card) 5)
            (add-clue)))
    (progn
      (put-in-discard card)
      (add-strike))))

; CLUE STUFF

(defun unseen-remaining (test-card)
  (let ((played-cards (append played-pile discard-pile (apply #'append player-hands)))
        (total-rank (gethash (rank-value test-card) rank-distribution)))
    (loop for card in played-cards
      do (if (eq-card card test-card)
             (setf total-rank (1- total-rank))))
    total-rank))

(defun no-unseen-remaining (card)
  (eq (unseen-remaining card) 0))

(defun unplayed-remaining (test-card)
  (let ((played-cards (append played-pile discard-pile))
        (total-rank (gethash (rank-value test-card) rank-distribution)))
    (loop for card in played-cards
      do (if (eq-card card test-card)
             (setf total-rank (1- total-rank))))
    total-rank))

(defun find-card-in (set test-card)
  (loop for card in set
        until (eq-card card test-card)))

(defun find-visible (test-card)
  (find-card-in (apply #'append player-hands) test-card))

(defun is-visible (test-card)
  (find-visible test-card))

(defun find-visible-exclude (hand test-card)
  (find-card-in (apply #'append (remove-if (lambda (el) (eq el hand)) player-hands)) test-card))

(defun is-visible-exclude (hand test-card)
  (find-visible-exclude hand test-card))
  
(defun is-played-card (test-card)
  (loop for card in played-pile
    until (eq-card card test-card)))

(defun find-all-cards-in (set test-card)
  (loop for card in set
        collect (if (eq-card card test-card)
                    card)))      

(defun find-all-visible (test-card)
  (remove-if-not (lambda (i) i) (find-all-cards-in (apply #'append player-hands) test-card)))

(defun clue-type (clue)
  (clue-is clue
      :suit 'suit
      :rank 'rank))

(defun clue-nature (card clue-value &optional (card-slot nil) (clued-slots nil))
  (if (or (and (eq (clue-type clue-value) 'rank) (eq (rank-value card) clue-value))
          (and (eq (clue-type clue-value) 'suit) (eq (suit-value card) clue-value))
          (and (find card-slot clued-slots)))
      'positive
    'negative))

(defun clue-information (information clue-nature clue-value)
  (if (eq clue-nature 'positive)
      (push clue-value (positive information))
    (push clue-value (negative information))))

(defun clue-rank (card clue-value clue-nature)
  (if (eq clue-nature 'positive)
      (setf (rank-value card) clue-value)))

(defun clue-suit (card clue-value clue-nature)
  (if (eq clue-nature 'positive)
      (setf (suit-value card) clue-value)))

(defun clue-card (card clue-value &optional (clue-nature (clue-nature card clue-value)))
  (clue-is clue-value
    :rank (progn
            (clue-rank card clue-value clue-nature)
            (clue-information (rank-information card) clue-nature clue-value))
    :suit (progn
            (clue-suit card clue-value clue-nature)
            (clue-information (suit-information card) clue-nature clue-value))))

(defun accept-clue (hand clue-value &optional (card-slots '()))
  (if (eq card-slots nil)
      (mapcar (lambda (card) (clue-card card clue-value (clue-nature card clue-value))) hand)
    (loop for card in hand
          for i from 0 upto (length hand)
          do (let ((nature (clue-nature card clue-value i card-slots)))
               (clue-card card clue-value nature)))))

; OTHER ACTIONS

(defun discard (hand slot &optional (drawn-card (new-card)))
  (put-in-discard (elt hand slot))
  (remove-and-draw hand slot drawn-card))

(defun play (hand slot &optional (drawn-card (new-card)))
  (let ((card (elt hand slot)))
    (if (and (not (eq (suit-value card) 'unknown)) 
             (not (eq (rank-value card) 'unknown)))
        (attempt-play card))
    (remove-and-draw hand slot drawn-card)))

(defun sender-hand (action)
  (nth (sender action) player-hands))

(defun target-hand (action)
  (nth (target action) player-hands))

;; state changing
(defun recieve-action (action)
  (let ((type (action-type action)))
    (cond
      ((eq type 'play)
       (play (sender-hand action) (target action) (data action)))
      ((eq type 'discard)
       (discard (sender-hand action) (target action) (data action)))
      ((eq type 'clue)
       (accept-clue (target-hand action) (data action))))))
 
; CARD INFORMATION

(defun is-safe-discard (test-card)
  (or (find-if (lambda (card) (eq-card test-card card)) played-pile)
      (some #'clued (find-all-visible test-card))))

; AI LOGIC

;;Global variables

(defun my-hand ()
  (nth own-index player-hands))

(defvar recieved-clue-last-turn nil)

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

(defun action-eq (action-a action-b)
  (and (eq (action-type action-a) (action-type action-b))
       (eq (target action-a) (target action-b))
       (eq (data action-a) (data action-b))))

;; Clue choosing

;;; Save Clue

(defun determine-save (hand)
  (let ((card (chop-card-of hand)))
  ;only one copy remains (e.g. 5s or unplayed cards in the trash)
    (if (and hand (not (is-safe-discard card))
             (or (eq (unplayed-remaining card) 1)
  ;it is currently playable and not visible elsewhere in players' hands
                 (and (is-playable card) 
                      (not (is-visible-exclude hand card)))
  ;it is a 2 and not visible elsewhere in players' hands
                 (and (eq (rank-value card) 2) 
                      (not (is-visible-exclude hand card)))))
        card
      nil)))

(defun playable-cards-in (hand)
  (remove-if-not #'is-playable hand))

(defun not-play-clue-marked-in (hand)
  (remove-if #'play-clued hand))

;only take action on a hand, pass everything else through
(defmacro on-hand (hand &rest body)
  `(if (or (eq nil hand) (not (listp ,hand)))
      ,hand
    (progn ,@body)))

;takes hand returns action or hand
(defun determine-best-save-clue (hand)
  (on-hand hand
    (let ((save-card (determine-save hand)))
      (if save-card
        (clue-action nil (rank-value save-card))))))

;right now we're always cluing the number

;;; Play Clue

(defun 1s-in-hand (hand)
  (remove-if-not (lambda (card) (eq 1 (rank-value card))) hand))

(defun shares-color (card-a card-b)
  (eq (suit-value card-a) (suit-value card-b)))

(defun shares-rank (card-a card-b)
  (eq (rank-value card-a) (rank-value card-b)))

(defun has-play-action (hand)
  (some #'play-clued hand))

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

(defun pick (option-a option-b)
  (if option-a
      option-a
    option-b))

(defun clue-1 (hand)
  (if (eq (length (1s-in-hand hand)) 1)
      (suit-clue-for (first (1s-in-hand hand)) hand)
    (rank-clue-for (first (1s-in-hand hand)) hand)))

(defun choose-clue (card hand)
  (if (eq (rank-value card) 1)
      (clue-1 hand)
    (pick (rank-clue-for card hand)
          (suit-clue-for card hand))))

;takes hand returns action or hand
(defun determine-best-play-clue (hand)
  (on-hand hand
    (if (not (has-play-action hand))
        (or-else (find-if-not (lambda (el) (eq nil el))
                  (mapcar 
                    (lambda (card) (choose-clue card hand)) 
                    (remove-if-any (#'clued #'is-safe-discard) (play-card-priority hand))))
          hand)
      hand)))
;indicate sufficient information for one or more previously unclued cards to be played
;best play clue is not: already clued card in someone else's hand
;best play clue is: card that can be played, (finesse???)

(defun determine-clue ()
  (let ((action nil)
        (actions (mapcar #'determine-best-save-clue 
                   (mapcar #'determine-best-play-clue player-hands))))
    (setf (nth own-index actions) nil)
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

(defun next-play-card (hand)
  (or-else (first (reverse (1s-in-hand hand)))
    (if (play-clued (chop-card-of hand))
        (chop-card-of hand)
      (first (remove-if-not (lambda (card) (play-clued card)) hand)))))

(defun has-playable-card (hand)
  (next-play-card hand))

(defun determine-play (hand)
    (if (has-playable-card hand) 
        (play-action (card-index (next-play-card hand) hand))))

(defun determine-action (&optional (hand (my-hand)))
  (let ((action nil))
    ;Princple 9, assume clue is highest value
    (if (has-playable-card hand)
        (setf action (determine-play hand)))
    ;if we weren't told to do something, check for possible good clues
    (if (and (> clues 0) (not action))
        (setf action (determine-clue)))
    ;if we don't want to clue or play, then we'll discard
    (if (not action)
        (setf action (discard-action (chop-index hand))))
    (setf recieved-clue-last-turn nil)
    action))

;; action analysis

(defun unclued-cards-touched-by (clue-action hand)
  (loop for card in hand
        for i from 0 upto (length hand)
        when (and (not (clued card)) (find i (slots clue-action)))
        collect card))

(defun update-delayed-play-blockers (card)
  (if (delayed-play-blockers card)
      (if (setf (delayed-play-blockers card) (remove-if #'is-played-card (delayed-play-blockers card)))
          (setf (play-clued card) t))))

(defun set-play-clue (card)
  (setf (play-clued card) t))

(defun set-delayed-play (card blockers)
  (setf (delayed-play-blockers card) blockers))

(defun ranks-for (suit)
  (mapcar (lambda (rank) (new-known-card suit rank)) valid-ranks))

(defun suits-for (rank)
  (mapcar (lambda (suit) (new-known-card suit rank)) valid-suits))

;TODO, remove negative information
(defun possible-cards (clue card)
  (remove-if #'is-played-card
    (remove-if #'no-unseen-remaining
      (clue-is clue
        :suit (ranks-for clue)
        :rank (suits-for clue)))))

;TODO, remove negative information
(defun blockers (clue card)
  (remove-if-not #'clued 
    (flat #'mapcar #'find-all-visible 
      (clue-is clue
        :suit (ranks-for clue)
        :rank (suits-for (- clue 1))))))

(defun recieve-play-clue (card clue)
  (if (some #'is-playable (possible-cards clue card))
      (set-play-clue card)
    (set-delayed-play card (blockers clue card))))

(defun recieve-save-clue (card)
  (setf (save-clued card) t))

(defun value-matches (clue card)
   (eq clue 
    (clue-is clue
     :suit (suit-value card)
     :rank (rank-value card))))

(defun slots-value-matches (hand clue)
  (loop for card in hand 
        for i from 0 upto (length hand)
        when (value-matches clue card)
        collect i))

(defun discard-matches (clue)
  (remove-if (lambda (card) (value-matches clue card))
             discard-pile))

(defun card-needs-save (card)
  (not (is-played-card card)))

(defun slot-to-card-for-hand (hand)
  (lambda (slot) (nth slot hand)))

(defun recieve-1s-clue (clue-action)
  (mapcar #'set-play-clue
    (mapcar (slot-to-card-for-hand (my-hand))
      (slots clue-action))))

(defun seen-clued (card)
  (some #'clued (find-all-visible card)))

(defun guess-save-value (card clue)
  (let ((possible (remove-if-any (#'seen-clued #'is-playable #'is-played-card #'no-unseen-remaining)
                    (clue-is clue
                      :suit (ranks-for clue)
                      :rank (suits-for clue)))))
    (if (eq (length possible) 1)
        (clue-is clue
          :suit (setf (rank-value card) (rank-value (first possible)))
          :rank (setf (suit-value card) (suit-value (first possible)))))))

(defun can-be-playable-clue (clue)
  (some #'is-playable
    (clue-is clue
      :suit (ranks-for clue)
      :rank (suits-for clue))))

(defun recieve-clue (clue-action)
  (if (eq (data clue-action) 1)
      (recieve-1s-clue clue-action)
    (let ((new-cards (unclued-cards-touched-by clue-action (my-hand)))
          (chop (chop-card-of (my-hand))))
      (if (and (not (can-be-playable-clue (data clue-action)))
               (not (some (lambda (card) (blockers (data clue-action) card)) new-cards)))
          (mapcar #'recieve-save-clue new-cards)
        (if (not (find-if (lambda (card) (eq card chop)) new-cards))
            (recieve-play-clue (first new-cards) (data clue-action))
          (if (or (eq (data clue-action) 2) (eq (data clue-action) 5)
                  (some #'card-needs-save (discard-matches (data clue-action))))
              (progn
                (recieve-save-clue chop)
                (guess-save-value chop (data clue-action)))
            (recieve-play-clue chop (data clue-action)))))))
  (accept-clue (my-hand) (data clue-action) (slots clue-action)))

(defun interpret (action)
  (cond
    ((eq (action-type action) 'clue)
     (cond 
       ((eq (target action) own-index)
        (recieve-clue action))
       (t (accept-clue (nth (target action) player-hands) (data action)))))
    ((eq (action-type action) 'play)
     (play (nth (sender action) player-hands) (target action) (data action)))
    ((eq (action-type action) 'discard)
     (discard (nth (sender action) player-hands) (target action) (data action))))
 (mapcar #'update-delayed-play-blockers (my-hand)))