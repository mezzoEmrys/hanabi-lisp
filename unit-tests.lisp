;;(load "main.lisp")

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defun report-result (test name)
  (format t "~:[FAIL~;pass~] ... ~a~%" test name)
  test)

(defmacro combine-results (&body forms)
  (with-gensyms (result)
    `(let ((,result t))
      ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
      ,result)))

(defmacro test-group (&body tests) 
  `(progn 
     (combine-results
       ,@(loop for (n f) on tests by #'cddr collect `(report-result ,f ,n)))))


;BASIC PRINCIPLES
(test-group
  "#1 - Chop Principle"
  (test-group 
    "picking the chop card of an unknown hand"
    (let ((hand (make-hand)))
      (eq (car (last hand)) (chop-card-of hand)))

    "picking the chop card of a hand with a save clue on last card"
    (let ((hand (make-hand)))
      (setf (save-clued (nth 3 hand)) t)
      (eq (chop-card-of hand) (nth 2 hand)))

    "picking the chop card of a hand with a save clue on the penultimate card"
    (let ((hand (make-hand)))
      (setf (save-clued (nth 2 hand)) t)
      (eq (chop-card-of hand) (nth 3 hand)))

    "when discarding, and you have no known safe discards, you should discard your chop"
    (let ((recieved-clue-last-turn nil)
          (hand (make-hand))
          (clues 0))
      (action-eq (discard-action (chop-index hand)) 
                 (determine-action hand))))

  "#2 - Good Touch Principle"
  (test-group
    "safe discard is card that has been played" 
    (let ((played-pile (list (new-known-card 'red 1))))
      (is-safe-discard (new-known-card 'red 1)))

    "safe discard is a card that someone else has a clue on"
    (let* ((player-hands (generate-player-hands)) 
           (card (car (nth 1 player-hands))))
      (set-card card 'red 1)
      (clue-card card 'rank 1 'positive)
      (is-safe-discard (new-known-card 'red 1)))

    "safe discards should not be clued"
    (let ((clues 8)
          (player-hands (generate-player-hands))
          (recieved-clue-last-turn nil))
      (loop for hand in player-hands
            do(loop for suit in valid-suits
                    for card in hand
                    do (progn 
                         (setf (rank-value card) 1)
                         (setf (suit-value card) suit)
                         (clue-information (suit-information card) 'positive suit))))
      (setf (nth 0 player-hands) (make-hand))
      (action-eq (discard-action (chop-index (nth 0 player-hands)))
                 (determine-action (nth 0 player-hands))))
  
    "players should assume any clued card in hand will eventually be played"
    (let ((recieved-clue-last-turn nil)
          (hand (make-hand))
          (clues 0))
      (clue-card (chop-card-of hand) 'suit 'red 'positive)
      (action-eq (discard-action (chop-index hand)) 
                 (determine-action hand))))

  "#3 - Save Principle"
  (test-group
    "only one copy remains"
    (let ((clues 8)
          (player-hands (player-hands-from 
                          '(((unknown unknown) (unknown unknown) (unknown unknown) (unknown unknown))
                            ((red 4) (red 2) (red 3) (red 5))
                            ((yellow 2) (yellow 2) (red 2) (blue 2))
                            ((blue 2) (blue 3) (blue 3) (blue 4)))))
          (recieved-clue-last-turn nil))
      (action-eq (clue-action 1 5)
                 (determine-action (my-hand))))

    "currently playable and not visible elsewhere in players' hands"
    (let ((clues 8)
          (player-hands (player-hands-from
                          '(((unknown unknown) (unknown unknown) (unknown unknown) (unknown unknown))
                            ((red 1) (red 2) (red 3) (red 5))
                            ((yellow 2) (yellow 2) (red 2) (blue 2))
                            ((blue 2) (blue 3) (blue 3) (blue 4)))))
          (recieved-clue-last-turn nil))
      (action-eq (clue-action 1 'red)
                 (determine-action (my-hand))))

    "is a 2 and not visible elsewhere in players' hands"
    (let ((clues 8)
          (player-hands (player-hands-from
                          '(((unknown unknown) (unknown unknown) (unknown unknown) (unknown unknown))
                            ((red 5) (red 2) (red 3) (purple 2))
                            ((yellow 2) (yellow 2) (red 2) (blue 2))
                            ((blue 2) (blue 3) (blue 3) (blue 4)))))
          (recieved-clue-last-turn nil))
      (action-eq (clue-action 1 2)
                 (determine-action (my-hand)))))

  "#4 - Minimum Clue Value Principle"
  (test-group
    "indicate sufficient information for one or more previous unclued cards to be played"
    (let ((clues 8)))

    "must prevent possible discard of a card (such as in save principle)"
    (let ((a nil))))

  "#5 - Play Order Principle"
  (test-group
    "when a play clue touches multiple cards, if it touches the chop, it's focused on the chop"
    (let ((a nil)))

    "otherwise, it's focused on the newest card"
    (let ((a nil))))

  "#6 - Left-Most Playable Principle"
  (test-group
    "cards are expected to be played from left to right failing principle 5"
    (let ((a nil))))

  "#7 - Information Lock Principle"
  (test-group
    "clues should not override unless there is direct conflict"
    (let ((a nil))))

  "#8 - Good Lie Principle"
  (test-group
    "you can be lied to, but the lie should be revealed soon"
    (let ((a nil))))

  "#9 - High Value Principle"
  (test-group
    "you should expect that what someone did is the highest value thing they could"
    (let ((a nil)))))

;BEGINNER STRATEGIES
(test-group
  "#1 - When to Discard"
  (test-group
    "avoid discarding if you can do anything else"
    (let ((a nil)))

    "early game, giving 2s and 5s clues off chop is okay to avoid first discard"
    (let ((a nil))))

  "#2 - Cluing 1s"
  (test-group
    "use color clues to clue single 1s"
    (let ((clues 8)
          (player-hands (player-hands-from
                          '(((unknown unknown) (unknown unknown) (unknown unknown) (unknown unknown))
                            ((red 5) (red 2) (red 3) (purple 1))
                            ((yellow 2) (yellow 2) (red 2) (blue 2))
                            ((blue 2) (blue 3) (blue 3) (blue 4)))))
          (recieved-clue-last-turn nil))
      (action-eq (clue-action 1 'purple)
                 (determine-action (my-hand))))

    "assume all 1 clues in hand are playable"
    (let ((a nil)))

    "at beginning of game play 1s from oldest to newest"
    (let ((a nil))))

  "#3 - Saving 5s on Chop"
  (test-group
    "use number clue for 5s"
    (let ((clues 8)
          (player-hands (player-hands-from 
                          '(((unknown unknown) (unknown unknown) (unknown unknown) (unknown unknown))
                            ((red 4) (red 2) (red 3) (red 5))
                            ((yellow 2) (yellow 2) (red 2) (blue 2))
                            ((blue 2) (blue 3) (blue 3) (blue 4)))))
          (recieved-clue-last-turn nil))
      (action-eq (clue-action 1 5)
                 (determine-action (my-hand)))))

  "#4 - Saving 5s off Chop"
  (test-group
    "avoid doing this except when trying to avoid discards"
    (let ((a nil))))

  "#5 - Saving 2s on Chop"
  (test-group
    "use number clue for 2s"
    (let ((clues 8)
          (player-hands (player-hands-from
                          '(((unknown unknown) (unknown unknown) (unknown unknown) (unknown unknown))
                            ((red 5) (red 2) (red 3) (purple 2))
                            ((yellow 2) (yellow 2) (red 2) (blue 2))
                            ((blue 2) (blue 3) (blue 3) (blue 4)))))
          (recieved-clue-last-turn nil))
      (action-eq (clue-action 1 2)
                 (determine-action (my-hand)))))

  "#6 - Delayed Play Clues"
  (test-group
    "if you recieve a save clue, it should be a play clue that should wait until it is possible."
    (let ((a nil)))))