(ql:quickload :websocket-driver-client)
(ql:quickload :ironclad)
(ql:quickload :jsown)

(defun sha-256 (str)
  (ironclad:byte-array-to-hex-string
    (ironclad:digest-sequence :sha256 
                              (ironclad:ascii-string-to-byte-array str))))

; SERVER CONNECTION

(defun create-login-json (username password)
  (let ((enc-pass (sha-256 (concatenate 'string "Hanabi password " password))))
    (jsown:new-js
      ("type" "login")
      ("resp" (jsown:new-js
                ("username" username)
                ("password" enc-pass))))))

(defvar *keldon-url* "ws://keldon.net:32221/socket.io/?EIO=3")

(defvar *intro-end* "&transport=websocket")
(defparameter *connect-end* "&transport=websocket&sid=")

(defvar *intro-client* (wsd:make-client (concatenate 'string *keldon-url* *intro-end*)))

(wsd:on :open *intro-client*
    (lambda ()
      (format t "Connected to Keldon~%")))

(wsd:on :close *intro-client*
    (lambda (&key code reason)
      (format t "Closed because '~A' (Code=~A)~%" reason code)))

(wsd:on :message *intro-client*
        (lambda (message)
          (format t "~&Code: ~A~%Message: ~A~%" 
            (subseq message 0 1) 
            (subseq message 1))))

(wsd:start-connection *intro-client*)

; login message
(wsd:send *intro-client* 
  (jsown:to-json 
    (list "message" (create-login-json "mezzobot" "BadTestingPassword"))))
;(wsd:ready-state *client*)