(in-package :examples)

(defun now () ""
  (multiple-value-bind (second minute hour day month year) (get-decoded-time)
     (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d" year month day hour minute second)))

(defun exec (f) "" 
  (handler-bind
	((uhppoted-error
	   #'(lambda (c) 
		   (format t "*** ERROR: ~a~%" (uhppoted:message c))
		   (invoke-restart 'ignore))))
	(uhppoted f 
			  :bind-addr      "0.0.0.0"
			  :broadcast-addr "255.255.255.255"
			  :listen-addr    "0.0.0.0:60001"
			  :timeout        2500
			  :controllers    (list '(405419896 "192.168.1.100") '(303986753 "192.168.1.100"))
			  :debug          T)))

(defun get-devices () "" 
  (format t "  ~a:~%    ~:w~%~%" "get-devices" (coerce (exec #'(lambda (u) (uhppoted-get-devices u))) 'list)))


(defun get-device () "" 
  (let ((device-id   405419896))
    (display "get-device" device-id (exec #'(lambda (u) (uhppoted-get-device u device-id))))))


(defun set-address () "" 
  (let ((device-id 405419896)
        (address   "192.168.1.125")
        (subnet    "255.255.254.0")
        (gateway   "192.168.1.5"))
    (display "set-address" device-id (exec #'(lambda (u) (uhppoted-set-address u device-id address subnet gateway))))))


(defun get-status () "" 
  (let ((device-id 405419896))
    (display "get-status" device-id (exec #'(lambda (u) (uhppoted-get-status u device-id))))))


(defun get-time () "" 
  (let ((device-id 405419896))
    (display "get-time" device-id (exec #'(lambda (u) (uhppoted-get-time u device-id))))))


(defun set-time () "" 
  (let ((device-id 405419896)
        (datetime  (now)))
    (display "set-time" device-id (exec #'(lambda (u) (uhppoted-set-time u device-id datetime))))))


(defun get-listener () "" 
  (let ((device-id 405419896))
    (display "get-listener" device-id (exec #'(lambda (u) (uhppoted-get-listener u device-id))))))


(defun set-listener () "" 
  (let ((device-id 405419896)
        (listener  "192.168.1.100:60001"))
    (display "set-listener" device-id (exec #'(lambda (u) (uhppoted-set-listener u device-id listener))))))


(defun get-door-control () "" 
  (let ((device-id 405419896)
        (door      4))
    (display "get-door-control" device-id (exec #'(lambda (u) (uhppoted-get-door-control u device-id door))))))


(defun set-door-control () "" 
  (let ((device-id 405419896)
        (door      4)
        (mode      uhppoted:normally-open)
        (delay     9))
    (display "set-door-control" device-id (exec #'(lambda (u) (uhppoted-set-door-control u device-id door mode delay))))))


(defun open-door () "" 
  (let ((device-id 405419896)
        (door      4))
    (display "open-door" device-id (exec #'(lambda (u) (uhppoted-open-door u device-id door))))))


(defun get-cards () "" 
  (let ((device-id 405419896))
    (display "get-cards" device-id (exec #'(lambda (u) (uhppoted-get-cards u device-id))))))


(defun get-card () "" 
  (let ((device-id   405419896)
        (card-number 8000001))
    (display "get-card" device-id (exec #'(lambda (u) (uhppoted-get-card u device-id card-number))))))


(defun get-card-by-index () "" 
  (let ((device-id 405419896)
        (index     7))
    (display "get-card-by-index" device-id (exec #'(lambda (u) (uhppoted-get-card-by-index u device-id index))))))


(defun put-card () "" 
  (let ((device-id   405419896)
        (card-number 8000001)
        (from        "2022-01-01")
        (to          "2022-12-31")
        (doors       (make-array 4 :initial-contents '(0 1 31 75))))
    (display "put-card" device-id (exec #'(lambda (u) (uhppoted-put-card u device-id card-number from to doors))))))


(defun delete-card () "" 
  (let ((device-id   405419896)
        (card-number 8000001))
    (display "delete-card" device-id (exec #'(lambda (u) (uhppoted-delete-card u device-id card-number))))))


(defun delete-cards () "" 
  (let ((device-id 405419896))
    (display "delete-cards" device-id (exec #'(lambda (u) (uhppoted-delete-cards u device-id))))))


(defun get-event-index () "" 
  (let ((device-id 405419896))
    (display "get-event-index" device-id (exec #'(lambda (u) (uhppoted-get-event-index u device-id))))))


(defun set-event-index () "" 
  (let ((device-id 405419896)
        (index     91))
    (display "set-event-index" device-id (exec #'(lambda (u) (uhppoted-set-event-index u device-id index))))))


(defun get-event () "" 
  (let ((device-id 405419896)
        (index     43))
    (display "get-event" device-id (exec #'(lambda (u) (uhppoted-get-event u device-id index))))))


(defun record-special-events () "" 
  (let ((device-id 405419896)
        (enabled   t))
    (display "record-special-events" device-id (exec #'(lambda (u) (uhppoted-record-special-events u device-id enabled))))))


(defun get-time-profile () "" 
  (let ((device-id  405419896)
        (profile-id 29))
    (display "get-time-profile" device-id (exec #'(lambda (u) (uhppoted-get-time-profile u device-id profile-id))))))


(defun set-time-profile () "" 
  (let ((device-id  405419896)
        (profile    (make-time-profile :ID        29
                                       :linked    71
                                       :from      "2022-02-01"
                                       :to        "2022-06-30"
                                       :monday    t
                                       :tuesday   nil
                                       :wednesday t
                                       :thursday  t
                                       :friday    nil
                                       :saturday  nil
                                       :sunday    t
                                       :segment1start "08:30"
                                       :segment1end   "11:30"
                                       :segment2start ""
                                       :segment2end   ""
                                       :segment3start ""
                                       :segment3end   "18:00")))
    (display "set-time-profile" device-id (exec #'(lambda (u) (uhppoted-set-time-profile u device-id profile))))))


(defun clear-time-profiles () "" 
  (let ((device-id 405419896))
    (display "clear-time-profiles" device-id (exec #'(lambda (u) (uhppoted-clear-time-profiles u device-id))))))


(defun add-task () "" 
  (let ((device-id  405419896)
        (task       (make-task :task      6
                               :door      4
                               :from      "2022-02-01"
                               :to        "2022-06-30"
                               :monday    t
                               :tuesday   nil
                               :wednesday t
                               :thursday  t
                               :friday    nil
                               :saturday  nil
                               :sunday    t
                               :at        "08:30"
                               :cards     11)))
    (display "add-task" device-id (exec #'(lambda (u) (uhppoted-add-task u device-id task))))))


(defun refresh-tasklist () "" 
  (let ((device-id 405419896))
    (display "refresh-tasklist" device-id (exec #'(lambda (u) (uhppoted-refresh-tasklist u device-id))))))


(defun clear-tasklist () "" 
  (let ((device-id 405419896))
    (display "clear-tasklist" device-id (exec #'(lambda (u) (uhppoted-clear-tasklist u device-id))))))


; (print (type-of result))
(defun display (tag device-id result) "" 
  (let ((fields (mapcar #'slot-definition-name (class-direct-slots (class-of result)))))
       (display-fields
         tag 
         (cons 
           (list "device-id" device-id)
           (loop for f in fields 
             collect (list (string f) (slot-value result f)))))))


(defun display-fields (tag fields) "" 
  (let* ((w (loop for (f) in fields maximize (length f)))
         (fmt (format nil "  ~~~da  ~~a~~%"  w)))
    (format t "~%~a~%" tag)
    (loop for (f v) in fields
      do (format t fmt (string-downcase f) v))
))

