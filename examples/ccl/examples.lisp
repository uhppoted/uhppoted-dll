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
  (let ((devices (coerce (exec #'(lambda (u) (uhppoted-get-devices u))) 'list)))
    (format t "  ~a:~%    ~:w~%~%" "get-devices" devices)))


(defun get-device () "" 
  (let* ((device-id 405419896)
         (device    (exec #'(lambda (u) (uhppoted-get-device u device-id)))))
    (display "get-device" device-id (as-fields device))))


(defun set-address () "" 
  (let ((device-id 405419896)
        (address   "192.168.1.125")
        (subnet    "255.255.254.0")
        (gateway   "192.168.1.5"))
    (exec #'(lambda (u) (uhppoted-set-address u device-id address subnet gateway)))
    (display "set-address" device-id (list (list "address" address)
                                           (list "subnet"  subnet)
                                           (list "gateway" gateway)))))


(defun get-status () "" 
  (let* ((device-id 405419896)
         (status (exec #'(lambda (u) (uhppoted-get-status u device-id)))))
    (display "get-status" device-id (as-fields status))))


(defun get-time () "" 
  (let* ((device-id 405419896)
        (datetime (exec #'(lambda (u) (uhppoted-get-time u device-id)))))
    (display "get-time" device-id (list (list "date/time" datetime)))))


(defun set-time () "" 
  (let ((device-id 405419896)
        (datetime  (now)))
    (exec #'(lambda (u) (uhppoted-set-time u device-id datetime)))
    (display "set-time" device-id (list (list "date/time" datetime)))))


(defun get-listener () "" 
  (let* ((device-id 405419896)
         (listener (exec #'(lambda (u) (uhppoted-get-listener u device-id)))))
    (display "get-listener" device-id (list (list "listener" listener)))))


(defun set-listener () "" 
  (let ((device-id 405419896)
        (listener  "192.168.1.100:60001"))
    (exec #'(lambda (u) (uhppoted-set-listener u device-id listener)))
    (display "set-listener" device-id (list (list "listener" listener)))))


(defun get-door-control () "" 
  (let* ((device-id 405419896)
         (door      4)
         (control   (exec #'(lambda (u) (uhppoted-get-door-control u device-id door)))))
    (display "get-door-control" device-id (list (list "mode"  (door-mode (door-control-mode  control)))
                                                (list "delay" (door-control-delay control))))))


(defun set-door-control () "" 
  (let ((device-id 405419896)
        (door      4)
        (mode      uhppoted:normally-open)
        (delay     9))
    (exec #'(lambda (u) (uhppoted-set-door-control u device-id door mode delay)))
    (display "set-door-control" device-id (list (list "door" door) 
                                                (list "mode" (door-mode mode))
                                                (list "delay" delay)))))


(defun open-door () "" 
  (let ((device-id 405419896)
        (door      4))
    (exec #'(lambda (u) (uhppoted-open-door u device-id door)))
    (display "open-door" device-id nil)))


(defun get-cards () "" 
  (let* ((device-id 405419896)
         (cards (exec #'(lambda (u) (uhppoted-get-cards u device-id)))))
    (display "get-cards" device-id (list (list "cards" cards)))))


(defun get-card () "" 
  (let* ((device-id   405419896)
         (card-number 8000001)
         (card (exec #'(lambda (u) (uhppoted-get-card u device-id card-number)))))
    (display "get-card" device-id (as-fields card))))


(defun get-card-by-index () "" 
  (let* ((device-id 405419896)
         (index     7)
         (card (exec #'(lambda (u) (uhppoted-get-card-by-index u device-id index)))))
    (display "get-card-by-index" device-id (cons (list "index" index) (as-fields card)))))


(defun put-card () "" 
  (let ((device-id   405419896)
        (card-number 8000001)
        (from        "2022-01-01")
        (to          "2022-12-31")
        (doors       (make-array 4 :initial-contents '(0 1 31 75))))
    (exec #'(lambda (u) (uhppoted-put-card u device-id card-number from to doors)))
    (display "put-card" device-id (list (list "card" card-number) 
                                        (list "from" from) 
                                        (list "to"  to) 
                                        (list "doors" doors)))))


(defun delete-card () "" 
  (let ((device-id   405419896)
        (card-number 8000001))
    (exec #'(lambda (u) (uhppoted-delete-card u device-id card-number)))
    (display "delete-card" device-id (list (list "card" card-number)))))


(defun delete-cards () "" 
  (let ((device-id 405419896))
    (exec #'(lambda (u) (uhppoted-delete-cards u device-id)))
    (display "delete-cards" device-id nil)))


(defun get-event-index () "" 
  (let* ((device-id 405419896)
         (index     (exec #'(lambda (u) (uhppoted-get-event-index u device-id)))))
    (display "get-event-index" device-id (list (list "index" index)))))


(defun set-event-index () "" 
  (let ((device-id 405419896)
        (index     91))
    (exec #'(lambda (u) (uhppoted-set-event-index u device-id index)))
    (display "set-event-index" device-id (list (list "index" index)))))


(defun get-event () "" 
  (let* ((device-id 405419896)
         (index     43)
         (event (exec #'(lambda (u) (uhppoted-get-event u device-id index)))))
    (display "get-event" device-id (as-fields event))))


(defun record-special-events () "" 
  (let ((device-id 405419896)
        (enabled   t))
    (exec #'(lambda (u) (uhppoted-record-special-events u device-id enabled)))
    (display "record-special-events" device-id nil)))


(defun get-time-profile () "" 
  (let* ((device-id  405419896)
         (profile-id 29)
         (profile (exec #'(lambda (u) (uhppoted-get-time-profile u device-id profile-id)))))
    (display "get-time-profile" device-id (as-fields profile))))


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
    (exec #'(lambda (u) (uhppoted-set-time-profile u device-id profile)))
    (display "set-time-profile" device-id (as-fields profile))))


(defun clear-time-profiles () "" 
  (let ((device-id 405419896))
    (exec #'(lambda (u) (uhppoted-clear-time-profiles u device-id)))
    (display "clear-time-profiles" device-id nil)))


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
    (exec #'(lambda (u) (uhppoted-add-task u device-id task)))
    (display "add-task" device-id (as-fields task))))


(defun refresh-tasklist () "" 
  (let ((device-id 405419896))
    (exec #'(lambda (u) (uhppoted-refresh-tasklist u device-id)))
    (display "refresh-tasklist" device-id nil)))


(defun clear-tasklist () "" 
  (let ((device-id 405419896))
    (exec #'(lambda (u) (uhppoted-clear-tasklist u device-id)))
    (display "clear-tasklist" device-id nil)))


(defun display (tag device-id fields) "" 
  (let* ((all (cons (list "device-id" device-id) fields))
         (w (loop for (f) in all maximize (length f)))
         (fmt (format nil "  ~~~da  ~~a~~%"  w)))
    (format t "~%~a~%" tag)
    (loop for (f v) in all
      do (format t fmt (string-downcase f) v))))


(defun as-fields (result) "" 
  (let ((fields (mapcar #'slot-definition-name (class-direct-slots (class-of result)))))
       (loop for f in fields 
         collect (list (string-downcase (string f)) (field-value result f)))))

(defun field-value (result f) "" 
  (let ((v (slot-value result f)))
    (cond ((as-fields v) (as-fields v))
          ((typep v 'boolean) (if v "Y" "N"))
          (t v)
      )))

(defun door-mode (mode)
  (cond ((equal mode uhppoted:normally-open)   "normally open")
        ((equal mode uhppoted:normally-closed) "normally closed")
        ((equal mode uhppoted:controlled)      "controlled")
        (T  "<unknown>")))
