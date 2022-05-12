(in-package :examples)

(defun now () ""
  (multiple-value-bind (second minute hour day month year) (get-decoded-time)
     (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d" year month day hour minute second)))

(defun exec (f) "" 
  (handler-bind
   ((uhppoted-error
     #'(lambda (c) 
        (format t "~%*** ERROR: ~a~%~%" (uhppoted:message c))
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
    (when devices 
      (format t "  ~a:~%    ~:w~%~%" "get-devices" devices))))


(defun get-device () "" 
  (let* ((device-id 405419896)
         (device    (exec #'(lambda (u) (uhppoted-get-device u device-id)))))
    (when device 
      (display "get-device" device-id (as-fields device)))))


(defun set-address () "" 
  (let* ((device-id 405419896)
         (address   "192.168.1.125")
         (subnet    "255.255.254.0")
         (gateway   "192.168.1.5")
         (ok        (exec #'(lambda (u) (uhppoted-set-address u device-id address subnet gateway)))))
    (when ok 
      (display "set-address" device-id (list "address" address
                                             "subnet"  subnet
                                             "gateway" gateway)))))


(defun get-status () "" 
  (let* ((device-id 405419896)
         (status (exec #'(lambda (u) (uhppoted-get-status u device-id)))))
    (when status 
      (display "get-status" device-id (as-fields status)))))


(defun get-time () "" 
  (let* ((device-id 405419896)
         (datetime (exec #'(lambda (u) (uhppoted-get-time u device-id)))))
    (when datetime 
      (display "get-time" device-id (list "date/time" datetime)))))


(defun set-time () "" 
  (let* ((device-id 405419896)
         (datetime  (now))
         (ok        (exec #'(lambda (u) (uhppoted-set-time u device-id datetime)))))
    (when ok 
      (display "set-time" device-id (list "date/time" datetime)))))


(defun get-listener () "" 
  (let* ((device-id 405419896)
         (listener (exec #'(lambda (u) (uhppoted-get-listener u device-id)))))
    (when listener
      (display "get-listener" device-id (list "listener" listener)))))


(defun set-listener () "" 
  (let* ((device-id 405419896)
         (listener  "192.168.1.100:60001")
         (ok        (exec #'(lambda (u) (uhppoted-set-listener u device-id listener)))))
    (when ok
      (display "set-listener" device-id (list "listener" listener)))))


(defun get-door-control () "" 
  (let* ((device-id 405419896)
         (door      4)
         (control   (exec #'(lambda (u) (uhppoted-get-door-control u device-id door)))))
    (when control
      (display "get-door-control" device-id (list "mode"  (door-mode (door-control-mode  control))
                                                  "delay" (door-control-delay control))))))


(defun set-door-control () "" 
  (let* ((device-id 405419896)
         (door      4)
         (mode      uhppoted:normally-open)
         (delay     9)
         (ok        (exec #'(lambda (u) (uhppoted-set-door-control u device-id door mode delay)))))
    (when ok 
      (display "set-door-control" device-id (list "door"  door
                                                  "mode"  (door-mode mode)
                                                  "delay" delay)))))


(defun open-door () "" 
  (let* ((device-id 405419896)
         (door      4)
         (ok (exec #'(lambda (u) (uhppoted-open-door u device-id door)))))
    (when ok
      (display "open-door" device-id (list "door" door)))))


(defun get-cards () "" 
  (let* ((device-id 405419896)
         (cards (exec #'(lambda (u) (uhppoted-get-cards u device-id)))))
    (when cards
      (display "get-cards" device-id (list "cards" cards)))))


(defun get-card () "" 
  (let* ((device-id   405419896)
         (card-number 8000001)
         (card (exec #'(lambda (u) (uhppoted-get-card u device-id card-number)))))
    (when card 
      (display "get-card" device-id (as-fields card)))))

(defun get-card-by-index () "" 
  (let* ((device-id 405419896)
         (index     7)
         (card (exec #'(lambda (u) (uhppoted-get-card-by-index u device-id index)))))
    (when card
      (display "get-card-by-index" device-id (nconc (list "index" index) (as-fields card))))))


(defun put-card () "" 
  (let* ((device-id   405419896)
         (card-number 8000001)
         (from        "2022-01-01")
         (to          "2022-12-31")
         (doors       (make-array 4 :initial-contents '(0 1 31 75)))
         (ok          (exec #'(lambda (u) (uhppoted-put-card u device-id card-number from to doors)))))
    (when ok)
      (display "put-card" device-id (list "card"  card-number
                                          "from"  from
                                          "to"    to
                                          "doors" (format nil "~{~a ~}" (coerce doors 'list))))))


(defun delete-card () "" 
  (let* ((device-id   405419896)
         (card-number 8000001)
         (ok          (exec #'(lambda (u) (uhppoted-delete-card u device-id card-number)))))
    (when ok
      (display "delete-card" device-id (list "card" card-number)))))


(defun delete-cards () "" 
  (let* ((device-id 405419896)
         (ok        (exec #'(lambda (u) (uhppoted-delete-cards u device-id)))))
    (when ok
      (display "delete-cards" device-id nil))))


(defun get-event-index () "" 
  (let* ((device-id 405419896)
         (index     (exec #'(lambda (u) (uhppoted-get-event-index u device-id)))))
    (when index
      (display "get-event-index" device-id (list "index" index)))))


(defun set-event-index () "" 
  (let* ((device-id 405419896)
         (index     91)
         (ok        (exec #'(lambda (u) (uhppoted-set-event-index u device-id index)))))
    (when ok
      (display "set-event-index" device-id (list "index" index)))))


(defun get-event () "" 
  (let* ((device-id 405419896)
         (index     43)
         (event (exec #'(lambda (u) (uhppoted-get-event u device-id index)))))
    (when event 
        (display "get-event" device-id (as-fields event)))))


(defun record-special-events () "" 
  (let* ((device-id 405419896)
         (enabled   t)
         (ok        (exec #'(lambda (u) (uhppoted-record-special-events u device-id enabled)))))
    (when ok
      (display "record-special-events" device-id nil))))


(defun get-time-profile () "" 
  (let* ((device-id  405419896)
         (profile-id 29)
         (profile (exec #'(lambda (u) (uhppoted-get-time-profile u device-id profile-id)))))
    (when profile
      (display "get-time-profile" device-id (as-fields profile)))))


(defun set-time-profile () "" 
  (let* ((device-id  405419896)
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
                                        :segment3end   "18:00"))
         (ok (exec #'(lambda (u) (uhppoted-set-time-profile u device-id profile)))))
    (when ok
      (display "set-time-profile" device-id (as-fields profile)))))


(defun clear-time-profiles () "" 
  (let* ((device-id 405419896)
         (ok        (exec #'(lambda (u) (uhppoted-clear-time-profiles u device-id)))))
    (when ok
      (display "clear-time-profiles" device-id nil))))


(defun add-task () "" 
  (let* ((device-id  405419896)
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
                                :cards     11))
         (ok (exec #'(lambda (u) (uhppoted-add-task u device-id task)))))
    (when ok
      (display "add-task" device-id (as-fields task)))))


(defun refresh-tasklist () "" 
  (let* ((device-id 405419896)
         (ok        (exec #'(lambda (u) (uhppoted-refresh-tasklist u device-id)))))
    (when ok
      (display "refresh-tasklist" device-id nil))))


(defun clear-tasklist () "" 
  (let* ((device-id 405419896)
         (ok        (exec #'(lambda (u) (uhppoted-clear-tasklist u device-id)))))
    (when ok
      (display "clear-tasklist" device-id nil))))


(defun display (tag device-id fields) "" 
  (let* ((all (as-pairs (nconc (list "device-id" device-id) fields)))
         (w   (label-width all))
         (fmt (format nil "  ~~~da  ~~a~~%"  w)))
    (format t "~%~a~%" tag)
    (loop for (f v) in all
       do (cond ((typep v 'cons) 
                  (let* ((l (as-pairs v)) (w (label-width l)) (fmt (format nil "  ~a ~~~da  ~~a~~%"  f w)))
                    (loop for (f v) in l
                      do (format t fmt (string-downcase f) v))))
                (t (format t fmt (string-downcase f) v))))))


(defun as-fields (result) "" 
  (let ((fields (mapcar #'slot-definition-name (class-direct-slots (class-of result)))))
       (loop for f in fields 
         append (list (string-downcase (string f)) (field-value result f)))))


(defun as-pairs (fields) "" 
  (loop for (k v) on fields by #'cddr while v collect (list k v)))


(defun label-width (fields) ""
  (loop for (f) in fields maximize (length f)))


(defun field-value (result f) "" 
  (let ((v (slot-value result f)))
    (cond ((typep v 'boolean) (as-boolean v))
          ((typep v 'cons)    (format nil "~{~a ~}" (mapcar #'as-boolean v)))
          ((as-fields v)      (as-fields v))
          (t                  v))))


(defun as-boolean (v) "" 
  (cond ((typep v 'boolean) (if v "Y" "N"))
        (t v)))


(defun door-mode (mode)
  (cond ((equal mode uhppoted:normally-open)   "normally open")
        ((equal mode uhppoted:normally-closed) "normally closed")
        ((equal mode uhppoted:controlled)      "controlled")
        (T  "<unknown>")))
