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
             :controllers    (list '(405419896 "192.168.1.100" "tcp") '(303986753 "192.168.1.100"))
             :debug          T)))


(defun get-controllers (args) "" 
  (declare (ignore args))
  (let ((controllers (coerce (exec #'(lambda (u) (uhppoted-get-devices u))) 'list)))
    (when controllers
      (format t "  ~a:~%    ~:w~%~%" "get-controllers" controllers))))


(defun get-controller (args) "" 
  (let* ((device-id (args-device-id args))
         (device    (exec #'(lambda (u) (uhppoted-get-device u device-id)))))
    (when device 
      (display "get-controller" device-id (as-fields device)))))


(defun set-address (args) "" 
  (let* ((device-id (args-device-id    args))
         (address   (args-ip-address   args))
         (subnet    (args-subnet-mask  args))
         (gateway   (args-gateway-addr args))
         (ok        (exec #'(lambda (u) (uhppoted-set-address u device-id address subnet gateway)))))
    (when ok 
      (display "set-address" device-id (list "address" address
                                             "subnet"  subnet
                                             "gateway" gateway)))))


(defun get-status (args) "" 
  (let* ((device-id (args-device-id args))
         (status (exec #'(lambda (u) (uhppoted-get-status u device-id)))))
    (when status 
      (if (string= "" (event-timestamp (status-event status)))
        (setf (event-timestamp (status-event status)) "-"))

      (display "get-status" device-id (as-fields status)))))


(defun get-time (args) "" 
  (let* ((device-id (args-device-id args))
         (datetime (exec #'(lambda (u) (uhppoted-get-time u device-id)))))
    (when datetime 
      (display "get-time" device-id (list "date/time" datetime)))))


(defun set-time (args) "" 
  (let* ((device-id (args-device-id args))
         (datetime  (now))
         (ok        (exec #'(lambda (u) (uhppoted-set-time u device-id datetime)))))
    (when ok 
      (display "set-time" device-id (list "date/time" datetime)))))


(defun get-listener (args) "" 
  (let* ((device-id (args-device-id args))
         (listener (exec #'(lambda (u) (uhppoted-get-listener u device-id)))))
    (when listener
      (display "get-listener" device-id (list "listener" listener)))))


(defun set-listener (args) "" 
  (let* ((device-id (args-device-id     args))
         (listener  (args-listener-addr args))
         (ok        (exec #'(lambda (u) (uhppoted-set-listener u device-id listener)))))
    (when ok
      (display "set-listener" device-id (list "listener" listener)))))


(defun get-door-control (args) "" 
  (let* ((device-id (args-device-id args))
         (door      (args-door      args))
         (control   (exec #'(lambda (u) (uhppoted-get-door-control u device-id door)))))
    (when control
      (display "get-door-control" device-id (list "mode"  (uhppoted-lookup lookup-mode (door-control-mode  control) "")
                                                  "delay" (door-control-delay control))))))


(defun set-door-control (args) "" 
  (let* ((device-id (args-device-id args))
         (door      (args-door      args))
         (mode      uhppoted:normally-open)
         (delay     9)
         (ok        (exec #'(lambda (u) (uhppoted-set-door-control u device-id door mode delay)))))
    (when ok 
      (display "set-door-control" device-id (list "door"  door
                                                  "mode"  (uhppoted-lookup lookup-mode mode "")
                                                  "delay" delay)))))


(defun open-door (args) "" 
  (let* ((device-id (args-device-id args))
         (door      (args-door      args))
         (ok (exec #'(lambda (u) (uhppoted-open-door u device-id door)))))
    (when ok
      (display "open-door" device-id (list "door" door)))))


(defun get-cards (args) "" 
  (let* ((device-id (args-device-id args))
         (cards (exec #'(lambda (u) (uhppoted-get-cards u device-id)))))
    (when cards
      (display "get-cards" device-id (list "cards" cards)))))


(defun get-card (args) "" 
  (let* ((device-id   (args-device-id   args))
         (card-number (args-card-number args))
         (card (exec #'(lambda (u) (uhppoted-get-card u device-id card-number)))))
    (when card 
      (display "get-card" device-id (as-fields card)))))

(defun get-card-by-index (args) "" 
  (let* ((device-id (args-device-id  args))
         (index     (args-card-index args))
         (card (exec #'(lambda (u) (uhppoted-get-card-by-index u device-id index)))))
    (when card
      (display "get-card-by-index" device-id (nconc (list "index" index) (as-fields card))))))


(defun put-card (args) "" 
  (let* ((device-id   (args-device-id   args))
         (card-number (args-card-number args))
         (from        "2022-01-01")
         (to          "2022-12-31")
         (doors       (make-array 4 :initial-contents '(0 1 31 75)))
         (PIN         7531)
         (ok          (exec #'(lambda (u) (uhppoted-put-card u device-id card-number from to doors PIN)))))
    (when ok)
      (display "put-card" device-id (list "card"  card-number
                                          "from"  from
                                          "to"    to
                                          "doors" (format nil "~{~a ~}" (coerce doors 'list))
                                          "PIN"   PIN))))


(defun delete-card (args) "" 
  (let* ((device-id   (args-device-id   args))
         (card-number (args-card-number args))
         (ok          (exec #'(lambda (u) (uhppoted-delete-card u device-id card-number)))))
    (when ok
      (display "delete-card" device-id (list "card" card-number)))))


(defun delete-cards (args) "" 
  (let* ((device-id (args-device-id args))
         (ok        (exec #'(lambda (u) (uhppoted-delete-cards u device-id)))))
    (when ok
      (display "delete-cards" device-id nil))))


(defun get-event-index (args) "" 
  (let* ((device-id (args-device-id args))
         (index     (exec #'(lambda (u) (uhppoted-get-event-index u device-id)))))
    (when index
      (display "get-event-index" device-id (list "index" index)))))


(defun set-event-index (args) "" 
  (let* ((device-id (args-device-id   args))
         (index     (args-event-index args))
         (ok        (exec #'(lambda (u) (uhppoted-set-event-index u device-id index)))))
    (when ok
      (display "set-event-index" device-id (list "index" index)))))


(defun get-event (args) "" 
  (let* ((device-id (args-device-id args))
         (index     43)
         (event (exec #'(lambda (u) (uhppoted-get-event u device-id index)))))
    (when event 
        (display "get-event" device-id (as-fields event)))))


(defun record-special-events (args) "" 
  (let* ((device-id (args-device-id args))
         (enabled   t)
         (ok        (exec #'(lambda (u) (uhppoted-record-special-events u device-id enabled)))))
    (when ok
      (display "record-special-events" device-id nil))))


(defun get-time-profile (args) "" 
  (let* ((device-id  (args-device-id  args))
         (profile-id (args-profile-id args))
         (profile (exec #'(lambda (u) (uhppoted-get-time-profile u device-id profile-id)))))
    (when profile
      (display "get-time-profile" device-id (as-fields profile)))))


(defun set-time-profile (args) "" 
  (let* ((device-id  (args-device-id args))
         (profile    (make-time-profile :ID        (args-profile-id args)
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


(defun clear-time-profiles (args) "" 
  (let* ((device-id (args-device-id args))
         (ok        (exec #'(lambda (u) (uhppoted-clear-time-profiles u device-id)))))
    (when ok
      (display "clear-time-profiles" device-id nil))))


(defun add-task (args) "" 
  (let* ((device-id  (args-device-id args))
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


(defun refresh-tasklist (args) "" 
  (let* ((device-id (args-device-id args))
         (ok        (exec #'(lambda (u) (uhppoted-refresh-tasklist u device-id)))))
    (when ok
      (display "refresh-tasklist" device-id nil))))


(defun clear-tasklist (args) "" 
  (let* ((device-id (args-device-id args))
         (ok        (exec #'(lambda (u) (uhppoted-clear-tasklist u device-id)))))
    (when ok
      (display "clear-tasklist" device-id nil))))


(defun set-pc-control (args) "" 
  (let* ((device-id (args-device-id args))
         (enabled   t)
         (ok (exec #'(lambda (u) (uhppoted-set-pc-control u device-id enabled)))))
    (when ok
      (display "set-pc-control" device-id nil))))


(defun set-interlock (args) "" 
  (let* ((device-id (args-device-id args))
         (interlock 1)
         (ok (exec #'(lambda (u) (uhppoted-set-interlock u device-id interlock)))))
    (when ok
      (display "set-interlock" device-id nil))))


(defun activate-keypads (args) "" 
  (let* ((device-id (args-device-id args))
         (reader1 t)
         (reader2 t)
         (reader3 nil)
         (reader4 t)
         (ok (exec #'(lambda (u) (uhppoted-activate-keypads u device-id reader1 reader2 reader3 reader4)))))
    (when ok
      (display "activate-keypads" device-id nil))))


(defun set-door-passcodes (args) "" 
  (let* ((device-id (args-device-id args))
         (door      (args-door      args))
         (passcode1 12345)
         (passcode2 999999)
         (passcode3 0)
         (passcode4 54321)
         (ok (exec #'(lambda (u) (uhppoted-set-door-passcodes u device-id door passcode1 passcode2 passcode3 passcode4)))))
    (when ok
      (display "set-door-passcodes" device-id nil))))


(defun restore-default-parameters (args) "" 
  (let* ((device-id (args-device-id args))
         (ok        (exec #'(lambda (u) (uhppoted-restore-default-parameters u device-id)))))
    (when ok
      (display "restore-default-parameters" device-id nil))))

(defparameter *listen-events-interrupted* (make-semaphore))
(defparameter *listen-events-stopped*     (make-semaphore))

(defcallback on-sigint (:int signal) "Callback function for SIGINT"
  (declare (ignore signal))
  (signal-semaphore *listen-events-interrupted*))

; NTS: sends a 'break:interrupted' to the debugger on return which is concealed by
;      the immediate exit after *listen-events-stoppped* is set
(defun listen-events-thread (userdata) "" 
  (let*  ((on-event (lambda (controller event userdata) 
                      (declare (ignore userdata))
                      (display " event" controller (as-fields event))))
          (on-error (lambda (err)   (format t " *** ERROR ~a~%" err))))
    (exec #'(lambda (u) (uhppoted-listen-events u on-event on-error userdata)))
    (signal-semaphore *listen-events-stopped*)))

; Ref. https://stackoverflow.com/questions/9950680/unix-signal-handling-in-common-lisp
(defun listen-events (args) "" 
  (declare (ignore args))
  (process-run-function "listen" #'(lambda () (listen-events-thread 0) (break "interrupted")))

  (external-call "signal" :int 2
                          :address on-sigint
                          :address)

  (let ((*break-on-signals* nil))
    (setf ccl:*break-hook*
      (lambda (cond hook)
        (declare (ignore cond hook))
        ; (ccl:quit)))
        t))

  (wait-on-semaphore *listen-events-interrupted*)
  (format t " ... interrupted~%")

  (signal-semaphore *uhppoted-listen-stop*)
  (timed-wait-on-semaphore *listen-events-stopped* 5)
  (format t " ... done~%")))


(defun args-device-id (args) 
  (parse-integer (parse-args args "--controller" "405419896")))

(defun args-ip-address (args) 
  (parse-args args "--ip-address" "192.168.1.125"))

(defun args-subnet-mask (args)
  (parse-args args "--subnet-mask" "255.255.254.0"))

(defun args-gateway-addr (args)
  (parse-args args "--gateway-address" "192.168.1.5"))

(defun args-listener-addr (args)
  (parse-args args "--listener-address" "192.168.1.100:60001"))

(defun args-door (args)
  (parse-integer (parse-args args "--door" "4")))

(defun args-card-number (args)
  (parse-integer (parse-args args "--card" "8000001")))

(defun args-card-index (args)
  (parse-integer (parse-args args "--card-index" "7")))

(defun args-event-index (args)
  (parse-integer (parse-args args "--event-index" "91")))

(defun args-profile-id (args)
  (parse-integer (parse-args args "--time-profile" "29")))

(defun parse-args (args tag defval)
  (let ((ix (position-if #'(lambda (a) (string= a tag)) args)))
    (if (and ix (nth (+ ix 1) args))
        (nth (+ ix 1) args)
        defval)))


(defun display (tag device-id fields) "" 
  (let* ((all (as-pairs (nconc (list "controller" device-id) fields)))
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
         append (list (string-downcase (string f)) (field-value (string f) result f)))))


(defun as-pairs (fields) "" 
  (loop for (k v) on fields by #'cddr while v collect (list k v)))


(defun label-width (fields) ""
  (loop for (f) in fields maximize (length f)))


(defun field-value (label result f) "" 
  (let* ((v (slot-value result f))
         (vv (cond ((typep v 'boolean) (as-boolean v))
                   ((typep v 'cons)    (format nil "~{~a ~}" (mapcar #'as-boolean v)))
                   ((as-fields v)      (as-fields v))
                   (t                  v))))
    (cond ((string= label "DIRECTION")  (uhppoted-lookup lookup-direction    vv ""))
          ((string= label "EVENT-TYPE") (uhppoted-lookup lookup-event-type   vv ""))
          ((string= label "REASON")     (uhppoted-lookup lookup-event-reason vv ""))
          (t vv))))


(defun as-boolean (v) "" 
  (cond ((typep v 'boolean) (if v "Y" "N"))
        (t v)))


(defun door-mode (mode)
  (cond ((equal mode uhppoted:normally-open)   "normally open")
        ((equal mode uhppoted:normally-closed) "normally closed")
        ((equal mode uhppoted:controlled)      "controlled")
        (T  "<unknown>")))
