(in-package :tests)

(defconstant TEST-DEVICE-ID   405419896)
(defconstant TEST-CARD-NUMBER 8165538)
(defconstant TEST-CARD-INDEX  19)
(defconstant TEST-EVENT-INDEX 51)
(defconstant TEST-DOOR        4)
(defconstant TEST-PROFILE-ID  49)

(defstruct result field
                  expected
                  value)

(define-condition failed (error)
  ((message :initarg :message :reader message)))

(defun exec (f) "" 
  (handler-bind
    ((uhppoted-error #'(lambda (c) (error 'failed :message  (uhppoted:message c)))))
    (uhppoted f 
              :bind-addr      "0.0.0.0"
              :broadcast-addr "255.255.255.255"
              :listen-addr    "0.0.0.0:60001"
              :timeout        2500
              :controllers    (list '(405419896 "192.168.1.100") '(303986753 "192.168.1.100"))
              :debug          T)))


(defun get-devices () "" 
  (let ((devices (exec #'(lambda (u) (uhppoted-get-devices u)))))
    (evaluate "get-devices" 
              (list (make-result :field "device count" :expected 3                                :value (length devices))
                    (make-result :field "device list"  :expected '(201020304 303986753 405419896) :value (coerce devices 'list))))))


(defun get-device () "" 
  (let ((device (exec #'(lambda (u) (uhppoted-get-device u TEST-DEVICE-ID)))))
    (evaluate "get-device" 
              (list (make-result :field "device ID"              :expected 405419896           :value (device-id      device))
                    (make-result :field "device address"         :expected "192.168.1.101"     :value (device-address device))
                    (make-result :field "device subnet mask"     :expected "255.255.255.0"     :value (device-subnet  device))
                    (make-result :field "device gateway address" :expected "192.168.1.1"       :value (device-gateway device))
                    (make-result :field "device MAC address"     :expected "00:12:23:34:45:56" :value (device-MAC     device))
                    (make-result :field "device version"         :expected "v8.92"             :value (device-version device))
                    (make-result :field "device date"            :expected "2018-11-05"        :value (device-date    device))))))


(defun set-address () "" 
  (exec #'(lambda (u) (uhppoted-set-address u TEST-DEVICE-ID "192.168.1.125" "255.255.254.0" "192.168.1.0")))
  (evaluate "set-address" '()))


(defun get-status () "" 
  (let* ((status (exec #'(lambda (u) (uhppoted-get-status u TEST-DEVICE-ID))))
         (event  (status-event status)))
    (evaluate "get-status" 
              (list (make-result :field "device ID"         :expected 405419896             :value (status-id        status))
                    (make-result :field "system date/time"  :expected "2022-03-19 15:48:32" :value (status-timestamp status))
                    (make-result :field "doors state"       :expected '(t nil nil t)        :value (status-doors     status))
                    (make-result :field "buttons state"     :expected '(t nil t   nil)      :value (status-buttons   status))
                    (make-result :field "relays state"      :expected 18                    :value (status-relays    status))
                    (make-result :field "inputs state"      :expected 52                    :value (status-inputs    status))
                    (make-result :field "system error"      :expected 86                    :value (status-syserror  status))
                    (make-result :field "system info"       :expected 253                   :value (status-info      status))
                    (make-result :field "sequence number"   :expected 9876                  :value (status-seqno     status))
                    (make-result :field "event index"       :expected 135                   :value (event-index      event))
                    (make-result :field "event timestamp"   :expected "2022-01-02 12:34:56" :value (event-timestamp  event))
                    (make-result :field "event type"        :expected 6                     :value (event-event-type event))
                    (make-result :field "event granted"     :expected 1                     :value (event-granted    event))
                    (make-result :field "event door"        :expected 3                     :value (event-door       event))
                    (make-result :field "event direction"   :expected 1                     :value (event-direction  event))
                    (make-result :field "event card number" :expected 8100023               :value (event-card       event))
                    (make-result :field "event reason"      :expected 21                    :value (event-reason     event))))))


(defun get-time () "" 
  (let ((datetime (exec #'(lambda (u) (uhppoted-get-time u TEST-DEVICE-ID)))))
    (evaluate "get-time" 
              (list (make-result :field "date/time" :expected "2022-01-02 12:34:56" :value datetime)))))


(defun set-time () "" 
  (exec #'(lambda (u) (uhppoted-set-time u TEST-DEVICE-ID "2022-03-23 12:24:17")))
  (evaluate "set-time" '()))


(defun get-listener () "" 
  (let ((listener (exec #'(lambda (u) (uhppoted-get-listener u TEST-DEVICE-ID)))))
    (evaluate "get-listener" 
              (list (make-result :field "event listener address" :expected "192.168.1.100:60001" :value listener)))))


(defun set-listener () "" 
  (exec #'(lambda (u) (uhppoted-set-listener u TEST-DEVICE-ID "192.168.1.100:60001")))
  (evaluate "set-listener" '()))


(defun get-door-control () "" 
  (let ((control (exec #'(lambda (u) (uhppoted-get-door-control u TEST-DEVICE-ID TEST-DOOR)))))
    (evaluate "get-door-control" 
              (list (make-result :field "door control mode" :expected uhppoted:controlled :value (door-control-mode  control))
                    (make-result :field "door open delay"   :expected 7                   :value (door-control-delay control))))))


(defun set-door-control () "" 
  (exec #'(lambda (u) (uhppoted-set-door-control u TEST-DEVICE-ID TEST-DOOR uhppoted:normally-closed 6)))
  (evaluate "set-door-control" '()))


(defun open-door () "" 
  (exec #'(lambda (u) (uhppoted-open-door u TEST-DEVICE-ID TEST-DOOR)))
  (evaluate "open-door" '()))


(defun get-cards () "" 
  (let ((cards (exec #'(lambda (u) (uhppoted-get-cards u TEST-DEVICE-ID)))))
    (evaluate "get-card" 
              (list (make-result :field "card count" :expected 39 :value cards)))))


(defun get-card () "" 
  (let ((card (exec #'(lambda (u) (uhppoted-get-card u TEST-DEVICE-ID TEST-CARD-NUMBER)))))
    (evaluate "get-card" 
              (list (make-result :field "card number"      :expected 8165538      :value (card-card-number card))
                    (make-result :field "card 'from' date" :expected "2022-01-01" :value (card-from card))
                    (make-result :field "card 'to' date"   :expected "2022-12-31" :value (card-to card))
                    (make-result :field "card doors"       :expected '(0 1 31 75) :value (card-doors card))))))


(defun get-card-by-index () "" 
  (let ((card (exec #'(lambda (u) (uhppoted-get-card-by-index u TEST-DEVICE-ID TEST-CARD-INDEX)))))
    (evaluate "get-card-by-index" 
              (list (make-result :field "card number"      :expected 8165538      :value (card-card-number card))
                    (make-result :field "card 'from' date" :expected "2022-01-01" :value (card-from card))
                    (make-result :field "card 'to' date"   :expected "2022-12-31" :value (card-to card))
                    (make-result :field "card doors"       :expected '(0 1 31 75) :value (card-doors card))))))

(defun put-card () "" 
  (let ((doors (make-array 4 :initial-contents '(0 1 31 75))))
    (exec #'(lambda (u) (uhppoted-put-card u TEST-DEVICE-ID TEST-CARD-NUMBER "2022-01-01" "2022-12-31" doors)))
    (evaluate "put-card" '())))


(defun delete-card () "" 
  (exec #'(lambda (u) (uhppoted-delete-card u TEST-DEVICE-ID TEST-CARD-NUMBER)))
  (evaluate "delete-card" '()))


(defun delete-cards () "" 
  (exec #'(lambda (u) (uhppoted-delete-cards u TEST-DEVICE-ID)))
  (evaluate "delete-cards" '()))


(defun get-event-index () "" 
  (let ((index    (exec #'(lambda (u) (uhppoted-get-event-index u TEST-DEVICE-ID)))))
    (evaluate "get-event-index" 
              (list (make-result :field "event index" :expected 47 :value index)))))


(defun set-event-index () "" 
  (exec #'(lambda (u) (uhppoted-set-event-index u TEST-DEVICE-ID TEST-EVENT-INDEX)))
  (evaluate "set-event-index" '()))


(defun get-event () "" 
  (let ((event (exec #'(lambda (u) (uhppoted-get-event u TEST-DEVICE-ID TEST-EVENT-INDEX)))))
    (evaluate "get-event" 
              (list (make-result :field "event index"       :expected 51                    :value (event-index      event))
                    (make-result :field "event timestamp"   :expected "2022-04-15 12:29:15" :value (event-timestamp  event))
                    (make-result :field "event type"        :expected 6                     :value (event-event-type event))
                    (make-result :field "event granted"     :expected T                     :value (event-granted    event))
                    (make-result :field "event door"        :expected 3                     :value (event-door       event))
                    (make-result :field "event direction"   :expected 1                     :value (event-direction  event))
                    (make-result :field "event card number" :expected 8165538               :value (event-card       event))
                    (make-result :field "event reason"      :expected 21                    :value (event-reason     event))))))


(defun record-special-events () "" 
  (exec #'(lambda (u) (uhppoted-record-special-events u TEST-DEVICE-ID t)))
  (evaluate "record-special-events" '()))


(defun get-time-profile () "" 
  (let ((profile (exec #'(lambda (u) (uhppoted-get-time-profile u TEST-DEVICE-ID TEST-PROFILE-ID)))))
    (evaluate "get-time-profile" 
              (list (make-result :field "profile ID"              :expected 49           :value (time-profile-id            profile))
                    (make-result :field "linked profile"          :expected 71           :value (time-profile-linked        profile))
                    (make-result :field "profile 'from' date'"    :expected "2022-02-01" :value (time-profile-from          profile))
                    (make-result :field "profile 'to' date'"      :expected "2022-06-30" :value (time-profile-to            profile))
                    (make-result :field "profile Monday"          :expected T            :value (time-profile-monday        profile))
                    (make-result :field "profile Tuesday"         :expected nil          :value (time-profile-tuesday       profile))
                    (make-result :field "profile Wednesday"       :expected T            :value (time-profile-wednesday     profile))
                    (make-result :field "profile Thursday"        :expected T            :value (time-profile-thursday      profile))
                    (make-result :field "profile Friday"          :expected nil          :value (time-profile-friday        profile))
                    (make-result :field "profile Saturday"        :expected nil          :value (time-profile-saturday      profile))
                    (make-result :field "profile Sunday"          :expected T            :value (time-profile-sunday        profile))
                    (make-result :field "profile segment 1 start" :expected "08:30"      :value (time-profile-segment1start profile))
                    (make-result :field "profile segment 1 end"   :expected "11:30"      :value (time-profile-segment1end   profile))
                    (make-result :field "profile segment 2 start" :expected "00:00"      :value (time-profile-segment2start profile))
                    (make-result :field "profile segment 2 end"   :expected "00:00"      :value (time-profile-segment2end   profile))
                    (make-result :field "profile segment 3 start" :expected "00:00"      :value (time-profile-segment3start profile))
                    (make-result :field "profile segment 3 end"   :expected "18:00"      :value (time-profile-segment3end   profile))))))


(defun set-time-profile () "" 
  (let ((profile (make-time-profile :ID        49
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
    (exec #'(lambda (u) (uhppoted-set-time-profile u TEST-DEVICE-ID profile)))
    (evaluate "set-time-profile" '())))


(defun clear-time-profiles () "" 
  (exec #'(lambda (u) (uhppoted-clear-time-profiles u TEST-DEVICE-ID)))
  (evaluate "clear-time-profiles" '()))


(defun add-task () "" 
  (let ((task (make-task :task      4
                         :door      3
                         :from      "2022-02-01"
                         :to        "2022-06-30"
                         :monday    t
                         :tuesday   nil
                         :wednesday t
                         :thursday  t
                         :friday    nil
                         :saturday  nil
                         :sunday    t
                         :at        "09:45"
                         :cards     11)))
    (exec #'(lambda (u) (uhppoted-add-task u TEST-DEVICE-ID task)))
    (evaluate "add-task" '())))


(defun refresh-tasklist () "" 
  (exec #'(lambda (u) (uhppoted-refresh-tasklist u TEST-DEVICE-ID)))
  (evaluate "refresh-tasklist" '()))


(defun clear-tasklist () "" 
  (exec #'(lambda (u) (uhppoted-clear-tasklist u TEST-DEVICE-ID)))
  (evaluate "clear-tasklist" '()))


(defun evaluate (tag results)
  (let ((ok t))
    (loop for v in results
      do (unless (equal (result-expected v) (result-value v)) 
                 (progn
                   (setf ok nil)
                   (format t "~21a incorrect ~a (expected:~a, got:~a)~%" 
                             tag 
                             (result-field v) 
                             (result-expected v) 
                             (result-value v)))))
    (if ok (passed tag) (failed tag))))


(defun passed (tag) ""
  (format t "~21a ok~%" tag))

(defun failed (tag) ""
  (format t "~21a failed~%" tag))
; (error 'failed :message  "get-event    FAILED"))))
