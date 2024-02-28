(in-package :tests)

(defconstant TEST-DEVICE-ID   405419896)
(defconstant TEST-DEVICE-ID2  303986753)
(defconstant TEST-CARD-NUMBER 8165538)
(defconstant TEST-CARD-INDEX  19)
(defconstant TEST-PIN         7531)
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


(defun get-status-no-event () "" 
  (let* ((status (exec #'(lambda (u) (uhppoted-get-status u TEST-DEVICE-ID2))))
         (event  (status-event status)))
    (evaluate "get-status-no-event" 
              (list (make-result :field "device ID"         :expected 303986753             :value (status-id        status))
                    (make-result :field "system date/time"  :expected "2022-03-19 15:48:32" :value (status-timestamp status))
                    (make-result :field "doors state"       :expected '(t nil nil t)        :value (status-doors     status))
                    (make-result :field "buttons state"     :expected '(t nil t   nil)      :value (status-buttons   status))
                    (make-result :field "relays state"      :expected 18                    :value (status-relays    status))
                    (make-result :field "inputs state"      :expected 52                    :value (status-inputs    status))
                    (make-result :field "system error"      :expected 86                    :value (status-syserror  status))
                    (make-result :field "system info"       :expected 253                   :value (status-info      status))
                    (make-result :field "sequence number"   :expected 9876                  :value (status-seqno     status))
                    (make-result :field "event index"       :expected 0                     :value (event-index      event))
                    (make-result :field "event timestamp"   :expected ""                    :value (event-timestamp  event))
                    (make-result :field "event type"        :expected 0                     :value (event-event-type event))
                    (make-result :field "event granted"     :expected 0                     :value (event-granted    event))
                    (make-result :field "event door"        :expected 0                     :value (event-door       event))
                    (make-result :field "event direction"   :expected 0                     :value (event-direction  event))
                    (make-result :field "event card number" :expected 0                     :value (event-card       event))
                    (make-result :field "event reason"      :expected 0                     :value (event-reason     event))))))


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
                    (make-result :field "card doors"       :expected '(0 1 31 75) :value (card-doors card))
                    (make-result :field "card PIN"         :expected 7531         :value (card-PIN card))))))


(defun get-card-by-index () "" 
  (let ((card (exec #'(lambda (u) (uhppoted-get-card-by-index u TEST-DEVICE-ID TEST-CARD-INDEX)))))
    (evaluate "get-card-by-index" 
              (list (make-result :field "card number"      :expected 8165538      :value (card-card-number card))
                    (make-result :field "card 'from' date" :expected "2022-01-01" :value (card-from  card))
                    (make-result :field "card 'to' date"   :expected "2022-12-31" :value (card-to    card))
                    (make-result :field "card doors"       :expected '(0 1 31 75) :value (card-doors card))
                    (make-result :field "card PIN"         :expected 7531         :value (card-PIN   card))))))

(defun put-card () "" 
  (let ((doors (make-array 4 :initial-contents '(0 1 31 75))))
    (exec #'(lambda (u) (uhppoted-put-card u TEST-DEVICE-ID TEST-CARD-NUMBER "2022-01-01" "2022-12-31" doors TEST-PIN)))
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


(defun set-pc-control () "" 
  (exec #'(lambda (u) (uhppoted-set-pc-control u TEST-DEVICE-ID t)))
  (evaluate "set-pc-control" '()))


(defun set-interlock () "" 
  (exec #'(lambda (u) (uhppoted-set-interlock u TEST-DEVICE-ID 4)))
  (evaluate "set-interlock" '()))


(defun activate-keypads () "" 
  (exec #'(lambda (u) (uhppoted-activate-keypads u TEST-DEVICE-ID t t nil t)))
  (evaluate "activate-keypads" '()))


(defun set-door-passcodes() "" 
  (exec #'(lambda (u) (uhppoted-set-door-passcodes u TEST-DEVICE-ID TEST-DOOR 12345 999999 0 54321)))
  (evaluate "set-door-passcodes" '()))


(defun restore-default-parameters() "" 
  (exec #'(lambda (u) (uhppoted-restore-default-parameters u TEST-DEVICE-ID)))
  (evaluate "restore-default-parameters" '()))


(defun internationalisation () "" 
  (let ((mode-normally-open              (uhppoted-lookup lookup-mode         normally-open                                ""))
        (mode-normally-closed            (uhppoted-lookup lookup-mode         normally-closed                              ""))
        (mode-controlled                 (uhppoted-lookup lookup-mode         controlled                                   ""))
        (event-direction-in              (uhppoted-lookup lookup-direction    direction-in                                 ""))
        (event-direction-out             (uhppoted-lookup lookup-direction    direction-out                                ""))
        (event-none                      (uhppoted-lookup lookup-event-type   event-type-none                              ""))
        (event-swipe                     (uhppoted-lookup lookup-event-type   event-type-swipe                             ""))
        (event-door                      (uhppoted-lookup lookup-event-type   event-type-door                              ""))
        (event-alarm                     (uhppoted-lookup lookup-event-type   event-type-alarm                             ""))
        (event-overwritten               (uhppoted-lookup lookup-event-type   event-type-overwritten                       ""))
        (none                            (uhppoted-lookup lookup-event-reason event-reason-none                            ""))
        (swipe                           (uhppoted-lookup lookup-event-reason event-reason-swipe                           ""))
        (swipe-open                      (uhppoted-lookup lookup-event-reason event-reason-swipe-open                      ""))
        (swipe-close                     (uhppoted-lookup lookup-event-reason event-reason-swipe-close                     ""))
        (denied                          (uhppoted-lookup lookup-event-reason event-reason-denied                          ""))
        (no-access-rights                (uhppoted-lookup lookup-event-reason event-reason-no-access-rights                ""))
        (incorrect-password              (uhppoted-lookup lookup-event-reason event-reason-incorrect-password              ""))
        (anti-passback                   (uhppoted-lookup lookup-event-reason event-reason-anti-passback                   ""))
        (more-cards                      (uhppoted-lookup lookup-event-reason event-reason-more-cards                      ""))
        (first-card-open                 (uhppoted-lookup lookup-event-reason event-reason-first-card-open                 ""))
        (door-is-normally-closed         (uhppoted-lookup lookup-event-reason event-reason-door-is-normally-closed         ""))
        (interlock                       (uhppoted-lookup lookup-event-reason event-reason-interlock                       ""))
        (not-in-allowed-time-period      (uhppoted-lookup lookup-event-reason event-reason-not-in-allowed-time-period      ""))
        (invalid-timezone                (uhppoted-lookup lookup-event-reason event-reason-invalid-timezone                ""))
        (access-denied                   (uhppoted-lookup lookup-event-reason event-reason-access-denied                   ""))
        (push-button-ok                  (uhppoted-lookup lookup-event-reason event-reason-pushbutton-ok                   ""))
        (door-opened                     (uhppoted-lookup lookup-event-reason event-reason-door-opened                     ""))
        (door-closed                     (uhppoted-lookup lookup-event-reason event-reason-door-closed                     ""))
        (door-opened-supervisor-password (uhppoted-lookup lookup-event-reason event-reason-door-opened-supervisor-password ""))
        (controller-power-on             (uhppoted-lookup lookup-event-reason event-reason-controller-power-on             ""))
        (controller-reset                (uhppoted-lookup lookup-event-reason event-reason-controller-reset                ""))
        (pushbutton-invalid-door-locked  (uhppoted-lookup lookup-event-reason event-reason-pushbutton-invalid-door-locked  ""))
        (pushbutton-invalid-offline      (uhppoted-lookup lookup-event-reason event-reason-pushbutton-invalid-offline      ""))
        (pushbutton-invalid-interlock    (uhppoted-lookup lookup-event-reason event-reason-pushbutton-invalid-interlock    ""))
        (pushbutton-invalid-threat       (uhppoted-lookup lookup-event-reason event-reason-pushbutton-invalid-threat       ""))
        (door-open-too-long              (uhppoted-lookup lookup-event-reason event-reason-door-open-too-long              ""))
        (forced-open                     (uhppoted-lookup lookup-event-reason event-reason-forced-open                     ""))
        (fire                            (uhppoted-lookup lookup-event-reason event-reason-fire                            ""))
        (forced-closed                   (uhppoted-lookup lookup-event-reason event-reason-forced-closed                   ""))
        (theft-prevention                (uhppoted-lookup lookup-event-reason event-reason-theft-prevention                ""))
        (zone24x7                        (uhppoted-lookup lookup-event-reason event-reason-zone-24x7                       ""))
        (emergency                       (uhppoted-lookup lookup-event-reason event-reason-emergency                       ""))
        (remote-open-door                (uhppoted-lookup lookup-event-reason event-reason-remote-open-door                ""))
        (remote-open-door-usb-reader     (uhppoted-lookup lookup-event-reason event-reason-remote-open-door-usb-reader     "")))
    (evaluate "lookup" 
              (list 
                (make-result :field "normally open"                   :expected "normally open"                     :value mode-normally-open)
                (make-result :field "normally closed"                 :expected "normally closed"                   :value mode-normally-closed)
                (make-result :field "controlled"                      :expected "controlled"                        :value mode-controlled)
                (make-result :field "direction:in"                    :expected "in"                                :value event-direction-in)
                (make-result :field "direction:out"                   :expected "out"                               :value event-direction-out)
                (make-result :field "event type:none"                 :expected "none"                              :value event-none)
                (make-result :field "event type:swipe"                :expected "swipe"                             :value event-swipe)
                (make-result :field "event type:door"                 :expected "door"                              :value event-door)
                (make-result :field "event type:alarm"                :expected "alarm"                             :value event-alarm)
                (make-result :field "event type:overwritten"          :expected "overwritten"                       :value event-overwritten)
                (make-result :field "none"                            :expected ""                                  :value none)
                (make-result :field "swipe"                           :expected "swipe"                             :value swipe)
                (make-result :field "swipe open"                      :expected "swipe open"                        :value swipe-open)
                (make-result :field "swipe close"                     :expected "swipe close"                       :value swipe-close)
                (make-result :field "denied"                          :expected "swipe:denied (system)"             :value denied)
                (make-result :field "no_access_rights"                :expected "no access rights"                  :value no-access-rights)
                (make-result :field "incorrect-password"              :expected "incorrect password"                :value incorrect-password)
                (make-result :field "anti-passback"                   :expected "anti-passback"                     :value anti-passback)
                (make-result :field "more-cards"                      :expected "more cards"                        :value more-cards)
                (make-result :field "first-card-open"                 :expected "first card open"                   :value first-card-open)
                (make-result :field "door-is-normally-closed"         :expected "door is normally closed"           :value door-is-normally-closed)
                (make-result :field "interlock"                       :expected "interlock"                         :value interlock)
                (make-result :field "not-in-allowed-time-period"      :expected "not in allowed time period"        :value not-in-allowed-time-period)
                (make-result :field "invalid-timezone"                :expected "invalid timezone"                  :value invalid-timezone)
                (make-result :field "access-denied"                   :expected "access denied"                     :value access-denied)
                (make-result :field "push-button-ok"                  :expected "pushbutton ok"                     :value push-button-ok)
                (make-result :field "door-opened"                     :expected "door opened"                       :value door-opened)
                (make-result :field "door-closed"                     :expected "door closed"                       :value door-closed)
                (make-result :field "door-opened-supervisor-password" :expected "door opened (supervisor password)" :value door-opened-supervisor-password)
                (make-result :field "controller-power-on"             :expected "controller power on"               :value controller-power-on)
                (make-result :field "controller-reset"                :expected "controller reset"                  :value controller-reset)
                (make-result :field "pushbutton-invalid-door-locked"  :expected "pushbutton invalid (door locked)"  :value pushbutton-invalid-door-locked)
                (make-result :field "pushbutton-invalid-offline"      :expected "pushbutton invalid (offline)"      :value pushbutton-invalid-offline)
                (make-result :field "pushbutton-invalid-interlock"    :expected "pushbutton invalid (interlock)"    :value pushbutton-invalid-interlock)
                (make-result :field "pushbutton-invalid-threat"       :expected "pushbutton invalid (threat)"       :value pushbutton-invalid-threat)
                (make-result :field "door-open-too-long"              :expected "door open too long"                :value door-open-too-long)
                (make-result :field "forced-open"                     :expected "forced open"                       :value forced-open)
                (make-result :field "fire"                            :expected "fire"                              :value fire)
                (make-result :field "forced-closed"                   :expected "forced closed"                     :value forced-closed)
                (make-result :field "theft-prevention"                :expected "theft prevention"                  :value theft-prevention)
                (make-result :field "24x7 zone"                       :expected "24x7 zone"                         :value zone24x7)
                (make-result :field "emergency"                       :expected "emergency"                         :value emergency)
                (make-result :field "remote-open-door"                :expected "remote open door"                  :value remote-open-door)
                (make-result :field "remote-open-door-usb-reader"     :expected "remote open door (USB reader)"     :value remote-open-door-usb-reader)))))


(defun structs () "" 
  (handler-bind
    ((uhppoted-error #'(lambda (c) (error 'failed :message  (uhppoted:message c)))))
    (progn
      (uhppoted #'(lambda (u) (uhppoted-get-device u 4294967295)) 
                :bind-addr      "0.0.0.0"
                :broadcast-addr "255.255.255.255"
                :listen-addr    "0.0.0.0:60001"
                :timeout        2500
                :debug          t)

      (uhppoted #'(lambda (u) (uhppoted-get-device u 4294967294)) 
                :bind-addr      "0.0.0.0"
                :broadcast-addr "255.255.255.255"
                :listen-addr    "0.0.0.0:60001"
                :timeout        2500
                :debug          nil)
      (passed "structs"))))


(defun evaluate (tag results)
  (let ((ok t))
    (loop for v in results
      do (unless (equal (result-expected v) (result-value v)) 
                 (progn
                   (setf ok nil)
                   (format t "~26a incorrect ~a (expected:~a, got:~a)~%" 
                             tag 
                             (result-field v) 
                             (result-expected v) 
                             (result-value v)))))
    (if ok (passed tag) (failed tag))))


(defun passed (tag) ""
  (format t "~26a ok~%" tag))

(defun failed (tag) ""
  (format t "~26a failed~%" tag))

