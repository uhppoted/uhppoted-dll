(in-package :common-lisp-user)

(defpackage :uhppoted 
  (:use :common-lisp
		    :ccl)
  (:export uhppoted
           uhppoted-get-devices
           uhppoted-get-device
           uhppoted-set-address
           uhppoted-get-status
           uhppoted-get-time
           uhppoted-set-time
           uhppoted-get-listener
           uhppoted-set-listener
           uhppoted-get-door-control
           uhppoted-set-door-control
           uhppoted-open-door
           uhppoted-get-cards
           uhppoted-get-card
           uhppoted-get-card-by-index
           uhppoted-put-card
           uhppoted-delete-card
           uhppoted-delete-cards
           uhppoted-get-event-index
           uhppoted-set-event-index
           uhppoted-get-event
           uhppoted-record-special-events
           uhppoted-get-time-profile
           uhppoted-set-time-profile
           uhppoted-clear-time-profiles
           uhppoted-add-task
           uhppoted-refresh-tasklist
           uhppoted-clear-tasklist
           uhppoted-set-pc-control
           uhppoted-set-interlock
           uhppoted-activate-keypads
           uhppoted-set-door-passcodes
           uhppoted-restore-default-parameters
           uhppoted-error
           with-warning
           message
           uhppoted-lookup

           device-id
           device-address
           device-subnet
           device-gateway
           device-MAC
           device-version
           device-date
       
           status-id
           status-timestamp
           status-doors
           status-buttons
           status-relays
           status-inputs
           status-syserror
           status-info
           status-seqno
           status-event

           event-timestamp
           event-index
           event-event-type
           event-granted
           event-door
           event-direction
           event-card
           event-reason

           door-control-mode
           door-control-delay

           card-card-number
           card-from
           card-to
           card-doors
           card-PIN

           make-time-profile
           time-profile-id
           time-profile-linked
           time-profile-from
           time-profile-to
           time-profile-monday
           time-profile-tuesday
           time-profile-wednesday
           time-profile-thursday
           time-profile-friday
           time-profile-saturday
           time-profile-sunday
           time-profile-segment1start
           time-profile-segment1end
           time-profile-segment2start
           time-profile-segment2end
           time-profile-segment3start
           time-profile-segment3end

           make-task
           task-task
           task-door
           task-from
           task-to
           task-monday
           task-tuesday
           task-wednesday
           task-thursday
           task-friday
           task-saturday
           task-sunday
           task-at
           task-cards

           normally-open
           normally-closed
           controlled

           direction-in
           direction-out

           event-type-none
           event-type-swipe
           event-type-door
           event-type-alarm
           event-type-overwritten

           event-reason-none
           event-reason-swipe
           event-reason-swipe-open
           event-reason-swipe-close
           event-reason-denied
           event-reason-no-access-rights
           event-reason-incorrect-password
           event-reason-anti-passback
           event-reason-more-cards
           event-reason-first-card-open
           event-reason-door-is-normally-closed
           event-reason-interlock
           event-reason-not-in-allowed-time-period
           event-reason-invalid-timezone
           event-reason-access-denied
           event-reason-pushbutton-ok
           event-reason-door-opened
           event-reason-door-closed
           event-reason-door-opened-supervisor-password
           event-reason-controller-power-on
           event-reason-controller-reset
           event-reason-pushbutton-invalid-door-locked
           event-reason-pushbutton-invalid-offline
           event-reason-pushbutton-invalid-interlock
           event-reason-pushbutton-invalid-threat
           event-reason-door-open-too-long
           event-reason-forced-open
           event-reason-fire
           event-reason-forced-closed
           event-reason-theft-prevention
           event-reason-zone-24x7
           event-reason-emergency
           event-reason-remote-open-door
           event-reason-remote-open-door-usb-reader
           
           lookup-mode
           lookup-direction
           lookup-event-type
           lookup-event-reason))

