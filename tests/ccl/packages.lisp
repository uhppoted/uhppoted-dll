(in-package :common-lisp-user)

(defpackage :tests
  (:use :common-lisp 
		:ccl
		:uhppoted)
  (:export get-controllers
		       get-controller
		       set-address
		       get-status
           get-status-no-event
           get-time
           set-time
           get-listener
           set-listener
           get-door-control
           set-door-control
           open-door
           get-cards
           get-card
           get-card-by-index
           put-card
           delete-card
           delete-cards
           get-event-index
           set-event-index
           get-event
           record-special-events
           get-time-profile
           set-time-profile
           clear-time-profiles
           add-task
           refresh-tasklist
           clear-tasklist
           set-pc-control
           set-interlock
           activate-keypads
           set-door-passcodes
           restore-default-parameters
           listen-events
           internationalisation
           structs
           failed
           message))

