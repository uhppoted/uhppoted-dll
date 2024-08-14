(in-package uhppoted)

#+linux-target
(open-shared-library 
  (native-translated-namestring 
    (make-pathname :directory (getenv "LD_LIBRARY_PATH") :name "libuhppoted" :type "so")))

#+darwin-target
(open-shared-library 
  (native-translated-namestring 
    (make-pathname :directory (getenv "DYLD_LIBRARY_PATH") :name "libuhppoted" :type "dylib")))

#+windows-target
(open-shared-library 
  (native-translated-namestring 
    (make-pathname :directory (getenv "LIBRARY_PATH") :name "uhppoted" :type "dll")))

(defconstant LOOKUP-MODE         "door.mode")
(defconstant LOOKUP-DIRECTION    "event.direction")
(defconstant LOOKUP-EVENT-TYPE   "event.type")
(defconstant LOOKUP-EVENT-REASON "event.reason")

(defconstant ModeNormallyOpen   "normally open")
(defconstant ModeNormallyClosed "normally closed")
(defconstant ModeControlled     "controlled")
(defconstant ModeUnknown        "unknown")

(defconstant DirectionIn      "in")
(defconstant DirectionOut     "out")
(defconstant DirectionUnknown "unknown")

(defconstant EventTypeNone        "none")
(defconstant EventTypeSwipe       "swipe")
(defconstant EventTypeDoor        "door")
(defconstant EventTypeAlarm       "alarm")
(defconstant EventTypeOverwritten "overwritten")
(defconstant EventTypeUnknown     "unknown")

(defconstant EventReasonNone                         "")
(defconstant EventReasonSwipe                        "swipe")
(defconstant EventReasonSwipeOpen                    "swipe open")
(defconstant EventReasonSwipeClose                   "swipe close")
(defconstant EventReasonDenied                       "swipe:denied (system)")
(defconstant EventReasonNoAccessRights               "no access rights")
(defconstant EventReasonIncorrectPassword            "incorrect password")
(defconstant EventReasonAntiPassback                 "anti-passback")
(defconstant EventReasonMoreCards                    "more cards")
(defconstant EventReasonFirstCardOpen                "first card open")
(defconstant EventReasonDoorIsNormallyClosed         "door is normally closed")
(defconstant EventReasonInterlock                    "interlock")
(defconstant EventReasonNotInAllowedTimePeriod       "not in allowed time period")
(defconstant EventReasonInvalidTimezone              "invalid timezone")
(defconstant EventReasonAccessDenied                 "access denied")
(defconstant EventReasonPushButtonOk                 "pushbutton ok")
(defconstant EventReasonDoorOpened                   "door opened")
(defconstant EventReasonDoorClosed                   "door closed")
(defconstant EventReasonDoorOpenedSupervisorPassword "door opened (supervisor password)")
(defconstant EventReasonControllerPowerOn            "controller power on")
(defconstant EventReasonControllerReset              "controller reset")
(defconstant EventReasonPushbuttonInvalidDoorLocked  "pushbutton invalid (door locked)")
(defconstant EventReasonPushbuttonInvalidOffline     "pushbutton invalid (offline)")
(defconstant EventReasonPushbuttonInvalidInterlock   "pushbutton invalid (interlock)")
(defconstant EventReasonPushbuttonInvalidThreat      "pushbutton invalid (threat)")
(defconstant EventReasonDoorOpenTooLong              "door open too long")
(defconstant EventReasonForcedOpen                   "forced open")
(defconstant EventReasonFire                         "fire")
(defconstant EventReasonForcedClosed                 "forced closed")
(defconstant EventReasonTheftPrevention              "theft prevention")
(defconstant EventReasonZone24x7                     "24x7 zone")
(defconstant EventReasonEmergency                    "emergency")
(defconstant EventReasonRemoteOpenDoor               "remote open door")
(defconstant EventReasonRemoteOpenDoorUSBReader      "remote open door (USB reader)")
(defconstant EventReasonUnknown                      "unknown")

(defstruct device ID 
                  address
                  subnet
                  gateway
                  MAC
                  version
                  date)

(defstruct status ID 
                  timestamp
                  doors
                  buttons
                  relays
                  inputs
                  syserror
                  info
                  seqno
                  event)

(defstruct event timestamp
                 index
                 event-type
                 granted
                 door
                 direction
                 card
                 reason)

(defstruct door-control mode
                        delay)

(defstruct card card-number 
                from
                to
                doors
                PIN)

(defstruct time-profile ID
                        linked
                        from
                        to
                        monday
                        tuesday
                        wednesday
                        thursday
                        friday
                        saturday
                        sunday
                        segment1start
                        segment1end
                        segment2start
                        segment2end
                        segment3start
                        segment3end)

(defstruct task task
                door
                from
                to
                monday
                tuesday
                wednesday
                thursday
                friday
                saturday
                sunday
                at
                cards)

(defstruct listen-event timestamp
                        index
                        event-type
                        granted
                        door
                        direction
                        card
                        reason)

(def-foreign-type nil
  (:struct :UDEVICE (:id        :int)
	                  (:address   :address)
                    (:transport :address)))

(def-foreign-type nil
  (:struct :UDEVICES (:N       :int)
                     (:devices :address)))

(def-foreign-type nil
  (:struct :UHPPOTE (:bind      :address)
                    (:broadcast :address)
                    (:listen    :address)
                    (:timeout   :int)
                    (:devices   :address)
                    (:debug     :int)))

(def-foreign-type nil
  (:struct :GoDevice (:id      :unsigned-long)
                     (:address :address)
                     (:subnet  :address)
                     (:gateway :address)
                     (:MAC     :address)
                     (:version :address)
                     (:date    :address)))

(def-foreign-type nil
  (:struct :GoEvent (:timestamp  :address)
                    (:index      :unsigned-fullword)
                    (:event-type :unsigned-byte)
                    (:granted    :unsigned-byte)
                    (:door       :unsigned-byte)
                    (:direction  :unsigned-byte)
                    (:card       :unsigned-fullword)
                    (:reason     :unsigned-byte)))

(def-foreign-type nil
  (:struct :GoStatus (:id        :unsigned-fullword)
                     (:timestamp :address)
                     (:doors     :address)
                     (:buttons   :address)
                     (:relays    :unsigned-byte)
                     (:inputs    :unsigned-byte)
                     (:syserror  :unsigned-byte)
                     (:info      :unsigned-byte)
                     (:seqno     :unsigned-fullword)
                     (:event     :address)))

(def-foreign-type nil
  (:struct :GoDoorControl (:mode  :unsigned-byte)
                          (:delay :unsigned-byte)))

(def-foreign-type nil
  (:struct :GoCard (:card-number :unsigned-fullword)
                   (:from        :address)
                   (:to          :address)
                   (:doors       :address)
                   (:PIN         :unsigned-fullword)))

(def-foreign-type nil
  (:struct :GoTimeProfile (:ID            :unsigned-byte)
                          (:linked        :unsigned-byte)
                          (:from          :address)
                          (:to            :address)
                          (:monday        :unsigned-byte)
                          (:tuesday       :unsigned-byte)
                          (:wednesday     :unsigned-byte)
                          (:thursday      :unsigned-byte)
                          (:friday        :unsigned-byte)
                          (:saturday      :unsigned-byte)
                          (:sunday        :unsigned-byte)
                          (:segment1start :address)
                          (:segment1end   :address)
                          (:segment2start :address)
                          (:segment2end   :address)
                          (:segment3start :address)
                          (:segment3end   :address)))

(def-foreign-type nil
  (:struct :GoTask (:task      :unsigned-byte)
                   (:door      :unsigned-byte)
                   (:from      :address)
                   (:to        :address)
                   (:monday    :unsigned-byte)
                   (:tuesday   :unsigned-byte)
                   (:wednesday :unsigned-byte)
                   (:thursday  :unsigned-byte)
                   (:friday    :unsigned-byte)
                   (:saturday  :unsigned-byte)
                   (:sunday    :unsigned-byte)
                   (:at        :address)
                   (:cards     :unsigned-byte)))

(def-foreign-type nil
  (:struct :GoListenEvent (:controller :unsigned-fullword) 
                          (:timestamp  :address)
                          (:index      :unsigned-fullword)
                          (:event-type :unsigned-byte)
                          (:granted    :unsigned-byte)
                          (:door       :unsigned-byte)
                          (:direction  :unsigned-byte)
                          (:card       :unsigned-fullword)
                          (:reason     :unsigned-byte)))

(define-condition uhppoted-error (error)
  ((message :initarg :message :reader message)))

(defun go-error (cstr) "Converts a 'C' char * returned by the Go FFI to a string and frees the 'C' string"
  (with-macptrs ((p cstr))
    (%get-cstring p)))

(defun go-string (cstr) "Converts a 'C' char * returned by the Go FFI to a string and frees the 'C' string"
  (with-macptrs ((p cstr))
    (%get-cstring p)))

(defun go-sizeof (type) "" 
  (cond ((eq type :udevice) 24)
	      (T 0)))

(defun uhppoted (f &key (bind-addr "") (broadcast-addr "") (listen-addr "") (timeout 5) (controllers NIL) (debug NIL)) ""
  (%stack-block ((devices (* (length controllers) (go-sizeof :udevice))))
    (rletz ((udevices (:struct UDEVICES) :N       (length controllers) 
                                         :devices devices)
			      (uhppote (:struct :UHPPOTE) :bind      (ccl::make-cstring bind-addr)
                                        :broadcast (ccl::make-cstring broadcast-addr)
                                        :listen    (ccl::make-cstring listen-addr)
                                        :timeout   timeout
                                        :devices   udevices
                                        :debug     (cond (debug 1) (T 0))))
      (loop for (id addr transport) in controllers
        do (progn
		         (setf (pref devices :udevice.id) id)
			       (setf (pref devices :udevice.address)   (ccl::make-cstring addr))
             (setf (pref devices :udevice.transport) (ccl::make-cstring (or transport "")))
			       (%setf-macptr devices (%inc-ptr devices (go-sizeof :udevice)))))

	    (unwind-protect
	      (restart-case (funcall f uhppote)
				  (ignore       ()      nil)
					(use-value    (value) value)
					(with-warning (err)   (warn err)))
        (progn
		      (free (pref uhppote :UHPPOTE.bind))
          (free (pref uhppote :UHPPOTE.broadcast))
          (free (pref uhppote :UHPPOTE.listen))  
          (let ((p (pref (pref uhppote :UHPPOTE.devices) :UDEVICES.devices)))
            (loop for a from 1 to (length controllers)
              do (progn
                   (free (pref p :UDEVICE.address))
                   (free (pref p :UDEVICE.transport))
			       (%setf-macptr p (%inc-ptr p (go-sizeof :udevice)))))))))))


(defun uhppoted-get-devices (uhppote &optional (N 16)) "Retrieves a list of device IDs on the local LAN"
  (destructuring-bind  (p q) (uhppoted-get-devices-n uhppote N)
	  (cond ((>= N p) (subseq q 0 p))
		  (T (uhppoted-get-devices uhppote (+ N 16))))))

(defun uhppoted-get-devices-n (uhppote max) ""
    (multiple-value-bind (array  arrayp)  (make-heap-ivector max '(unsigned-byte 32))
    (multiple-value-bind (errmsg errmsgp) (make-heap-ivector 256 '(unsigned-byte 1))
	  (unwind-protect
	    (rletz ((N    :signed-long max)
              (errN :signed-long 256))
        (with-macptrs ((err (external-call "GetDevices" :address uhppote 
                                                        :address arrayp 
	  									                                  :address N 
                                                        :address errmsgp
                                                        :address errN
                                                        :signed-long)))
          ; CCL absolutely insists 'err' is a foreign pointer (because with-macptrs maybe ?)
          (unless (%null-ptr-p err) (error 'uhppoted-error :message (%get-cstring errmsgp)))
		      (list (%get-signed-long N) array)))
      (dispose-heap-ivector errmsg)
	    (dispose-heap-ivector array)))))


(defun uhppoted-get-device (uhppote device-id) "Retrieves the device information for a controller"
  (multiple-value-bind (address addressp) (make-heap-ivector 16  '(unsigned-byte 1))
  (multiple-value-bind (subnet  subnetp)  (make-heap-ivector 16  '(unsigned-byte 1))
  (multiple-value-bind (gateway gatewayp) (make-heap-ivector 16  '(unsigned-byte 1))
  (multiple-value-bind (MAC     MACp)     (make-heap-ivector 18  '(unsigned-byte 1))
  (multiple-value-bind (version versionp) (make-heap-ivector 7   '(unsigned-byte 1))
  (multiple-value-bind (date    datep)    (make-heap-ivector 11  '(unsigned-byte 1))
  (multiple-value-bind (errmsg  errmsgp)  (make-heap-ivector 256 '(unsigned-byte 1))
  (unwind-protect
    (rletz ((device (:struct :GoDevice))
            (errN :signed-long 256))

      (setf (pref device :GoDevice.address) addressp)
      (setf (pref device :GoDevice.subnet)  subnetp)
      (setf (pref device :GoDevice.gateway) gatewayp)
      (setf (pref device :GoDevice.MAC)     MACp)
      (setf (pref device :GoDevice.version) versionp)
      (setf (pref device :GoDevice.date)    datep)

      (with-macptrs ((err (external-call "GetDevice" :address uhppote 
		  						                                   :address device 
			  						                                 :unsigned-long device-id 
                                                     :address errmsgp
                                                     :address errN
                                                     :signed-long)))
        ; CCL absolutely insists 'err' is a foreign pointer (because with-macptrs maybe ?)
        (unless (%null-ptr-p err) (error 'uhppoted-error :message (%get-cstring errmsgp)))
	      (make-device :id      (%get-unsigned-long device)
                     :address (%get-cstring (pref device :GoDevice.address))
                     :subnet  (%get-cstring (pref device :GoDevice.subnet))
                     :gateway (%get-cstring (pref device :GoDevice.gateway))
                     :MAC     (%get-cstring (pref device :GoDevice.MAC))
                     :version (%get-cstring (pref device :GoDevice.version))
                     :date    (%get-cstring (pref device :GoDevice.date)))))

      (dispose-heap-ivector address)
      (dispose-heap-ivector subnet)
      (dispose-heap-ivector gateway)
      (dispose-heap-ivector MAC)
      (dispose-heap-ivector version)
      (dispose-heap-ivector date)
      (dispose-heap-ivector errmsg))))))))))


(defun uhppoted-set-address (uhppote device-id ip-addr subnet-mask gateway-addr) "Sets the controller IP address, subnet mask and gateway"
  (with-cstrs ((address ip-addr)
               (subnet  subnet-mask)
               (gateway gateway-addr))
  (multiple-value-bind (errmsg  errmsgp)  (make-heap-ivector 256 '(unsigned-byte 1))
  (unwind-protect
    (rlet ((errN :signed-long 256))
    (with-macptrs ((err (external-call "SetAddress" :address uhppote 
		  		                                					:unsigned-long device-id 
			  						                                :address address  
                                                    :address subnet
                                                    :address gateway
                                                    :address errmsgp
                                                    :address errN
                                                    :signed-long)))
      ; CCL absolutely insists 'err' is a foreign pointer (because with-macptrs maybe ?)
      (unless (%null-ptr-p err) (error 'uhppoted-error :message (%get-cstring errmsgp)))
      t))
      (dispose-heap-ivector errmsg)))))


(defun uhppoted-get-status (uhppote device-id) "Retrieves a controller status information"
  (multiple-value-bind (sysdatetime sysdatetimep) (make-heap-ivector 20  '(unsigned-byte 1))
  (multiple-value-bind (timestamp   timestampp)   (make-heap-ivector 20  '(unsigned-byte 1))
  (multiple-value-bind (errmsg      errmsgp)      (make-heap-ivector 256 '(unsigned-byte 1))
  (unwind-protect
    (%stack-block ((doors   4)
	  			         (buttons 4))
      (rletz ((event  (:struct :GoEvent)  :timestamp timestampp)
              (status (:struct :GoStatus) :timestamp sysdatetimep
                                          :doors     doors
			  		                              :buttons   buttons
				  	                              :event     event)
              (errN :signed-long 256))
        (with-macptrs ((err (external-call "GetStatus" :address uhppote 
			  							                                 :address status
				  						                                 :unsigned-long device-id 
                                                       :address errmsgp
                                                       :address errN
                                                       :signed-long)))
          ; CCL absolutely insists 'err' is a foreign pointer (because with-macptrs maybe ?)
          (unless (%null-ptr-p err) (error 'uhppoted-error :message (%get-cstring errmsgp)))
          (make-status :id        (%get-unsigned-long status)
			  		           :timestamp (%get-cstring (pref status :GoStatus.timestamp))
				  	           :doors     (list (if (equal 0 (%get-unsigned-byte doors 0)) nil T)
					  				                    (if (equal 0 (%get-unsigned-byte doors 1)) nil T)
						  			                    (if (equal 0 (%get-unsigned-byte doors 2)) nil T)
							  		                    (if (equal 0 (%get-unsigned-byte doors 3)) nil T))
					             :buttons   (list (if (equal 0 (%get-unsigned-byte buttons 0)) nil T)
							                          (if (equal 0 (%get-unsigned-byte buttons 1)) nil T)
									                      (if (equal 0 (%get-unsigned-byte buttons 2)) nil T)
									                      (if (equal 0 (%get-unsigned-byte buttons 3)) nil T))
  					           :relays     (pref status :GoStatus.relays)
	  				           :inputs     (pref status :GoStatus.inputs)
		  			           :syserror   (pref status :GoStatus.syserror)
			  		           :info       (pref status :GoStatus.info)
				  	           :seqno      (pref status :GoStatus.seqno)
					             :event      (make-event :timestamp (%get-cstring (pref event :GoEvent.timestamp))
						  				 :index      (pref event :GoEvent.index)
							  			 :event-type (pref event :GoEvent.event-type)
								  		 :granted    (pref event :GoEvent.granted)
									  	 :door       (pref event :GoEvent.door)
									     :direction  (pref event :GoEvent.direction)
  										 :card       (pref event :GoEvent.card)
	  									 :reason     (pref event :GoEvent.reason))))))
    (dispose-heap-ivector sysdatetime)
    (dispose-heap-ivector timestamp)
    (dispose-heap-ivector errmsg))))))

(defun uhppoted-get-time (uhppote device-id) "Retrieves a controller date/time"
  (multiple-value-bind (datetime datetimep) (make-heap-ivector 20  '(unsigned-byte 1))
  (multiple-value-bind (errmsg   errmsgp)   (make-heap-ivector 256 '(unsigned-byte 1))
  (unwind-protect
    (rlet ((errN :signed-long 256))
      (with-macptrs ((err (external-call "GetTime" :address uhppote 
                                                   :address datetimep
                                                   :unsigned-long device-id 
                                                   :address errmsgp
                                                   :address errN
                                                   :signed-long)))
        ; CCL absolutely insists 'err' is a foreign pointer (because with-macptrs maybe ?)
        (unless (%null-ptr-p err) (error 'uhppoted-error :message (%get-cstring errmsgp)))
        (%get-cstring datetimep)))
  (dispose-heap-ivector datetime)
  (dispose-heap-ivector errmsg)))))


(defun uhppoted-set-time (uhppote device-id datetime) "Sets a controller date/time"
  (multiple-value-bind (errmsg   errmsgp)   (make-heap-ivector 256 '(unsigned-byte 1))
  (unwind-protect
    (with-cstrs ((dt datetime))
    (rlet ((errN :signed-long 256))
      (with-macptrs ((err (external-call "SetTime" :address uhppote 
                                                   :unsigned-long device-id 
                                                   :address dt
                                                   :address errmsgp
                                                   :address errN
                                                   :signed-long)))
        ; CCL absolutely insists 'err' is a foreign pointer (because with-macptrs maybe ?)
        (unless (%null-ptr-p err) (error 'uhppoted-error :message (%get-cstring errmsgp)))
        t)))
  (dispose-heap-ivector errmsg))))


(defun uhppoted-get-listener (uhppote device-id) "Retrieves the controller event listener address"
  (multiple-value-bind (listener listenerp) (make-heap-ivector 22  '(unsigned-byte 1))
  (multiple-value-bind (errmsg   errmsgp)   (make-heap-ivector 256 '(unsigned-byte 1))
  (unwind-protect
    (rlet ((errN :signed-long 256))
      (with-macptrs ((err (external-call "GetListener" :address uhppote 
                                                       :address listenerp
                                                       :unsigned-long device-id 
                                                       :address errmsgp
                                                       :address errN
                                                       :signed-long)))
        ; CCL absolutely insists 'err' is a foreign pointer (because with-macptrs maybe ?)
        (unless (%null-ptr-p err) (error 'uhppoted-error :message (%get-cstring errmsgp)))
        (%get-cstring listenerp)))
  (dispose-heap-ivector listener)
  (dispose-heap-ivector errmsg)))))


(defun uhppoted-set-listener (uhppote device-id listener) "Sets a controller's event listener address and port"
  (multiple-value-bind (errmsg errmsgp) (make-heap-ivector 256 '(unsigned-byte 1))
  (unwind-protect
    (with-cstrs ((addr listener))
    (rlet ((errN :signed-long 256))
      (with-macptrs ((err (external-call "SetListener" :address uhppote 
                                                       :unsigned-long device-id 
                                                       :address addr
                                                       :address errmsgp
                                                       :address errN
                                                       :signed-long)))
        ; CCL absolutely insists 'err' is a foreign pointer (because with-macptrs maybe ?)
        (unless (%null-ptr-p err) (error 'uhppoted-error :message (%get-cstring errmsgp)))
        t)))
  (dispose-heap-ivector errmsg))))


(defun uhppoted-get-door-control (uhppote device-id door) "Retrieves the door control state and open delay for a controller"
  (multiple-value-bind (errmsg errmsgp) (make-heap-ivector 256 '(unsigned-byte 1))
  (unwind-protect
    (rletz ((control (:struct :GoDoorControl))
            (errN    :signed-long 256))
      (with-macptrs ((err (external-call "GetDoorControl" :address uhppote 
                                                          :address control 
                                                          :unsigned-long device-id 
                                                          :unsigned-byte door
                                                          :address errmsgp
                                                          :address errN
                                                          :signed-long)))
        ; CCL absolutely insists 'err' is a foreign pointer (because with-macptrs maybe ?)
        (unless (%null-ptr-p err) (error 'uhppoted-error :message (%get-cstring errmsgp)))
        (make-door-control :mode  (pref control :GoDoorControl.mode)
                           :delay (pref control :GoDoorControl.delay))))
  (dispose-heap-ivector errmsg))))


(defun uhppoted-set-door-control (uhppote device-id door mode delay) "Sets the control mode and delay for a controller door"
  (multiple-value-bind (errmsg errmsgp) (make-heap-ivector 256 '(unsigned-byte 1))
  (unwind-protect
    (rlet ((errN    :signed-long 256))
    (with-macptrs ((err (external-call "SetDoorControl" :address uhppote 
                                                        :unsigned-long device-id 
                                                        :unsigned-byte door
                                                        :unsigned-byte mode
                                                        :unsigned-byte delay
                                                        :address errmsgp
                                                        :address errN
                                                        :signed-long)))
      ; CCL absolutely insists 'err' is a foreign pointer (because with-macptrs maybe ?)
      (unless (%null-ptr-p err) (error 'uhppoted-error :message (%get-cstring errmsgp)))
      t))
  (dispose-heap-ivector errmsg))))


(defun uhppoted-open-door (uhppote device-id door) "Remotely opens a controller door"
  (multiple-value-bind (errmsg errmsgp) (make-heap-ivector 256 '(unsigned-byte 1))
  (unwind-protect
    (rlet ((errN    :signed-long 256))
    (with-macptrs ((err (external-call "OpenDoor" :address uhppote 
                                                  :unsigned-long device-id 
                                                  :unsigned-byte door
                                                  :address errmsgp
                                                  :address errN
                                                  :signed-long)))
      ; CCL absolutely insists 'err' is a foreign pointer (because with-macptrs maybe ?)
      (unless (%null-ptr-p err) (error 'uhppoted-error :message (%get-cstring errmsgp)))
      t))
  (dispose-heap-ivector errmsg))))


(defun uhppoted-get-cards (uhppote device-id) "Retrieves the number of cards stored on a controller"
  (multiple-value-bind (errmsg errmsgp) (make-heap-ivector 256 '(unsigned-byte 1))
  (unwind-protect
    (rlet ((N :signed-long 0)
           (errN    :signed-long 256))
    (with-macptrs ((err (external-call "GetCards" :address uhppote 
                                                 :address N 
                                                  :unsigned-long device-id 
                                                  :address errmsgp
                                                  :address errN
                                                  :signed-long)))
      ; CCL absolutely insists 'err' is a foreign pointer (because with-macptrs maybe ?)
      (unless (%null-ptr-p err) (error 'uhppoted-error :message (%get-cstring errmsgp)))
      (%get-signed-long N)))
  (dispose-heap-ivector errmsg))))


(defun uhppoted-get-card (uhppote device-id card-number) "Retrieves card detail for a card stored on controller"
  (multiple-value-bind (from   fromp)   (make-heap-ivector 11  '(unsigned-byte 1))
  (multiple-value-bind (to     top)     (make-heap-ivector 11  '(unsigned-byte 1))
  (multiple-value-bind (errmsg errmsgp) (make-heap-ivector 256 '(unsigned-byte 1))
  (unwind-protect
    (%stack-block ((doors   4))
    (rletz ((card (:struct :GoCard) :from  fromp
                                    :to    top
                                    :doors doors)
            (errN :signed-long 256))
      (with-macptrs ((err (external-call "GetCard" :address uhppote 
                                                   :address card
                                                   :unsigned-long device-id 
                                                   :unsigned-long card-number
                                                   :address errmsgp
                                                   :address errN
                                                   :signed-long)))
      ; CCL absolutely insists 'err' is a foreign pointer (because with-macptrs maybe ?)
      (unless (%null-ptr-p err) (error 'uhppoted-error :message (%get-cstring errmsgp)))
        (make-card :card-number (%get-unsigned-long (pref card :GoCard.card-number))
                   :from        (%get-cstring       (pref card :GoCard.from))
                   :to          (%get-cstring       (pref card :GoCard.to))
                   :doors       (list (%get-unsigned-byte doors 0)
                                      (%get-unsigned-byte doors 1)
                                      (%get-unsigned-byte doors 2)
                                      (%get-unsigned-byte doors 3))
                   :PIN         (pref card :GoCard.PIN)))))
  (dispose-heap-ivector from)
  (dispose-heap-ivector to)
  (dispose-heap-ivector errmsg))))))


(defun uhppoted-get-card-by-index (uhppote device-id index) "Retrieves card detail for the card stored at an index on controller"
  (multiple-value-bind (from   fromp)   (make-heap-ivector 11  '(unsigned-byte 1))
  (multiple-value-bind (to     top)     (make-heap-ivector 11  '(unsigned-byte 1))
  (multiple-value-bind (errmsg errmsgp) (make-heap-ivector 256 '(unsigned-byte 1))
  (unwind-protect
    (%stack-block ((doors   4))
    (rletz ((card (:struct :GoCard) :from  fromp
                                    :to    top
                                    :doors doors)
            (errN :signed-long 256))
      (with-macptrs ((err (external-call "GetCardByIndex" :address uhppote 
                                                          :address card
                                                          :unsigned-long device-id 
                                                          :unsigned-long index
                                                          :address errmsgp
                                                          :address errN
                                                          :signed-long)))
      ; CCL absolutely insists 'err' is a foreign pointer (because with-macptrs maybe ?)
      (unless (%null-ptr-p err) (error 'uhppoted-error :message (%get-cstring errmsgp)))
        (make-card :card-number (%get-unsigned-long (pref card :GoCard.card-number))
                   :from        (%get-cstring       (pref card :GoCard.from))
                   :to          (%get-cstring       (pref card :GoCard.to))
                   :doors       (list (%get-unsigned-byte doors 0)
                                      (%get-unsigned-byte doors 1)
                                      (%get-unsigned-byte doors 2)
                                      (%get-unsigned-byte doors 3))
                   :PIN         (pref card :GoCard.PIN)))))
  (dispose-heap-ivector from)
  (dispose-heap-ivector to)
  (dispose-heap-ivector errmsg))))))


(defun uhppoted-put-card (uhppote device-id card-number from to doors PIN) "Adds or updates the card detail stored on a controller"
  (with-cstrs ((from_ from)
               (to_   to))
    (multiple-value-bind (doors_ pdoors) (make-heap-ivector 4 '(unsigned-byte 1))
      (unwind-protect
        (progn
          (setf (paref pdoors (:* :unsigned-byte) 0) (aref doors 0))
          (setf (paref pdoors (:* :unsigned-byte) 1) (aref doors 1))
          (setf (paref pdoors (:* :unsigned-byte) 2) (aref doors 2))
          (setf (paref pdoors (:* :unsigned-byte) 3) (aref doors 3))
          (with-macptrs ((err (external-call "PutCard" :address uhppote 
                                                       :unsigned-long device-id 
                                                       :unsigned-long card-number
                                                       :address       from_  
                                                       :address       to_
                                                       :address       pdoors
                                                       :unsigned-long PIN
                                                       :address)))
            (unless (%null-ptr-p err) (error 'uhppoted-error :message (go-error err)))
            t)))
      (dispose-heap-ivector doors_))))


(defun uhppoted-delete-card (uhppote device-id card-number) "Deletes a card from a controller"
  (with-macptrs ((err (external-call "DeleteCard" :address uhppote 
                                                  :unsigned-long device-id 
                                                  :unsigned-long card-number
                                                  :address)))
    (unless (%null-ptr-p err) (error 'uhppoted-error :message (go-error err)))
    t))


(defun uhppoted-delete-cards (uhppote device-id) "Deletes all cards from a controller"
  (with-macptrs ((err (external-call "DeleteCards" :address uhppote 
                                                   :unsigned-long device-id 
                                                   :address)))
    (unless (%null-ptr-p err) (error 'uhppoted-error :message (go-error err)))
    t))


(defun uhppoted-get-event-index (uhppote device-id) "Retrieves the current event index from a controller"
  (rletz ((index :signed-long 0))
         (with-macptrs ((err (external-call "GetEventIndex" :address uhppote 
                                                            :address index
                                                            :unsigned-long device-id 
                                                            :address)))
          (unless (%null-ptr-p err) (error 'uhppoted-error :message (go-error err)))
          (%get-unsigned-long index))))


(defun uhppoted-set-event-index (uhppote device-id index) "Retrieves the current event index from a controller"
  (with-macptrs ((err (external-call "SetEventIndex" :address uhppote 
                                                     :unsigned-long device-id 
                                                     :unsigned-long index
                                                     :address)))
    (unless (%null-ptr-p err) (error 'uhppoted-error :message (go-error err)))
    t))


(defun uhppoted-get-event (uhppote device-id index) "Retrieves the event at the index from on controller"
  (rletz ((event (:struct :GoEvent)))
    (with-macptrs ((err (external-call "GetEvent" :address uhppote 
                                                  :address event
                                                  :unsigned-long device-id 
                                                  :unsigned-long index
                                                  :address)))
      (unless (%null-ptr-p err) (error 'uhppoted-error :message (go-error err)))
      (make-event :timestamp  (go-string (pref event :GoEvent.timestamp))
                  :index      (pref event :GoEvent.index)
                  :event-type (pref event :GoEvent.event-type)
                  :granted    (if (equal 0 (pref event :GoEvent.granted)) nil T)
                  :door       (pref event :GoEvent.door)
                  :direction  (pref event :GoEvent.direction)
                  :card       (pref event :GoEvent.card)
                  :reason     (pref event :GoEvent.reason)))))


(defun uhppoted-record-special-events (uhppote device-id enabled) "Enables/disables recording additional events for a controller"
  (with-macptrs ((err (external-call "RecordSpecialEvents" :address uhppote 
                                                           :unsigned-long device-id 
                                                           :unsigned-byte (if enabled 1 0)
                                                           :address)))
    (unless (%null-ptr-p err) (error 'uhppoted-error :message (go-error err)))
    t))


(defun uhppoted-get-time-profile (uhppote device-id profile-id) "Retrieves a time profile from a controller"
  (rletz ((profile (:struct :GoTimeProfile)))
    (with-macptrs ((err (external-call "GetTimeProfile" :address uhppote 
                                                        :address profile
                                                        :unsigned-long device-id 
                                                        :unsigned-byte profile-id
                                                        :address)))
      (unless (%null-ptr-p err) (error 'uhppoted-error :message (go-error err)))
      (make-time-profile :ID            (pref profile :GoTimeProfile.ID)
                         :linked        (pref profile :GoTimeProfile.linked)
                         :from          (go-string (pref profile :GoTimeProfile.from))
                         :to            (go-string (pref profile :GoTimeProfile.to))
                         :monday        (/= 0 (pref profile :GoTimeProfile.monday))
                         :tuesday       (/= 0 (pref profile :GoTimeProfile.tuesday))
                         :wednesday     (/= 0 (pref profile :GoTimeProfile.wednesday))
                         :thursday      (/= 0 (pref profile :GoTimeProfile.thursday))
                         :friday        (/= 0 (pref profile :GoTimeProfile.friday))
                         :saturday      (/= 0 (pref profile :GoTimeProfile.saturday))
                         :sunday        (/= 0 (pref profile :GoTimeProfile.sunday))
                         :segment1start (go-string (pref profile :GoTimeProfile.segment1start))
                         :segment1end   (go-string (pref profile :GoTimeProfile.segment1end))
                         :segment2start (go-string (pref profile :GoTimeProfile.segment2start))
                         :segment2end   (go-string (pref profile :GoTimeProfile.segment2end))
                         :segment3start (go-string (pref profile :GoTimeProfile.segment3start))
                         :segment3end   (go-string (pref profile :GoTimeProfile.segment3end))))))


(defun uhppoted-set-time-profile (uhppote device-id profile) "Adds or update a time profile on a controller"
  (with-cstrs ((from          (time-profile-from          profile))
               (to            (time-profile-to            profile))
               (segment1start (time-profile-segment1start profile))
               (segment1end   (time-profile-segment1end   profile))
               (segment2start (time-profile-segment2start profile))
               (segment2end   (time-profile-segment2end   profile))
               (segment3start (time-profile-segment3start profile))
               (segment3end   (time-profile-segment3end   profile)))
  (rletz ((p (:struct :GoTimeProfile) :ID         (time-profile-id     profile)
                                       :linked    (time-profile-linked profile)
                                       :from      from
                                       :to        to
                                       :monday    (if (time-profile-monday    profile) 1 0)
                                       :tuesday   (if (time-profile-tuesday   profile) 1 0)
                                       :wednesday (if (time-profile-wednesday profile) 1 0)
                                       :thursday  (if (time-profile-thursday  profile) 1 0)
                                       :friday    (if (time-profile-friday    profile) 1 0)
                                       :saturday  (if (time-profile-saturday  profile) 1 0)
                                       :sunday    (if (time-profile-sunday    profile) 1 0)
                                       :segment1start segment1start
                                       :segment1end   segment1end
                                       :segment2start segment2start
                                       :segment2end   segment2end
                                       :segment3start segment3start
                                       :segment3end   segment3end))
    (with-macptrs ((err (external-call "SetTimeProfile" :address       uhppote 
                                                        :unsigned-long device-id 
                                                        :address       p
                                                        :address)))
      (unless (%null-ptr-p err) (error 'uhppoted-error :message (go-error err)))
      t))))


(defun uhppoted-clear-time-profiles (uhppote device-id) "Deletes all time profiles from a controller"
  (with-macptrs ((err (external-call "ClearTimeProfiles" :address uhppote 
                                                         :unsigned-long device-id 
                                                         :address)))
    (unless (%null-ptr-p err) (error 'uhppoted-error :message (go-error err)))
    t))


(defun uhppoted-add-task (uhppote device-id task) "Adds a scheduled task to a controller"
  (with-cstrs ((from (task-from task))
               (to   (task-to   task))
               (at   (task-at   task)))
    (rletz ((tt (:struct :GoTask) :task      (task-task task)
                                  :door      (task-door task)
                                  :from      from
                                  :to        to
                                  :monday    (if (task-monday    task) 1 0)
                                  :tuesday   (if (task-tuesday   task) 1 0)
                                  :wednesday (if (task-wednesday task) 1 0)
                                  :thursday  (if (task-thursday  task) 1 0)
                                  :friday    (if (task-friday    task) 1 0)
                                  :saturday  (if (task-saturday  task) 1 0)
                                  :sunday    (if (task-sunday    task) 1 0)
                                  :at at
                                  :cards     (task-cards task)))
     (with-macptrs ((err (external-call "AddTask" :address       uhppote 
                                                  :unsigned-long device-id 
                                                  :address       tt
                                                  :address)))
       (unless (%null-ptr-p err) (error 'uhppoted-error :message (go-error err)))
       t))))


(defun uhppoted-refresh-tasklist (uhppote device-id) "Refreshes a controller task list to activate added tasks"
  (with-macptrs ((err (external-call "RefreshTaskList" :address uhppote 
                                                       :unsigned-long device-id 
                                                       :address)))
    (unless (%null-ptr-p err) (error 'uhppoted-error :message (go-error err)))
    t))


(defun uhppoted-clear-tasklist (uhppote device-id) "Clears a controller task list"
  (with-macptrs ((err (external-call "ClearTaskList" :address uhppote 
                                                     :unsigned-long device-id 
                                                     :address)))
    (unless (%null-ptr-p err) (error 'uhppoted-error :message (go-error err)))
    t))

(defun uhppoted-set-pc-control (uhppote device-id enabled) "Enables/disables controller remote access control"
  (with-macptrs ((err (external-call "SetPCControl" :address uhppote 
                                                    :unsigned-long device-id 
                                                    :unsigned-byte (if enabled 1 0)
                                                    :address)))
    (unless (%null-ptr-p err) (error 'uhppoted-error :message (go-error err)))
    t))

(defun uhppoted-set-interlock (uhppote device-id interlock) "Sets a controller interlock mode"
  (with-macptrs ((err (external-call "SetInterlock" :address uhppote 
                                                    :unsigned-long device-id 
                                                    :unsigned-byte interlock
                                                    :address)))
    (unless (%null-ptr-p err) (error 'uhppoted-error :message (go-error err)))
    t))

(defun uhppoted-activate-keypads (uhppote device-id reader1 reader2 reader3 reader4) "Activates and deactivates a controller reader access keypads"
  (with-macptrs ((err (external-call "ActivateKeypads" :address uhppote 
                                                       :unsigned-long device-id 
                                                       :unsigned-byte (if reader1 1 0)
                                                       :unsigned-byte (if reader2 1 0)
                                                       :unsigned-byte (if reader3 1 0)
                                                       :unsigned-byte (if reader4 1 0)
                                                       :address)))
    (unless (%null-ptr-p err) (error 'uhppoted-error :message (go-error err)))
    t))

(defun uhppoted-set-door-passcodes (uhppote device-id door passcode1 passcode2 passcode3 passcode4) "Sets the supervisor keypad passcodes for a door"
  (with-macptrs ((err (external-call "SetDoorPasscodes" :address uhppote 
                                                        :unsigned-long device-id 
                                                        :unsigned-byte door
                                                        :unsigned-long passcode1
                                                        :unsigned-long passcode2
                                                        :unsigned-long passcode3
                                                        :unsigned-long passcode4
                                                        :address)))
    (unless (%null-ptr-p err) (error 'uhppoted-error :message (go-error err)))
    t))

(defun uhppoted-restore-default-parameters (uhppote device-id) "Resets a controller to the manufacturer default configuration"
  (with-macptrs ((err (external-call "RestoreDefaultParameters" :address uhppote 
                                                                :unsigned-long device-id 
                                                                :address)))
    (unless (%null-ptr-p err) (error 'uhppoted-error :message (go-error err)))
    t))

(defparameter *uhppoted-event-handler* nil)
(defparameter *uhppoted-error-handler* nil)
(defparameter *uhppoted-listen-stop*   (make-semaphore))

(defcallback uhppoted-on-event ((:struct :GoListenEvent) event :unsigned-doubleword userdata) "Callback function for controller events"
  (let ((controller (pref event :GoListenEvent.controller))
        (evt (make-listen-event :timestamp  (go-string (pref event :GoListenEvent.timestamp))
                                :index      (pref event :GoListenEvent.index)
                                :event-type (pref event :GoListenEvent.event-type)
                                :granted    (/= 0 (pref event :GoListenEvent.granted))
                                :door       (pref event :GoListenEvent.door)
                                :direction  (pref event :GoListenEvent.direction)
                                :card       (pref event :GoListenEvent.card)
                                :reason     (pref event :GoListenEvent.reason))))
    (when *uhppoted-event-handler*
      (funcall *uhppoted-event-handler* controller evt userdata))))

(defcallback uhppoted-on-error (:address err) "Callback function for controller event listen errors"
    (when *uhppoted-error-handler*
      (funcall *uhppoted-error-handler* (go-string err))))

; NTS resorting to a global variables to store the on-event an on-error callback functions because
;     seemingly defcallback can't be used inside a lambda (or at least not simply) and the user
;     defined condition handlers seem to be outside the process space of the DLL
(defun uhppoted-listen-events (uhppote on-event on-error userdata) "Listens for controller events"
  (setf *uhppoted-event-handler* on-event)
  (setf *uhppoted-error-handler* on-error)
  (unwind-protect
    (rlet ((listening :unsigned-byte 0)
           (stop      :unsigned-byte 0))
    (with-macptrs ((err (external-call "Listen" :address             uhppote 
                                                :address             uhppoted-on-event
                                                :address             listening
                                                :address             stop
                                                :address             uhppoted-on-error
                                                :unsigned-doubleword userdata
                                                :address)))
      (unless (%null-ptr-p err) (error 'uhppoted-error :message (format t "~a" err)))
      (progn 
        ; wait for event listener to start
        (loop repeat 5
              until (= (%get-unsigned-byte listening) 1)
          do (sleep 1))

        (unless (= (%get-unsigned-byte listening) 1) (error 'uhppoted-error :message "failed to start event listener"))

        ; wait for 'stop'
        (wait-on-semaphore *uhppoted-listen-stop*)
        (format t " ~a~%" "... stopping")
        (setf (pref stop :unsigned-byte) 1)
        
        ; wait for event listener to stop
        (loop repeat 5
              until (/= (%get-unsigned-byte listening) 1)
          do (sleep 1))
        
        (unless (= (%get-unsigned-byte listening) 0) (error 'uhppoted-error :message "failed to stop event listener"))
        t)))))


(defun uhppoted-lookup (category code locale) "Looks up the plain text description for a code"
  (cond ((string= category lookup-mode)         (uhppoted-lookup-mode         code locale))
        ((string= category lookup-direction)    (uhppoted-lookup-direction    code locale))
        ((string= category lookup-event-type)   (uhppoted-lookup-event-type   code locale))
        ((string= category lookup-event-reason) (uhppoted-lookup-event-reason code locale))
        (t "?")))

(defun uhppoted-lookup-mode (code locale) "Looks up the plain text description for a door control mode code"
  (declare (ignore locale))
  (cond ((equal code normally-open)   ModeNormallyOpen)
        ((equal code normally-closed) ModeNormallyClosed)
        ((equal code controlled)      ModeControlled)
        (t ModeUnknown)))

(defun uhppoted-lookup-direction (code locale) "Looks up the plain text description for an event direction code"
  (declare (ignore locale))
  (cond ((equal code direction-in)  DirectionIn)
        ((equal code direction-out) DirectionOut)
        (t DirectionUnknown)))

(defun uhppoted-lookup-event-type (code locale) "Looks up the plain text description for an event type code"
  (declare (ignore locale))
  (cond ((equal code event-type-none)        EventTypeNone)
        ((equal code event-type-swipe)       EventTypeSwipe)
        ((equal code event-type-door)        EventTypeDoor)
        ((equal code event-type-alarm)       EventTypeAlarm)
        ((equal code event-type-overwritten) EventTypeOverwritten)
        (t EventTypeUnknown)))

(defun uhppoted-lookup-event-reason (code locale) "Looks up the plain text description for an event reasoncode"
  (declare (ignore locale))
  (cond ((equal code event-reason-none)                            EventReasonNone)
        ((equal code event-reason-swipe)                           EventReasonSwipe)
        ((equal code event-reason-swipe-open)                      EventReasonSwipeOpen)
        ((equal code event-reason-swipe-close)                     EventReasonSwipeClose)
        ((equal code event-reason-denied)                          EventReasonDenied)
        ((equal code event-reason-no-access-rights)                EventReasonNoAccessRights)
        ((equal code event-reason-incorrect-password)              EventReasonIncorrectPassword)
        ((equal code event-reason-anti-passback)                   EventReasonAntiPassback)
        ((equal code event-reason-more-cards)                      EventReasonMoreCards)
        ((equal code event-reason-first-card-open)                 EventReasonFirstCardOpen)
        ((equal code event-reason-door-is-normally-closed)         EventReasonDoorIsNormallyClosed)
        ((equal code event-reason-interlock)                       EventReasonInterlock)
        ((equal code event-reason-not-in-allowed-time-period)      EventReasonNotInAllowedTimePeriod)
        ((equal code event-reason-invalid-timezone)                EventReasonInvalidTimezone)
        ((equal code event-reason-access-denied)                   EventReasonAccessDenied)
        ((equal code event-reason-pushbutton-ok)                   EventReasonPushButtonOk)
        ((equal code event-reason-door-opened)                     EventReasonDoorOpened)
        ((equal code event-reason-door-closed)                     EventReasonDoorClosed)
        ((equal code event-reason-door-opened-supervisor-password) EventReasonDoorOpenedSupervisorPassword)
        ((equal code event-reason-controller-power-on)             EventReasonControllerPowerOn)
        ((equal code event-reason-controller-reset)                EventReasonControllerReset)
        ((equal code event-reason-pushbutton-invalid-door-locked)  EventReasonPushbuttonInvalidDoorLocked)
        ((equal code event-reason-pushbutton-invalid-offline)      EventReasonPushbuttonInvalidOffline)
        ((equal code event-reason-pushbutton-invalid-interlock)    EventReasonPushbuttonInvalidInterlock)
        ((equal code event-reason-pushbutton-invalid-threat)       EventReasonPushbuttonInvalidThreat)
        ((equal code event-reason-door-open-too-long)              EventReasonDoorOpenTooLong)
        ((equal code event-reason-forced-open)                     EventReasonForcedOpen)
        ((equal code event-reason-fire)                            EventReasonFire)
        ((equal code event-reason-forced-closed)                   EventReasonForcedClosed)
        ((equal code event-reason-theft-prevention)                EventReasonTheftPrevention)
        ((equal code event-reason-zone-24x7)                       EventReasonZone24x7)
        ((equal code event-reason-emergency)                       EventReasonEmergency)
        ((equal code event-reason-remote-open-door)                EventReasonRemoteOpenDoor)
        ((equal code event-reason-remote-open-door-usb-reader)     EventReasonRemoteOpenDoorUSBReader)
        (t EventReasonUnknown)))
