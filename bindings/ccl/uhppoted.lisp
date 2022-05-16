(in-package uhppoted)

#+linux-target
(progn
  (format t ">>> TARGET-LINUX~%")
  (format t ">>> >> LD_LIBRARY_PATH ~a~%" (getenv "LD_LIBRARY_PATH"))
  (format t ">>> >> path ~a~%" (make-pathname :directory (getenv "LD_LIBRARY_PATH") :name "libuhppoted" :type "so"))
  (format t ">>> >> native-path ~a~%" (native-translated-namestring 
    (make-pathname :directory (getenv "LD_LIBRARY_PATH") :name "libuhppoted" :type "so")))
  (format t ">>> >> ls(1): ~a~%" (directory "*.*" :directories t))
  (format t ">>> >> ls(2): ~a~%" (directory "../../lib/tests/*.*" :directories t))
  (format t ">>> >> open LIB ~a~%" 
    (open-shared-library 
      (native-translated-namestring 
        (make-pathname :directory (getenv "LD_LIBRARY_PATH") :name "libuhppoted" :type "so")))))


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


(defconstant NORMALLY-OPEN   1)
(defconstant NORMALLY-CLOSED 2)
(defconstant CONTROLLED      3)

(defconstant DIRECTION-IN  1)
(defconstant DIRECTION-OUT 2)

(defconstant EVENT-TYPE-NONE        0)
(defconstant EVENT-TYPE-SWIPE       1)
(defconstant EVENT-TYPE-DOOR        2)
(defconstant EVENT-TYPE-ALARM       3)
(defconstant EVENT-TYPE-OVERWRITTEN 255)

(defconstant EVENT-REASON-SWIPE                           1)
(defconstant EVENT-REASON-DENIED                          5)
(defconstant EVENT-REASON-NO-ACCESS-RIGHTS                6)
(defconstant EVENT-REASON-INCORRECT-PASSWORD              7)
(defconstant EVENT-REASON-ANTI-PASSBACK                   8)
(defconstant EVENT-REASON-MORE-CARDS                      9)
(defconstant EVENT-REASON-FIRST-CARD-OPEN                 10)
(defconstant EVENT-REASON-DOOR-IS-NORMALLY-CLOSED         11)
(defconstant EVENT-REASON-INTERLOCK                       12)
(defconstant EVENT-REASON-NOT-IN-ALLOWED-TIME-PERIOD      13)
(defconstant EVENT-REASON-INVALID-TIMEZONE                15)
(defconstant EVENT-REASON-ACCESS-DENIED                   18)
(defconstant EVENT-REASON-PUSH-BUTTON-OK                  20)
(defconstant EVENT-REASON-DOOR-OPENED                     23)
(defconstant EVENT-REASON-DOOR-CLOSED                     24)
(defconstant EVENT-REASON-DOOR-OPENED-SUPERVISOR-PASSWORD 25)
(defconstant EVENT-REASON-CONTROLLER-POWER-ON             28)
(defconstant EVENT-REASON-CONTROLLER-RESET                29)
(defconstant EVENT-REASON-PUSHBUTTON-INVALID-DOOR-LOCKED  31)
(defconstant EVENT-REASON-PUSHBUTTON-INVALID-OFFLINE      32)
(defconstant EVENT-REASON-PUSHBUTTON-INVALID-INTERLOCK    33)
(defconstant EVENT-REASON-PUSHBUTTON-INVALID-THREAT       34)
(defconstant EVENT-REASON-DOOR-OPEN-TOO-LONG              37)
(defconstant EVENT-REASON-FORCED-OPEN                     38)
(defconstant EVENT-REASON-FIRE                            39)
(defconstant EVENT-REASON-FORCED-CLOSED                   40)
(defconstant EVENT-REASON-THEFT-PREVENTION                41)
(defconstant EVENT-REASON-24X7-ZONE                       42)
(defconstant EVENT-REASON-EMERGENCY                       43)
(defconstant EVENT-REASON-REMOTE-OPEN-DOOR                44)
(defconstant EVENT-REASON-REMOTE-OPEN-DOOR-USB-READER     45)


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
                doors)

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


(def-foreign-type nil
  (:struct :UDEVICE (:id      :int)
	                  (:address :address)))

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
                   (:doors     :address)))

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


(define-condition uhppoted-error (error)
  ((message :initarg :message :reader message)))

(defun go-error (cstr) "Converts a 'C' char * returned by the Go FFI to a string and frees the 'C' string"
  (with-macptrs ((p cstr))
    (%get-cstring p)))

(defun go-string (cstr) "Converts a 'C' char * returned by the Go FFI to a string and frees the 'C' string"
  (with-macptrs ((p cstr))
    (%get-cstring p)))

(defun go-sizeof (type) "" 
  (cond ((eq type :udevice) 16)
	      (T 0)))

(defun uhppoted (f &key (bind-addr "") (broadcast-addr "") (listen-addr "") (timeout 5) (controllers NIL) (debug NIL)) ""
  (%stack-block ((devices (* (length controllers) (go-sizeof :udevice)   )))
    (rletz ((udevices (:struct UDEVICES) :N (length controllers) 
                                         :devices devices)
			      (uhppote (:struct :UHPPOTE) :bind      (ccl::make-cstring bind-addr)
                                        :broadcast (ccl::make-cstring broadcast-addr)
                                        :listen    (ccl::make-cstring listen-addr)
                                        :timeout   timeout
                                        :devices   udevices
                                        :debug     (cond (debug 1) (T 0))))
      (loop for (id addr) in controllers
        do (progn
		         (setf (pref devices :udevice.id) id)
			       (setf (pref devices :udevice.address) (ccl::make-cstring addr))
			       (%setf-macptr devices (%inc-ptr devices 16))))

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
			       (%setf-macptr p (%inc-ptr p 16))))))))))


(defun uhppoted-get-devices (uhppote &optional (N 16)) "Retrieves a list of device IDs on the local LAN"
  (destructuring-bind  (p q) (uhppoted-get-devices-n uhppote N)
	  (cond ((>= N p) (subseq q 0 p))
		  (T (uhppoted-get-devices uhppote (+ N 16))))))

(defun uhppoted-get-devices-n (uhppote max) ""
  (multiple-value-bind (array parray) (make-heap-ivector max '(unsigned-byte 32))
	  (unwind-protect
	    (rletz ((N :signed-long max))
        (with-macptrs ((err (external-call "GetDevices" :address uhppote 
										                                    :address N 
                                                        :address parray 
                                                        :address)))
           (unless (%null-ptr-p err) (error 'uhppoted-error :message (go-error err)))
		       (list (%get-signed-long N) array)))
	    (dispose-heap-ivector array))))


(defun uhppoted-get-device (uhppote device-id) "Retrieves the device information for a controller"
  (rletz ((device (:struct :GoDevice)))
    (with-macptrs ((err (external-call "GetDevice" :address uhppote 
								                                   :address device 
									                                 :unsigned-long device-id 
                                                   :address)))
      (unless (%null-ptr-p err) (error 'uhppoted-error :message (go-error err)))
	    (make-device :id      (%get-unsigned-long device)
                   :address (go-string (pref device :GoDevice.address))
			  	         :subnet  (go-string (pref device :GoDevice.subnet))
                   :gateway (go-string (pref device :GoDevice.gateway))
                   :MAC     (go-string (pref device :GoDevice.MAC))
                   :version (go-string (pref device :GoDevice.version))
                   :date    (go-string (pref device :GoDevice.date))))))


(defun uhppoted-set-address (uhppote device-id ip-addr subnet-mask gateway-addr) "Sets the controller IP address, subnet mask and gateway"
  (with-cstrs ((address ip-addr)
			   (subnet  subnet-mask)
			   (gateway gateway-addr))
    (with-macptrs ((err (external-call "SetAddress" :address uhppote 
				                                					  :unsigned-long device-id 
									                                  :address address  
                                                    :address subnet
                                                    :address gateway
                                                    :address)))
      (unless (%null-ptr-p err) (error 'uhppoted-error :message (go-error err)))
      t)))


(defun uhppoted-get-status (uhppote device-id) "Retrieves a controller status information"
  (%stack-block ((doors   4)
				         (buttons 4))
    (rletz ((event  (:struct :GoEvent))   
            (status (:struct :GoStatus) :doors   doors
					                              :buttons buttons
					                              :event   event))
      (with-macptrs ((err (external-call "GetStatus" :address uhppote 
										                                 :address status
										                                 :unsigned-long device-id 
										                                 :address)))
        (unless (%null-ptr-p err) (error 'uhppoted-error :message (go-error err)))
        (make-status :id        (%get-unsigned-long status)
					           :timestamp (go-string (pref status :GoStatus.timestamp))
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
					           :event      (make-event :timestamp (go-string (pref event :GoEvent.timestamp))
										 :index      (pref event :GoEvent.index)
										 :event-type (pref event :GoEvent.event-type)
										 :granted    (pref event :GoEvent.granted)
										 :door       (pref event :GoEvent.door)
									   :direction  (pref event :GoEvent.direction)
										 :card       (pref event :GoEvent.card)
										 :reason     (pref event :GoEvent.reason)))))))


(defun uhppoted-get-time (uhppote device-id) "Retrieves a controller date/time"
  (with-cstrs ((datetime ""))
     (with-macptrs ((err (external-call "GetTime" :address uhppote 
                                                  :address datetime
                                                  :unsigned-long device-id 
                                                  :address)))
       (unless (%null-ptr-p err) (error 'uhppoted-error :message (go-error err)))
       (go-string (%get-ptr datetime)))))


(defun uhppoted-set-time (uhppote device-id datetime) "Sets a controller date/time"
  (with-cstrs ((dt datetime))
    (with-macptrs ((err (external-call "SetTime" :address uhppote 
                                                 :unsigned-long device-id 
                                                 :address dt
                                                 :address)))
      (unless (%null-ptr-p err) (error 'uhppoted-error :message (go-error err)))
      t)))


(defun uhppoted-get-listener (uhppote device-id) "Retrieves the controller event listener address"
  (with-cstrs ((listener ""))
     (with-macptrs ((err (external-call "GetListener" :address uhppote 
                                                      :address listener
                                                      :unsigned-long device-id 
                                                      :address)))
       (unless (%null-ptr-p err) (error 'uhppoted-error :message (go-error err)))
       (go-string (%get-ptr listener)))))


(defun uhppoted-set-listener (uhppote device-id listener) "Sets a controller's event listener address and port"
  (with-cstrs ((addr listener))
    (with-macptrs ((err (external-call "SetListener" :address uhppote 
                                                     :unsigned-long device-id 
                                                     :address addr
                                                     :address)))
      (unless (%null-ptr-p err) (error 'uhppoted-error :message (go-error err)))
      t)))


(defun uhppoted-get-door-control (uhppote device-id door) "Retrieves the door control state and open delay for a controller"
  (rletz ((control (:struct :GoDoorControl)))
    (with-macptrs ((err (external-call "GetDoorControl" :address uhppote 
                                                        :address control 
                                                        :unsigned-long device-id 
                                                        :unsigned-byte door
                                                        :address)))
      (unless (%null-ptr-p err) (error 'uhppoted-error :message (go-error err)))
      (make-door-control :mode  (pref control :GoDoorControl.mode)
                         :delay (pref control :GoDoorControl.delay)))))


(defun uhppoted-set-door-control (uhppote device-id door mode delay) "Sets the control mode and delay for a controller door"
    (with-macptrs ((err (external-call "SetDoorControl" :address uhppote 
                                                        :unsigned-long device-id 
                                                        :unsigned-byte door
                                                        :unsigned-byte mode
                                                        :unsigned-byte delay
                                                        :address)))
      (unless (%null-ptr-p err) (error 'uhppoted-error :message (go-error err)))
      t))


(defun uhppoted-open-door (uhppote device-id door) "Remotely opens a controller door"
    (with-macptrs ((err (external-call "OpenDoor" :address uhppote 
                                                  :unsigned-long device-id 
                                                  :unsigned-byte door
                                                  :address)))
      (unless (%null-ptr-p err) (error 'uhppoted-error :message (go-error err)))
      t))


(defun uhppoted-get-cards (uhppote device-id) "Retrieves the number of cards stored on a controller"
  (rletz ((N :signed-long 0))
         (with-macptrs ((err (external-call "GetCards" :address uhppote 
                                                       :address N 
                                                       :unsigned-long device-id 
                                                       :address)))
          (unless (%null-ptr-p err) (error 'uhppoted-error :message (go-error err)))
          (%get-signed-long N))))


(defun uhppoted-get-card (uhppote device-id card-number) "Retrieves card detail for a card stored on controller"
  (%stack-block ((doors   4))
    (rletz ((card (:struct :GoCard) :doors   doors))
      (with-macptrs ((err (external-call "GetCard" :address uhppote 
                                                   :address card
                                                   :unsigned-long device-id 
                                                   :unsigned-long card-number
                                                   :address)))
        (unless (%null-ptr-p err) (error 'uhppoted-error :message (go-error err)))
        (make-card :card-number (%get-unsigned-long (pref card :GoCard.card-number))
                   :from        (go-string (pref card :GoCard.from))
                   :to          (go-string (pref card :GoCard.to))
                   :doors       (list (%get-unsigned-byte doors 0)
                                      (%get-unsigned-byte doors 1)
                                      (%get-unsigned-byte doors 2)
                                      (%get-unsigned-byte doors 3)))))))


(defun uhppoted-get-card-by-index (uhppote device-id index) "Retrieves card detail for the card stored at an index on controller"
  (%stack-block ((doors   4))
    (rletz ((card (:struct :GoCard) :doors   doors))
      (with-macptrs ((err (external-call "GetCardByIndex" :address uhppote 
                                                   :address card
                                                   :unsigned-long device-id 
                                                   :unsigned-long index
                                                   :address)))
        (unless (%null-ptr-p err) (error 'uhppoted-error :message (go-error err)))
        (make-card :card-number (%get-unsigned-long (pref card :GoCard.card-number))
                   :from        (go-string (pref card :GoCard.from))
                   :to          (go-string (pref card :GoCard.to))
                   :doors       (list (%get-unsigned-byte doors 0)
                                      (%get-unsigned-byte doors 1)
                                      (%get-unsigned-byte doors 2)
                                      (%get-unsigned-byte doors 3)))))))


(defun uhppoted-put-card (uhppote device-id card-number from to doors) "Adds or updates the card detail stored on a controller"
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


