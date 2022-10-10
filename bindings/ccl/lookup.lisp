(in-package uhppoted)

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

(defconstant EVENT-REASON-NONE                            0)
(defconstant EVENT-REASON-SWIPE                           1)
(defconstant EVENT-REASON-SWIPE-OPEN                      2)
(defconstant EVENT-REASON-SWIPE-CLOSE                     3)
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
(defconstant EVENT-REASON-PUSHBUTTON-OK                   20)
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
(defconstant EVENT-REASON-ZONE-24X7                       42)
(defconstant EVENT-REASON-EMERGENCY                       43)
(defconstant EVENT-REASON-REMOTE-OPEN-DOOR                44)
(defconstant EVENT-REASON-REMOTE-OPEN-DOOR-USB-READER     45)

