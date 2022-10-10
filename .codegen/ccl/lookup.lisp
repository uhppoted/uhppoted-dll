(in-package uhppoted)
{{range .lookup.doors.modes}}
(defconstant {{uppercase .mode | hyphenate | pad 15}} {{.code}})
{{- end}}
{{range .lookup.events.directions}}
(defconstant DIRECTION-{{uppercase .direction | hyphenate | pad 3}} {{.code}})
{{- end}}
{{range .lookup.events.types}}
(defconstant EVENT-TYPE-{{uppercase .type | hyphenate | pad 11}} {{.code}})
{{- end}}
{{range .lookup.events.reasons}}
(defconstant EVENT-REASON-{{uppercase .reason | hyphenate | pad 31}} {{.code}})
{{- end}}

