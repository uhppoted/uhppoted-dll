#pragma once
{{range .lookup.doors.modes}}
const uint8_t {{constant .mode}} = {{.code}};
{{- end}}
{{range .lookup.events.directions}}
const uint8_t DIRECTION_{{constant .direction}} = {{.code}};
{{- end}}
{{range .lookup.events.types}}
const uint8_t EVENT_TYPE_{{constant .type}} = {{.code}};
{{- end}}
{{range .lookup.events.reasons}}
const uint8_t EVENT_REASON_{{constant .reason}} = {{.code}};
{{- end}}
