#pragma once
{{range .lookup.doors.modes}}
#define {{constant .mode}} {{.code}}
{{- end}}
{{range .lookup.events.directions}}
#define DIRECTION_{{constant .direction}} {{.code}}
{{- end}}
{{range .lookup.events.types}}
#define EVENT_TYPE_{{constant .type}} {{.code}}
{{- end}}
{{range .lookup.events.reasons}}
#define EVENT_REASON_{{constant .reason}} {{.code}}
{{- end}}
