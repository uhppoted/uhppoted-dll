from typing import Final

#pragma once
{{range .lookup.doors.modes}}
{{constant .mode}}: Final[int] = {{.code}}
{{- end}}
{{range .lookup.events.directions}}
DIRECTION_{{constant .direction}}: Final[int] = {{.code}}
{{- end}}
{{range .lookup.events.types}}
EVENT_TYPE_{{constant .type}}: Final[int] = {{.code}}
{{- end}}
{{range .lookup.events.reasons}}
EVENT_REASON_{{constant .reason}}: Final[int] = {{.code}}
{{- end}}
