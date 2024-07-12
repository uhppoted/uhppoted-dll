using System;
using System.Runtime.InteropServices;
using System.Collections.Generic;
using System.Text;

public class DoorMode {
{{- range .lookup.doors.modes}}
    public const byte {{CamelCase .mode}} = {{.code}};
{{- end}}
}

public class Direction {
{{- range .lookup.events.directions}}
    public const byte {{CamelCase .direction}} = {{.code}};
{{- end}}
}

public class EventType {
{{- range .lookup.events.types}}
    public const byte {{CamelCase .type}} = {{.code}};
{{- end}}
}

public class EventReason {
{{- range .lookup.events.reasons}}
    public const byte {{CamelCase .reason}} = {{.code}};
{{- end}}
}
