# VB.NET

VB.NET GUI example application, originally authored by @threeeye.

## Screenshots

<img width="640" src="/doc/VB.NET/images/main.png">
<img width="640" src="/doc/VB.NET/images/get-devices.png">
<img width="640" src="/doc/VB.NET/images/card-management.png">
<img width="640" src="/doc/VB.NET/images/put-card.png">
<img width="640" src="/doc/VB.NET/images/events-start-listen.png">
<img width="640" src="/doc/VB.NET/images/events-stop-listen.png">
<img width="640" src="/doc/VB.NET/images/events-card-swiped.png">

## Notes

1. This application **must** be built against the _windows-2019-ltsc_ branch until all the fixes required to run
   under the more restrictive environment of LTSC have been migrated to the _main_ branch.
2. The application has only been tested on Windows 2019 LTSC.
3. _uhppoted.dll_ must be in the application startup path (e.g. _\bin\Debug\net8.0-windows_)
4. A prebuilt DLL for Windows 2019 LTSC can be downloaded from [\doc\VB.NET\dll\uhppoted-dll-windows-2019-LTSC.zip]