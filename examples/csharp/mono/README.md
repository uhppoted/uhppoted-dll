# Mono

## Notes

1. The `mono` bindings event listener implementation hangs on close for no discernible reason - it may be something
   in the mono runtime (the .NET runtimes do not exhibit the same problem). The _hangs on close_ only manifests after
   the callback has been invoked from the DLL internal event listener.
   - Ref. https://stackoverflow.com/questions/11006506/console-mono-executables-do-not-exit-when-completed
