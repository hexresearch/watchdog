# watchdog

Utility for PC auto-restart. The app listens for signals
on given TCP port and if nothing comes for specified amount of time
it executes reboot-command.

The invocation:

~~~
watchdog --conf config.yaml
~~~

Example of the typical config:

~~~yaml
server:
  host: "localhost"
  port: 8080
duration: 10 # seconds
rebootCmd: "echo REBOOT ME"
lastRebootTimeFilePath: "reboot-log.txt"  # optional
debugMode: true                           # optional (false by default)
~~~

The app listens for packets on TCP-socked specified in field `server`.
For `duration` seconds it waits for the packets. If nothing happens
it executes `rebootCmd`.

Optionally we can specify file `lastRebootTimeFilePath` to log
the last rebootCmd invocation time. If `debugMode` is set then
the command is not performed immediately but delayed by 10 seconds so that
user has choice to abort the reboot with Ctrl+C.