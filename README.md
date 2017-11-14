# watchdog

Utility for PC auto-restart. The app listens for UNIX signals or messages
on given TCP port and if nothing comes for specified amount of time
it executes reboot-command.

The invocation:

~~~
watchdog --conf config.yaml
~~~

Example of the typical config:

~~~yaml
server:                                   # optional
  host: "localhost"
  port: 8080
signal: SIGUSR1                           # optional
duration: 10 # seconds
rebootCmd: "echo REBOOT ME"
lastRebootTimeFilePath: "reboot-log.txt"  # optional
debugMode: true                           # optional (false by default)
~~~

The app listens for packets on TCP-socked specified in field `server`.
For `duration` seconds it waits for the packets. If nothing happens
it executes `rebootCmd`. Also app can wait for unix signal specified in
the field signal. Note that both fields `server` and `signal` are optional.
If we need signals we can activate only signal section.
For TCP-only usage just omit the `signal` field.

Optionally we can specify file `lastRebootTimeFilePath` to log
the last rebootCmd invocation time. If `debugMode` is set then
the command is not performed immediately but delayed by 10 seconds so that
user has choice to abort the reboot with Ctrl+C.

### Test scenario

Start first terminal with example config:

~~~
watchdog --conf config,yaml
~~~

**For TCP**:

It will not reboot while we invoke in the second terminal:

~~~
echo 1 | netcat localhost 8080
~~~


**For UNIX signals**:

It will not reboot while we invoke in the second terminal:

~~~
pkill --signal USR1 watchdog
~~~


### Example for systemd service script

Supply needed path to app and config file and put this
file to `/etc/systemd/system/` to define systemd service.

~~~
# location: /etc/systemd/system/
[Unit]
Description=Auto-restart utility
After=multi-user.target

[Service]
Type=notify
ExecStart=/path/to/watchdog --conf /path/to/config.yaml &

[Install]
WantedBy=multi-user.target
~~~