# ad_repl

A simple REPL that syncs a shell session with an ad buffer.

  - "Execute" is defined to be "send as input to the shell".
  - Hitting return at the end of the buffer will send that line to the shell.
  - Running "clear" will clear the ad buffer
  - Running "exit" will close the shell subprocess as well as the ad buffer

