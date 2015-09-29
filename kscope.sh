#!/bin/bash

SSHCONNECT="lib/SSHconnect.jar"

if [ -f $SSHCONNECT ]; then
  echo "set environment variable: SSHCONNECT"
  export CLASSPATH=$SSHCONNECT:$CLASSPATH
fi

java -Xmx1024m -jar kscope.jar
