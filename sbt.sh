#!/usr/bin/env sh


JAVA_OPTS="-Xmx1024M -XX:MaxPermSize=512m -XX:ReservedCodeCacheSize=128m"

exec java $JAVA_OPTS -jar sbt-launch-0.13.0.jar "$@"
