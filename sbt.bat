@echo off
set DEBUG_OPTS=

rem ENABLE IF YOU WANT TO DEBUG:
rem set DEBUG_OPTS=-Xdebug -Xrunjdwp:transport=dt_socket,server=y,suspend=n,address=1044

set WORKSPACE=%LOCALAPPDATA%
set JAVA_OPTS=-Dbuild.number=%BUILD_NUMBER% -Dsbt.global.base="%WORKSPACE%/.sbt" -Dsbt.boot.directory="%WORKSPACE%/.sbt/boot" -Dsbt.ivy.home="%WORKSPACE%/.ivy2" -Dhttp.proxyHost=surf-proxy.intranet.db.com -Dhttp.proxyPort=8080 -Dhttp.nonProxyHosts="*.db.com|localhost|127.0.0.1" -Xmx1024M -XX:MaxPermSize=512m -XX:ReservedCodeCacheSize=128m %DEBUG_OPTS%
set CLASSPATH=-cp sbt-launch-0.13.0.jar

java %JAVA_OPTS% %CLASSPATH% xsbt.boot.Boot %1 %2 %3 %4 %5 %6 %7 %8 %9