@echo off
echo.
echo Starting SBT
echo.


set WORKSPACE=%LOCALAPPDATA%
set JAVA_OPTS=-Dsbt.global.base="%WORKSPACE%/.sbt" -Dsbt.boot.directory="%WORKSPACE%/.sbt/boot" -Dsbt.ivy.home="%WORKSPACE%/.ivy2" -Dhttp.proxyHost=surf-proxy.intranet.db.com -Dhttp.proxyPort=8080 -Dhttp.nonProxyHosts="*.db.com|localhost|127.0.0.1" -Xmx1024M -XX:MaxPermSize=512m -XX:ReservedCodeCacheSize=128m -Dsbt.log.noformat=true %DEBUG_OPTS%
set CLASSPATH=-cp "sbt-launch-0.12.2.jar;jansi-1.11.jar"


set JAVA_OPTS=-Dhttp.proxyHost=surf-proxy.intranet.db.com -Dhttp.proxyPort=8080 -Dhttp.nonProxyHosts="*.db.com|localhost|127.0.0.1"
java %JAVA_OPTS% %CLASSPATH% xsbt.boot.Boot %1 %2 %3 %4 %5 %6 %7 %8 %9