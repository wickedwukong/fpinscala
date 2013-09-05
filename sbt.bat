@echo off
set SCRIPT_DIR=%~dp0

set JAVA_OPTS=-Dhttp.proxyHost=surf-proxy.intranet.db.com -Dhttp.proxyPort=8080 -Dhttp.nonProxyHosts="*.db.com|localhost|127.0.0.1"
java %JAVA_OPTS% %SBT_OPTS% -Xmx1024m -Xss4M -XX:MaxPermSize=512m -jar "%SCRIPT_DIR%sbt-launch.jar" %*
