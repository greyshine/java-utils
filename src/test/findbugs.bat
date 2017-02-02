@echo off

mvn -f ..\pom.xml site
start ..\target\index.html
REM mvn -f ..\pom.xml findbugs:gui