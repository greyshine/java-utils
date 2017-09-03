#!/bin/sh

echo installing some libs from
echo http://www.oracle.com/technetwork/java/javasebusiness/downloads/java-archive-downloads-java-plat-419418.html
echo and http://download.oracle.com/otndocs/jcp/7542-jms-1.1-fr-doc-oth-JSpec/
echo into local maven repo

libsDir=$(dirname "$0")/../../libs

echo maven version:
mvn -version

# Missing artifact javax.jms:jms:jar:1.1
mvn install:install-file -Dfile=$libsDir/javax.jms-1.1.jar -DgroupId=javax.jms -DartifactId=jms -Dversion=1.1 -Dpackaging=jar

# Missing artifact com.sun.jmx:jmxri:jar:1.2.1
mvn install:install-file -Dfile=$libsDir/jmxri-1.2.1.jar -DgroupId=com.sun.jmx -DartifactId=jmxri -Dversion=1.2.1 -Dpackaging=jar

# com.sun.jdmk:jmxtools:jar:1.2.1
mvn install:install-file -Dfile=$libsDir/jmxtools-1.2.1.jar -DgroupId=com.sun.jdmk -DartifactId=jmxtools -Dversion=1.2.1 -Dpackaging=jar