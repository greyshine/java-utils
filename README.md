[![Build status](https://travis-ci.org/greyshine/java-utils.svg?branch=master)](https://travis-ci.org/greyshine/java-utils)
[![Coverage Status](https://coveralls.io/repos/github/greyshine/java-utils/badge.svg?branch=master)](https://coveralls.io/github/greyshine/java-utils?branch=master)

# java-utils

My Java utilities collection.

The package __de.greyshine.utils.deprecated__ comes from various other sources and is located there in order to be merged into the final package __de.greyshine.utils.deprecated__. It is easier to refactor, re-check and copy the needed utils time after time.

## String functions
The terms blank and empty and null are treated differently.
- **null** means nothing, not existing. An empty String or a String filled with whitespace characters is not _null_!
- **empty**  means that the existing String has a length of 0; character count is  zero. Any whitespace is a character! 
- **blank** means either _null_ or any amount fro zero to n of whitespace characters; technically it is null-checked or trimmed and checked by the length equals 0.  

## Javax stuff - build errors?

For email functionality we need to have some extra libraries which cannot be automatically fetched by default repositories by maven. 
Some where manually downloaded from
[http://download.oracle.com/otndocs/jcp/7542-jms-1.1-fr-doc-oth-JSpec/](http://download.oracle.com/otndocs/jcp/7542-jms-1.1-fr-doc-oth-JSpec/)  
[http://www.oracle.com/technetwork/java/javasebusiness/downloads/java-archive-downloads-java-plat-419418.html](http://www.oracle.com/technetwork/java/javasebusiness/downloads/java-archive-downloads-java-plat-419418.html)

In order to install themon unix/mac into your repository run:  
``src/scripts/mvn-install-missing-javax-stuff.sh``  
For windows a script will come when I need it or you provide one to me :-)

## External Packages

External packages / artefacts / ... are used.
Licensing of their software also applies to this software as they declared it in their licenses.

* This package reuses showdownjs: [https://github.com/showdownjs/showdown](https://github.com/showdownjs/showdown)  
* Google Gson
* Flying-Saucer-Pdf
* Apache Commons
* javax.jms
* jmxri (located at libs folder)
* jmxtools (located at libs folder)
* javax.jms (located at libs folder)
* ...




