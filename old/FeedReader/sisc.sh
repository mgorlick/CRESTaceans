#!/bin/sh

RLWRAP_FILE=~/.sisc-rlwrap

checkIfNonInteractive() {
# returns true if parameter for "non-interactive" was passed to the script
    case "-x" in
    "$@")
        return 1;
    esac
    case "--no-repl" in
    "$@")
        return 1;
    esac
    return 0;
};


if [ ! -d "$SISC_HOME" ]
then
   SISC_HOME="`dirname $0`"
   if [ ! -f "$SISC_HOME/sisc.jar" ]
   then 
     if [ -f /usr/lib/sisc/sisc.jar ] 
     then 
       SISC_HOME="/usr/lib/sisc" 
     elif [ -f /usr/local/lib/sisc/sisc.jar ]
     then
       SISC_HOME="/usr/local/lib/sisc"
     fi
   fi
fi

if [ -r "$HOME/sisc.properties" ]
then
  PROPERTIES="-p $HOME/sisc.properties"
elif [ -r "$SISC_HOME/sisc.properties" ]
then
  PROPERTIES="-p $SISC_HOME/sisc.properties"
fi

# We'll attempt to use the rlwrap unix utility to get readline support
# for the REPL.  However, due to some weirdness in the way threading
# works in the JVM, interrupts using CTL-C will cause rlwrap to exit
# prematurely.  To remedy this, we call rlwrap recursively on this shell
# script, so that the script must terminate to exit through to rlwrap.

#If the rlwrap readline wrapper is in the path, enable it for SISC, 
#unless the -x or --no-repl switches were used
checkIfNonInteractive $@;
if [ "$?" -eq "0" ] && [ -z "$RLWRAPPED" ] && [ -x "`which rlwrap`" ]
then
  if [ ! -f $RLWRAP_FILE ]
  then
    touch $RLWRAP_FILE 
  fi
  RLWRAP="rlwrap -b '(){}[],+=&^%$#@\;|' -f $RLWRAP_FILE -l $RLWRAP_FILE "
  RLWRAPPED=yes rlwrap $0 $@
else
    EXTENSIONS=""
    if [ -z "$JAVA" ] || ! which $JAVA 
    then
      JAVA=java
    fi

        
    D=":"
    case `uname -s` in 
      CYGWIN* )
        D=";" ;;
    esac
      
    # Add in our extensions to Java
    for i in $SISC_HOME/serf/java $SISC_HOME/serf/lib/*.jar; do
      CLASSPATH=$CLASSPATH:$i
    done

    $JAVA $JAVAOPT -classpath $SISC_HOME/sisc-opt.jar${D}$SISC_HOME/sisc.jar${D}$SISC_HOME/sisc-lib.jar${D}$CLASSPATH -Dsisc.home=$SISC_HOME sisc.REPL -h $SISC_HOME/sisc.shp $PROPERTIES $EXTENSIONS "$@"
fi

