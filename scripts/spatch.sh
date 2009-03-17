#!/bin/bash

echo setting COCCINELLE_HOME=${COCCINELLE_HOME:=SHAREDIR}

if [ ! -r $COCCINELLE_HOME/standard.iso ] ; then
    echo "There is no standard.iso in SHAREDIR."
    echo "Are you sure you run a properly installed version of spatch ?\n"
else

 export COCCINELLE_HOME
 export LD_LIBRARY_PATH=$COCCINELLE_HOME:$LD_LIBRARY_PATH
 export PYTHONPATH=$COCCINELLE_HOME/python:$PYTHONPATH
 
 echo setting LD_LIBRARY_PATH=$LD_LIBRARY_PATH
 echo setting PYTHONPATH=$PYTHONPATH

fi

$COCCINELLE_HOME/spatch $*

