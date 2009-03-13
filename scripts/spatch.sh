#!/bin/bash

if [ ! -r SHAREDIR/standard.iso ] ; then
    echo "There is no standard.iso in SHAREDIR."
    echo "Are you sure you run a properly installed version of spatch ?\n"
else

 export COCCINELLE_HOME=SHAREDIR
 echo setting COCCINELLE_HOME=$COCCINELLE_HOME
 export LD_LIBRARY_PATH=$COCCINELLE_HOME:$LD_LIBRARY_PATH
 echo setting LD_LIBRARY_PATH=$LD_LIBRARY_PATH
 export PYTHONPATH=$COCCINELLE_HOME/python:$PYTHONPATH
 echo setting PYTHONPATH=$PYTHONPATH

fi

SHAREDIR/spatch $*

