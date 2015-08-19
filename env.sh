# I put both stuff useful for the user and developer in this file. Could
# separate and have a env-user.sh, env-compile.sh, env-developer.sh,
# but it's not worth it.

#!!!!You need to source me with "source env.sh" from the good directory!!!!

# 14 Aug 2009 Try directory we were executed from

if [ "$1" ] ; then
	DIR=$1
else
	DIR=`pwd`
fi

if [ ! -r $DIR/standard.iso ]
then
        echo "standard.iso not found in '$DIR' trying `dirname $0`"
	DIR=`dirname $0`
fi

if [ ! -r $DIR/standard.iso ]
    then echo "standard.iso not found in '$DIR'.
  Give the coccinelle directory as the first argument.
";
else


##############################################################################
# Compile
##############################################################################

##############################################################################
# Run
##############################################################################

# To find the data/ files such as the default standard.h file.
# Cf also globals/config.ml
echo setting COCCINELLE_HOME=$DIR
COCCINELLE_HOME=$DIR				; export COCCINELLE_HOME

# To find pycaml dynamic library
echo adding $COCCINELLE_HOME to LD_LIBRARY_PATH
LD_LIBRARY_PATH=$COCCINELLE_HOME:$LD_LIBRARY_PATH ; export LD_LIBRARY_PATH

# To find .py files like the one in python/coccib
echo adding $COCCINELLE_HOME/python to PYTHONPATH
PYTHONPATH=$COCCINELLE_HOME/python:$PYTHONPATH	; export PYTHONPATH

fi

export PATH=$COCCINELLE_HOME:$PATH
