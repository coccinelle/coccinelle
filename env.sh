# I put both stuff useful for the user and developer in this file. Could
# separate and have a env-user.sh, env-compile.sh, env-developer.sh, 
# but it's not worth it.

#!!!!You need to source me with "source env.sh" from the good directory!!!!
if [ ! -r standard.iso ]
    then echo "There is no standard.iso here. 
Are you sure you run this script from the coccinelle directory ?
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
echo setting COCCINELLE_HOME
export COCCINELLE_HOME=`pwd`

# To find pycaml dynamic library
echo setting LD_LIBRARY_PATH
export LD_LIBRARY_PATH=$COCCINELLE_HOME:$LD_LIBRARY_PATH

# To find .py files like the one in python/coccib
echo setting PYTHONPATH
export PYTHONPATH=$COCCINELLE_HOME/python:$PYTHONPATH

fi