#! /bin/bash
#
# run test for error exit
#
echo "Running error test:"
if [ -x ./error.x ] ; then 
    ./error.x 
else
    echo "FAIL: no executable found"
fi