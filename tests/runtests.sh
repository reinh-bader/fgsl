#! /bin/bash
# run complete set of unit tests


echo "+-----------------------------------+"
echo "| Starting execution of FGSL tests  |"
echo "+-----------------------------------+"
./version.exe
for file in *.exe; do
    if [ $file != "version.exe" ] ; then
	echo "Running $file: "
	./$file
    fi
done

echo "Running error test:"
./error.sh > ERROR.dat 2>&1
grep "OK" ERROR.dat 2> /dev/null
grep "FAIL" ERROR.dat 2> /dev/null
echo "+-----------------------------------+"
echo "| Completed execution of FGSL tests |"
echo "+-----------------------------------+"
exit 0
