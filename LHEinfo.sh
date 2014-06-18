######
# Script to produce the total number of events from an LHE
# Author: Dominic Smith, University of Bristol/ VUB
# Date: June 2014
# Comments: Run with ./LHEinfo.sh <filename>.lhe
#####

echo "                   " 
file=$1
echo "Analysing $file"
echo "     "

head -1 $file
echo "      "
echo "Analysing..." 
echo "In $file there are: "
variable=`(awk '/<event>/' $file)`
VAR=( $variable )
echo ${#VAR[@]} "events"
echo " "

echo "Cheers"