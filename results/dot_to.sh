#!/bin/bash

usage()
{
    echo "Usage: $0 [-t <type>] [-h];"
    exit 0;
}

NO_ARGS=0 
E_OPTERROR=85

if [ $# -eq "$NO_ARGS" ]    # Script invoked with no command-line args?
then
  echo "Usage: `basename $0` options (-t)"
  exit $E_OPTERROR          # Exit and explain usage.
                            # Usage: scriptname -options
                            # Note: dash (-) necessary
fi  

while getopts :t opt; do
   case $opt in
     t ) echo "Chose output type $OPTARG" ;;
				 #dot -Tps2 $1.dot -o $1.eps
				 #ps2pdf $1.eps $1.pdf ;;
    \? ) usage   ;;
  esac
done

#dot -Tps2 $1.dot -o $1.eps
#ps2pdf $1.eps $1.pdf

#gs -q -dSAFER -dNOPAUSE -dBATCH -dAutoRotatePages=/All -sOutputFile=$1.pdf -sDEVICE=pdfwrite -c .setpdfwrite -f $1.eps
# this setting forces to print with landscape orientation
#gs -q -dSAFER -dNOPAUSE -dBATCH -dAutoRotatePages=/None -sOutputFile=$1.pdf -sDEVICE=pdfwrite -c "<</Orientation 3>> setpagedevice" -c .setpdfwrite -f $1.eps
