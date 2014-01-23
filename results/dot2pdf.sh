#!/bin/bash

dot -Tps2 $1.dot -o $1.eps
ps2pdf $1.eps $1.pdf

#gs -q -dSAFER -dNOPAUSE -dBATCH -dAutoRotatePages=/All -sOutputFile=$1.pdf -sDEVICE=pdfwrite -c .setpdfwrite -f $1.eps
# this setting forces to print with landscape orientation
#gs -q -dSAFER -dNOPAUSE -dBATCH -dAutoRotatePages=/None -sOutputFile=$1.pdf -sDEVICE=pdfwrite -c "<</Orientation 3>> setpagedevice" -c .setpdfwrite -f $1.eps
