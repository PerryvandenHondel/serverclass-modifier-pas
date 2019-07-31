#!/bin/bash


##
## Compile the source and move the executable to the bin directory
##
fpc scproper.pas -obin/scproper


##
## Remove the object file.
##
rm bin/scproper.o

#git add bin/scproper
#git commit -m "Updated scproper executable"
#git push


