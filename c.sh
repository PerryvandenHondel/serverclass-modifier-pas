#!/bin/bash

##
## Use the set-build tool to increase the build number.
## Create UBuild.pas
##
~/development/pascal/set-build/set-build serverclass-modifier


##
## Compile the source and move the executable to the bin directory
##
fpc scmod.pas


##
## Remove the object file.
##
rm scmod.o

#git add bin/scproper
#git commit -m "Updated scproper executable"
#git push


