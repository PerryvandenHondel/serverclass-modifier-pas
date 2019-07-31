#!/bin/bash


##
## Copy the test file to the original file.
##
#cp samples/serverclass.conf.test bin/serverclass.conf


##
## Run the program
##
bin/scmod svc_testclass whitelist add dummybox2000*
bin/scmod svc_testclass whitelist add dummybox2010*
bin/scmod svc_testclass whitelist add dummybox2933*
bin/scmod svc_testclass whitelist add dummybox2929*


