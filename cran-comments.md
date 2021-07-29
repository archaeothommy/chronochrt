## Resubmission

I fixed the following points outlined in the mail

* We do not need "+ file LICENSE"... please omit it: 
    -> deleted from DESCRIPTION, file LICENSE excluded from package

* Missing Rd-tag \value in geom_chronochRt.Rd, geom_chronochRtImage.Rd, pipe.Rd 
    -> added to all functions \value and description of the output
     
* Use of installed.packages()
    -> replaced with requireNamespace()

## Test environments
* ubuntu 20.04 LTS (on gitlab-runner CI and Rhub), R 4.1.0
* Fedora Linux (on RHub), R-devel
* Windows 8.1 (R-devel and R-release)
* Windows Server 2008 R2 SP1 (on RHub), R-devel

## R CMD check results
There were no ERRORs, no WARNINGs and 1 NOTE.

NOTE: 

* new submission 



