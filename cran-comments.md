## Resubmission

I fixed the following points outlined in the mail

* Please remove the redundant "with R" from your title:
    -> deleted from DESCRIPTION

* Unexecutable code in man/import_chron.Rd: 
    -> fixed error (many thanks for pointing it out!)
    
* Omit any default path in writing functions.
    -> functions in examples and tests are now writing to tempdir(), 
    -> functions in vignettes are set to display only

* There are no references that describe the used methods. The package implements a grown visualisation standard. 

## Test environments
* tested on r-hub
* tested on win-builder.r-project.com
* tested on mac.r-project.com

## R CMD check results
There were no ERRORs, no WARNINGs and no NOTEs.
