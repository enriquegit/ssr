## Resubmission

This is a resubmission. In this version I have addressed the following suggestions:

* Please omit the redundant "in R" in the desription-    

Done.

* Please always write package names, software names and API names in 
single quotes in title and description. f.i: --> 'caret'    

Done.

* Please only capitalize sentence beginnings and names in the description 
text. --> f.i. ... co-training by commitee and ...    

Done.

* If there are references describing the methods in your package, please 
add these in the description field of your DESCRIPTION    

Done. A reference with the proposed format was added.

* Please unwrap the dontrun examples if they are executable in < 5 sec.    

'dontrun' blocks were ommited and examples were changed so that they run in < 5 sec.

* Please omit "on.exit(par(old_pars2), add = TRUE, after = FALSE)" from plot.ssr and other functions.    

Thanks for the suggestion. It has been fixed now.


## Test environments

* local Windows 10 install, R 3.6.1
* Ubuntu 16.04.6 LTS (on travis-ci), R 3.6.1
* R-hub debian-gcc-devel (R-devel)

## R CMD check results

There were no ERRORs, WARNINGs or NOTEs.

## Downstream dependencies

This is the first submission to CRAN.
