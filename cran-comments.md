## New version
This is a new version of the package (1.0.1) submitted as a response to an email by Prof Brian Ripley:

 Dear maintainer,

 Please see the problems shown on
<https://cran.r-project.org/web/checks/check_results_virtualPollen.html>.

 Please correct before 2022-02-25 to safely retain your package on CRAN.

 Do remember to look at the 'Additional issues'.

 The CRAN Team
 
From these issues, I only fixed the one appearing in the "donttest" section:

 "Error in `f()`:
! Either ymin or ymax must be given as an aesthetic."

It happens in a ggplot generated within the function parametersCheck(), where the syntax for the function ggplot2::geom_ribbon() was wrong. This issue is fixed now.


## Test environments
* Local: Ubuntu 18.04.2 LTS, R 3.6.0
* Rhub
  * macOS 10.11 El Capitan, R-release
  * Ubuntu Linux 16.04 LTS, R-devel
  * Ubuntu Linux 16.04 LTS, R-release
  * Windows Server 2008 R2 SP1, R-release, 32/64 bit
  * Windows Server 2008 R2 SP1, R-oldrel, 32/64 bit
  * Windows Server 2008 R2 SP1, R-devel, 32/64 bit
* win-builder (devel, release, oldrelease)


## R CMD check results
0 errors ✓ | 0 warnings ✓ | 0 notes ✓
