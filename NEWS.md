# EpiContactTrace 0.16.0 (2021-08-30)

## BUG FIXES

* Fixed broken examples for InDegree, OutDegree and
  IngoingContactChain.

# EpiContactTrace 0.15.0 (2020-12-12)

## IMPROVEMENTS

* NetworkStructure: add a method that works on a list of ContactTrace
  objects.

* NetworkSummary: add a method that works on a list of ContactTrace
  objects.

# EpiContactTrace 0.14.0 (2020-09-09)

## IMPROVEMENTS

* Renamed the `NEWS` file to `NEWS.md` and changed to use markdown
  format style.

* Removed the dependency to the plyr package for building pdf reports.

* Added orcid id:s in the DESCRIPTION file for the package authors.

* Changed the file extension for files in the R folder from '.r' to
  '.R'.

* Used roxygen2 ver 7.1.1 to build the documentation.

## BUG FIXES

* Added the missing table for direct outgoing contacts in the html
  report.

# EpiContactTrace 0.13.0 (2020-03-15)

## IMPROVEMENTS

* Set 'stringsAsFactors = FALSE' in the data.frame returned by
  'NetworkSummary'.

# EpiContactTrace 0.12.0 (2017-10-19)

## BUG FIXES

* Fix format of authors in CITATION file.

# EpiContactTrace 0.11.0 (2017-07-20)

## BUG FIXES

* Added missing calls to 'R_forceSymbols' and 'R_useDynamicSymbols' in
  the C init function.

# EpiContactTrace 0.10.0 (2017-07-20)

## IMPROVEMENTS

* Added the 'maxDistance' argument to the 'Trace' method to stop
  contact tracing at 'maxDistance' (inclusive) from the root node.
  Default is ‘NULL’ i.e. to not use the 'maxDistance' stop criteria.

# EpiContactTrace 0.9.1 (2015-07-03)

## BUG FIXES

* Fixed build failure on the Solaris platform.

# EpiContactTrace 0.9.0 (2015-07-02)

## CHANGES

* Removed Animate method to pass the CRAN checks. Install version
  0.8.8 manually from the archive if that method is required. See
  section 6.3 in 'R Installation and Administration' on how to install
  packages from source.

* Removed dependencies to plyr, R2HTML and Rcpp.

* Removed usage of testthat when testing the package.

* Removed list methods for ContactTrace objects. When calculating
  measures for several roots, the purpose is to use methods that work
  directly on the data.frame with movements.

* Updated maintainer email

* Changed 'Title' field in DESCRIPTION to title case

## PERFORMANCE IMPROVEMENTS

* Improved performance when calculating ShortestPaths on a data.frame
  of movements.

# EpiContactTrace 0.8.8 (2013-11-05)

## BUG FIXES

* Fixed failing test case 'Duplicate movements'

* Fixed NOTE not importing 'methods' from R CMD check


# EpiContactTrace 0.8.7

## NOTE

* Changed argument representation to slots in setClass for the two S4
  classes ContactTrace and Contacts. The package now requires R>=3.0.0

# EpiContactTrace 0.8.6 (2013-09-01)

## NOTE

* Plots are not supported in version 0.8.6 since igraph0 has been
  archived. We intend to resolve the issue in a future version.
  Install version 0.8.5 and igraph0 manually from the archive if plots
  are required. See section 6.3 in 'R Installation and Administration'
  on how to install packages from source.

# EpiContactTrace 0.8.5 (2013-07-19)

## NEW FEATURES

* Updated DESCRIPTION to use Imports instead of Depends for igraph0,
  plyr, R2HTML.

# EpiContactTrace 0.8.4

## NEW FEATURES

* Added method ReportObject to be called from a Sweave report template
  to get the current ContactTrace object.

# EpiContactTrace 0.8.3

## NEW FEATURES

* inDays and outDays now returned as integer from NetworkSummary of a
  ContactTrace object.

* Added 'all' to interval argument in Animate to view visualize all
  movements in one image. The default interval argument was changed to
  'all'.

## BUGFIX

* Fixed a bug that was introduced in ver 0.7.6 when performing contact
  tracing.

# EpiContactTrace 0.8.2

## BUGFIX

* Fixed a bug that was introduced in ver 0.7.6 when calculating
  IngoingContactChain and OutgoingContactChain using NetworkSummary.


# EpiContactTrace 0.8.0

## NEW FEATURES

* Using the the animation and ggmap package, contacts can now be
  visualized and animated.

# EpiContactTrace 0.7.6

## PERFORMANCE IMPROVEMENTS

* Improved performance when calculating contact chain in C++ code by
  counting the number of visited nodes more efficient.

# EpiContactTrace 0.7.5
---------------------

NEW FEATURES

* Report now uses match.arg to check output format.

# EpiContactTrace 0.7.4

## NEW FEATURES

* Added the parameters inBegin, inEnd, outBegin and outEnd to
  NetworkSummary to allow for different time windows for ingoing and
  outgoing contacts.

# EpiContactTrace 0.7.3

## BUG FIXES

* Fixed bug in Report. InDegree were not defined for the S4 class
  Contacts.

# EpiContactTrace 0.7.2

## NEW FEATURES

* Improved performance in C++ code by using visited nodes more
  efficient

# EpiContactTrace 0.7.1 (2012-11-01)

## NEW FEATURES

* Updated documentation

## BUG FIXES

* Fixed bug in C++ code not checking tEnd constraint.

* Fixed bug in NetworkSummary when calculating degree.

# EpiContactTrace 0.7.0

## NEW FEATURES

* When the methods Trace, NetworkSummary, InDegree, OutDegree,
  IngoingContactChain, OutgoingContactChain are called with the
  arguments movements, root, tEnd and days; the measures are
  calculated for each combination of root, tEnd and days.

## PERFORMANCE IMPROVEMENTS

* Ongoing work to improve performance using Rcpp.

# EpiContactTrace 0.6.9 (2012-06-21)

## NEW FEATURES

* Changed from igraph to igraph0 to handle the update of igraph to 0.6.

## BUG FIXES

* Fixed bug with Report of a list of ContactTrace objects.  Each
  ContactTrace object called Report with missing parameter clean.

# EpiContactTrace 0.6.8 (2012-06-12)

## BUG FIXES

* The html and pdf reports have been corrected. The table displaying
  individual outgoing contacts erronously switched source and
  destination.

# EpiContactTrace 0.6.7

## NEW FEATURES

* Removed unused parameters from Report method.

# EpiContactTrace 0.6.6

## NEW FEATURES

* Updated documentation.

# EpiContactTrace 0.6.5

## NEW FEATURES

* Updated documentation.

# EpiContactTrace 0.6.4

## NEW FEATURES

* Updated documentation.

# EpiContactTrace 0.6.3

## NEW FEATURES

* All documentation generated via roxygen.

# EpiContactTrace 0.6.2

## BUG FIXES

* If movements contained duplicate contacts, they were all added.  Now
  only unique contacts are included from the contact tracing.

# EpiContactTrace 0.6.1

## BUG FIXES

* 'plot' now works as intended for the S4 ContactTrace object with
  only outgoing contacts

# EpiContactTrace 0.6.0

## NEW FEATURES

* Added the method ShortestPaths

* NetworkStructure gives the distance from root for each node during
  the depth first search.


# EpiContactTrace 0.5.3

## NEW FEATURES

* Added the example dataset transfers

# EpiContactTrace 0.5.2

## NEW FEATURES

* Can now generate an 'html' report for the contact tracing using
  'Report'

* Can now generate a 'pdf' report for the contact tracing using
  'Report'

## BUG FIXES

* 'show' now works as intended for the S4 ContactTrace object
