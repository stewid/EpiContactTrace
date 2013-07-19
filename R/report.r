## Copyright 2013 Stefan Widgren and Maria Noremark,
## National Veterinary Institute, Sweden
##
## Licensed under the EUPL, Version 1.1 or - as soon they
## will be approved by the European Commission - subsequent
## versions of the EUPL (the "Licence");
## You may not use this work except in compliance with the
## Licence.
## You may obtain a copy of the Licence at:
##
## http://ec.europa.eu/idabc/eupl
##
## Unless required by applicable law or agreed to in
## writing, software distributed under the Licence is
## distributed on an "AS IS" basis,
## WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
## express or implied.
## See the Licence for the specific language governing
## permissions and limitations under the Licence.

##' Generate a contact tracing \code{Report}
##'
##' EpiContatTrace contains report templates to generate pdf- or html reports
##' for the farm specific contacts. These reports can be useful for hands-on
##' disease tracing in the field. The templates are used by Sweave and can be
##' adapted by the end user. However, in the default setting the report has the
##' following layout; first the contacts are visualised graphically in a plot,
##' as to give an immediate signal to the reader of the report of the number of
##' contacts. In the following, the contact data are presented with different
##' levels of detail split by ingoing and outgoing contacts. The first includes
##' collapsed data and the sequential contact structure at group level (i.e. no
##' information on individuals or dates). In this summary, the sequential
##' structure of each part of the chain is included, and a holding that appears
##' in several different parts of the chain can therefore be included more than
##' once in the summary. The reason for this is to facilitate sequential
##' tracing and getting an overview of each part of the chain. After the
##' summary all details of all contacts included in the contact chains is
##' presented, i.e. date of contact and data on individual level when
##' available. To generate pdf files a TeX installation must exist to compile the
##' latex file. The report is saved in the working directory with the name of
##' the root as filename.
##'
##'
##' @name Report-methods
##' @aliases Report Report-methods Report,ContactTrace-method
##' Report,list-method
##' @docType methods
##' @section Methods: \describe{
##'
##'   \item{\code{signature(object = "ContactTrace")}}{
##'     Generate a report for a \code{ContactTrace} object.
##'   }
##'
##'   \item{\code{signature(object = "list")}}{
##'     Generate reports for a list of \code{ContactTrace} obejcts.
##'   }
##' }
##' @param object the object
##' @param format the format to use, can be either 'html' or 'pdf'. The default
##'        is 'html'
##' @param template the Sweave template file to use. If none is provided, the default
##'        is used.
##' @references \itemize{
##'   \item Dube, C., et al., A review of network analysis terminology
##'     and its application to foot-and-mouth disease modelling and policy
##'     development. Transbound Emerg Dis 56 (2009) 73-85, doi:
##'     10.1111/j.1865-1682.2008.01064.x
##'
##'   \item Noremark, M., et al., Network analysis
##'     of cattle and pig movements in Sweden: Measures relevant for
##'     disease control and riskbased surveillance.  Preventive Veterinary
##'     Medicine 99 (2011) 78-90, doi: 10.1016/j.prevetmed.2010.12.009
##'
##'   \item Friedrich Leisch. Sweave: Dynamic generation of
##'     statistical reports using literate data analysis. In Wolfgang
##'     Hardle and Bernd Ronz, editors, Compstat 2002 - Proceedings in
##'     Computational Statistics, pages 575-580. Physica Verlag,
##'     Heidelberg, 2002. ISBN 3-7908-1517-9.
##'     \url{http://www.statistik.uni-muenchen.de/~leisch/Sweave/}
##' }
##' @seealso Sweave, R2HTML, texi2pdf.
##' @keywords methods
##' @import R2HTML
##' @export
##' @examples
##'
##' ## Load data
##' data(transfers)
##'
##' ## Perform contact tracing
##' contactTrace <- Trace(movements=transfers,
##'                       root=2645,
##'                       tEnd='2005-10-31',
##'                       days=90)
##'
##' \dontrun{
##' ## Generate an html report showing details of the contact tracing for
##' ## root 2646.
##' ## Note: Creates the files 2645.html and 2645.png in the working
##' ## directory.
##' Report(contactTrace)
##'
##' ## It's possible to generate reports for a list of ContactTrace objects.
##' ## Perform contact tracing for ten of the included herds
##' root <- sort(unique(c(transfers$source, transfers$destination)))[1:10]
##'
##' ## Perform contact tracing
##' contactTrace <- Trace(movements=transfers,
##'                       root=root,
##'                       tEnd='2005-10-31',
##'                       days=90)
##'
##' ## Generate reports
##' ## Note: Creates the files 1.html, 2.html, ..., 10.html and
##' ## 1.png, 2.png, ..., 10.png in the working directory
##' Report(contactTrace)
##' }
##'
setGeneric('Report',
           signature = 'object',
           function(object, ...) standardGeneric('Report'))

setMethod('Report',
          signature(object = 'ContactTrace'),
          function(object, format=c('html', 'pdf'), template=NULL)
      {
          format <- match.arg(format)

          if (!is.null(.ct_env$ct)) {
              stop('Unable to create report. The ct object already exists')
          }

          ## Make sure the added object is removed.
          on.exit(.ct_env$ct <- NULL)

          ## Add the ContactTrace object to the .ct_env environment
          .ct_env$ct <- object

          if(identical(format, 'html')) {
              if(is.null(template)) {
                  template <- system.file('Sweave/speak-html.rnw', package='EpiContactTrace')
              }

              Sweave(template, driver=R2HTML::RweaveHTML(), syntax="SweaveSyntaxNoweb")
              file.rename(sub('rnw$', 'html', basename(template)), sprintf('%s.html', object@root))
          } else {
              if(is.null(template)) {
                  template <- system.file('Sweave/speak-latex.rnw', package='EpiContactTrace')
              }

              Sweave(template, syntax="SweaveSyntaxNoweb")
              tools::texi2pdf(sub('rnw$', 'tex', basename(template)), clean=TRUE)
              file.rename(sub('rnw$', 'pdf', basename(template)), sprintf('%s.pdf', object@root))
              unlink(sub('rnw$', 'tex', basename(template)))
          }

          invisible(NULL)
      }
)

setMethod('Report',
          signature(object = 'list'),
          function(object, format=c('html', 'pdf'), template=NULL)
      {
          format <- match.arg(format)

          if(!all(sapply(object, function(x) length(x)) == 1)) {
              stop('Unexpected length of list')
          }

          if(!all(sapply(object, function(x) class(x)) == 'ContactTrace')) {
              stop('Unexpected object in list')
          }

          lapply(object, function(x) Report(x,
                                            format=format,
                                            template=template))

          invisible(NULL)
      }
)

## In order to communicate with Sweave add an environment to store the
## current report object
.ct_env <- new.env()

##' Get current \code{ContactTrace} object when generating a report
##'
##' EpiContatTrace contains report templates to generate pdf- or html reports
##' for the farm specific contacts. These reports can be useful for hands-on
##' disease tracing in the field. The templates are used by Sweave and can be
##' adapted by the end user. This method enables communication of the current
##' \code{ContactTrace} object to the report.
##'
##'
##' @usage ReportObject()
##' @return The current \code{ContactTrace} object when generating a report
##' @export
ReportObject <- function()
{
    return(.ct_env$ct)
}
