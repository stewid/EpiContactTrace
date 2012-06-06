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
##' available.
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
##' @keywords methods
##' @import R2HTML
##' @export
##' @examples
##'
##' # Load data
##' data(transfers)
##'
##' # Perform contact tracing
##' contactTrace <- Trace(movements=transfers,
##'                       root=2645,
##'                       tEnd='2005-10-31',
##'                       days=90)
##'
##' \dontrun{
##' # Generate an html report showing details of the contact tracing for
##' # root 2646.
##' # Note: Creates the files 2645.html and 2645.png in the working
##' # directory.
##' Report(contactTrace)
##'
##' # It's possible to generate reports for a list of ContactTrace objects.
##' # Perform contact tracing for ten of the included herds
##' root <- sort(unique(c(transfers$source, transfers$destination)))[1:10]
##'
##' # Perform contact tracing
##' contactTrace <- Trace(movements=transfers,
##'                       root=root,
##'                       tEnd='2005-10-31',
##'                       days=90)
##'
##' # Generate reports
##' # Note: Creates the files 1.html, 2.html, ..., 10.html and
##' # 1.png, 2.png, ..., 10.png in the working directory
##' Report(contactTrace)
##' }
##'
setGeneric('Report',
           signature = 'object',
           function(object, ...) standardGeneric('Report'))

setMethod('Report',
          signature(object = 'ContactTrace'),
          function(object, format='html', filename=NULL, template=NULL, verbose=TRUE, clean=TRUE)
      {
          if(any(identical(format, 'html'), identical(format, 'pdf'))) {
              if (exists('.ct_env', envir=globalenv())) {
                  stop('Unable to create report. The object .ct_env already exists in .GlobalEnv')
              }

              # Make sure the added environment is removed.
              on.exit(if (exists('.ct_env', envir=globalenv())) rm('.ct_env', envir=globalenv()))

              # In order to communicate with Sweave add an environment in .GlobalEnv
              assign('.ct_env', new.env(parent=globalenv()), envir=globalenv())

              # Add the ContactTrace object to the new environment
              assign('ct', object, envir=get('.ct_env', envir=globalenv()))

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
                  tools::texi2dvi(sub('rnw$', 'tex', basename(template)), pdf=TRUE, clean=clean)
                  file.rename(sub('rnw$', 'pdf', basename(template)), sprintf('%s.pdf', object@root))

                  if(identical(clean, TRUE)) {
                      unlink(sub('rnw$', 'tex', basename(template)))
                  }
              }
          }

          invisible(NULL)
      }
)

setMethod('Report',
          signature(object = 'list'),
          function(object, format='html', filename=NULL, template=NULL, verbose=TRUE, clean=TRUE)
      {
          if(!all(sapply(object, function(x) length(x)) == 1)) {
              stop('Unexpected length of list')
          }

          if(!all(sapply(object, function(x) class(x)) == 'ContactTrace')) {
              stop('Unexpected object in list')
          }

          lapply(object, function(x) Report(x,
                                            format=format,
                                            filename=filename,
                                            template=template,
                                            verbose=verbose,
                                            clean=clean))

          invisible(NULL)
      }
)
