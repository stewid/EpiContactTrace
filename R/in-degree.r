##' \code{InDegree}
##'
##' The number of herds with direct movements of animals to the root herd
##' during the defined time window used for tracing.
##'
##'
##' @name InDegree-methods
##' @aliases InDegree InDegree-methods InDegree,Contacts-method
##' InDegree,ContactTrace-method InDegree,list-method
##' InDegree,data.frame-method
##' @docType methods
##' @return An integer vector.
##' @section Methods:
##' \describe{
##'   \item{\code{signature(x = "Contacts")}}{
##'     Get the InDegree of a \code{Contacts} object with ingoing direction.
##'   }
##'
##'   \item{\code{signature(x = "ContactTrace")}}{
##'     Get the InDegree of a \code{ContactTrace} object.
##'   }
##'
##'   \item{\code{signature(x = "list")}}{
##'     Get the InDegree for a list of \code{ContactTrace} objects.
##'     Each item in the list must be a \code{ContactTrace} object.
##'   }
##' }
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
##' }
##' @keywords methods
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
##' InDegree(contactTrace)
##'
##' \dontrun{
##' ## Perform contact tracing for all included herds
##' ## First extract all source and destination from the dataset
##' root <- sort(unique(c(transfers$source,
##'                       transfers$destination)))
##'
##' ## Perform contact tracing
##' contactTrace <- Trace(movements=transfers,
##'                       root=root,
##'                       tEnd='2005-10-31',
##'                       days=90)
##'
##' InDegree(contactTrace)
##' }
##'
setGeneric('InDegree',
           signature = 'x',
           function(x, ...) standardGeneric('InDegree'))

setMethod('InDegree',
          signature(x = 'Contacts'),
          function (x)
      {
          if(!identical(x@direction, 'in')) {
              stop('Unable to determine InDegree for outgoing contacts')
          }

          return(length(unique(x@source[x@destination==x@root])))
      }
)

setMethod('InDegree',
          signature(x = 'ContactTrace'),
          function (x)
      {
          return(InDegree(x@ingoingContacts))
      }
)

setMethod('InDegree',
          signature(x = 'list'),
          function(x)
      {
          if(!all(sapply(x, function(y) length(y)) == 1)) {
              stop('Unexpected length of list')
          }

          if(!all(sapply(x, function(y) class(y)) == 'ContactTrace')) {
              stop('Unexpected object in list')
          }

          return(sapply(x, InDegree))
      }
)

setMethod('InDegree',
          signature(x = 'data.frame'),
          function(x,
                   root,
                   tEnd,
                   days)
      {
          if(any(missing(x),
                 missing(root),
                 missing(tEnd),
                 missing(days))) {
              stop('Missing parameters in call to InDegree')
          }

          return(NetworkSummary(x, root, tEnd, days)[, c('root',
                                                         'inBegin',
                                                         'inEnd',
                                                         'inDays',
                                                         'inDegree')])
      }
)
