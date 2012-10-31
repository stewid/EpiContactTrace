##' \code{InDegree}
##'
##' The number of herds with direct movements of animals to the root herd
##' during the defined time window used for tracing.
##'
##' @name InDegree-methods
##' @aliases InDegree
##' @aliases InDegree-methods
##' @aliases InDegree,Contacts-method
##' @aliases InDegree,ContactTrace-method
##' @aliases InDegree,list-method
##' @aliases InDegree,data.frame-method
##' @docType methods
##' @section Methods:
##' \describe{
##'   \item{\code{signature(x = "ContactTrace")}}{
##'     Get the InDegree of a \code{ContactTrace} object.
##'   }
##'
##'   \item{\code{signature(x = "list")}}{
##'     Get the InDegree for a list of \code{ContactTrace} objects.
##'     Each item in the list must be a \code{ContactTrace} object.
##'   }
##'
##'   \item{\code{signature(x = "data.frame")}}{
##'     Get the InDegree for a data.frame with movements, see examples.
##'   }
##' }
##' @seealso \code{\link{NetworkSummary}}
##' @param x a ContactTrace object, or a list of ContactTrace objects
##' or a \code{data.frame} with movements of animals between holdings,
##' see \code{\link{TraceDateInterval}} for details.
##' @param root vector of roots to perform contact tracing on.
##' @param tEnd the last date to include ingoing movements
##' @param days the number of previous days before tEnd to include
##' ingoing movements
##' @return A \code{data.frame} with the following columns:
##' \describe{
##'   \item{root}{
##'     The root of the contact tracing
##'   }
##'
##'   \item{inBegin}{
##'     The first date to include ingoing movements
##'   }
##'
##'   \item{inEnd}{
##'     The last date to include ingoing movements
##'   }
##'
##'   \item{inDays}{
##'     The number of days in the interval inBegin to inEnd
##'   }
##'
##'   \item{inDegree}{
##'     The \code{\link{InDegree}} of the root within the time-interval
##'   }
##' }
##'
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
##' result <- InDegree(transfers,
##'                    root=root,
##'                    tEnd='2005-10-31',
##'                    days=90)
##' }
##'
setGeneric('InDegree',
           signature = 'x',
           function(x, ...) standardGeneric('InDegree'))

## For internal use
setGeneric('in_degree',
           signature = 'x',
           function(x) standardGeneric('in_degree'))

## For internal use
setMethod('in_degree',
          signature(x = 'Contacts'),
          function(x)
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
          return(NetworkSummary(x)[, c('root',
                                       'inBegin',
                                       'inEnd',
                                       'inDays',
                                       'inDegree')])
      }
)

setMethod('InDegree',
          signature(x = 'list'),
          function(x)
      {
          return(NetworkSummary(x)[, c('root',
                                       'inBegin',
                                       'inEnd',
                                       'inDays',
                                       'inDegree')])
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
