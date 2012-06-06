##' \code{NetworkSummary}
##'
##' \code{NetworkSummary} gives a summary of the contact tracing including the
##' time-window, \code{\link{InDegree}}, \code{\link{OutDegree}},
##' \code{\link{IngoingContactChain}} and \code{\link{OutgoingContactChain}}.
##'
##'
##' @name NetworkSummary-methods
##' @aliases NetworkSummary NetworkSummary-methods
##' NetworkSummary,ContactTrace-method NetworkSummary,list-method
##' @docType methods
##' @return A \code{data.frame} with the following columns:
##' \describe{
##'   \item{root}{
##'     The root of the contact tracing
##'   }
##'
##'   \item{inBegin}{
##'     Equals inBegin in \code{\link{TraceDateInterval}}
##'   }
##'
##'   \item{inEnd}{
##'     Equals inEnd in \code{\link{TraceDateInterval}}
##'   }
##'
##'   \item{outBegin}{
##'     Equals outBegin in \code{\link{TraceDateInterval}}
##'   }
##'
##'   \item{outEnd}{
##'     Equals outEnd in \code{\link{TraceDateInterval}}
##'   }
##'
##'   \item{inDegree}{
##'     The \code{\link{InDegree}} of the contact tracing
##'   }
##'
##'   \item{outDegree}{
##'     The \code{\link{OutDegree}} of the contact tracing
##'   }
##'
##'   \item{ingoingContactChain}{
##'     The \code{\link{IngoingContactChain}} of the contact tracing
##'   }
##'
##'   \item{outgoingContactChain}{
##'     The \code{\link{OutgoingContactChain}} of the contact tracing
##'   }
##' }
##' @section Methods:
##' \describe{
##'   \item{\code{signature(object = "ContactTrace")}}{
##'     Get the network summary for the ingoing and outgoing
##'     \code{Contacts} of a ContactTrace object.
##'   }
##'
##'   \item{\code{signature(object = "list")}}{
##'     Get the network summary for a list of \code{ContactTrace} objects.
##'     Each item in the list must be a \code{ContactTrace} object.
##'   }
##' }
##' @keywords methods
##' @import plyr
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
##' NetworkSummary(contactTrace)
##'
##' \dontrun{
##' # Create a network summary for all included herds
##' # First extract all source and destination from the dataset
##' root <- sort(unique(c(transfers$source,
##'                       transfers$destination)))
##'
##' # Perform contact tracing
##' contactTrace <- Trace(movements=transfers,
##'                       root=root,
##'                       tEnd='2005-10-31',
##'                       days=90)
##'
##' NetworkSummary(contactTrace)
##' }
##'
setGeneric('NetworkSummary',
           signature = 'object',
           function(object) standardGeneric('NetworkSummary'))

setMethod('NetworkSummary',
          signature(object = 'ContactTrace'),
          function(object)
      {
          data.frame(root=object@root,
                     inBegin=object@ingoingContacts@tBegin,
                     inEnd=object@ingoingContacts@tEnd,
                     outBegin=object@outgoingContacts@tBegin,
                     outEnd=object@outgoingContacts@tEnd,
                     inDegree=InDegree(object),
                     outDegree=OutDegree(object),
                     ingoingContactChain=IngoingContactChain(object),
                     outgoingContactChain=OutgoingContactChain(object))
      }
)

setMethod('NetworkSummary',
          signature(object = 'list'),
          function(object)
      {
          if(!all(sapply(object, function(x) length(x)) == 1)) {
              stop('Unexpected length of list')
          }

          if(!all(sapply(object, function(x) class(x)) == 'ContactTrace')) {
              stop('Unexpected object in list')
          }

          return(ldply(object, NetworkSummary)[,-1])
      }
)
