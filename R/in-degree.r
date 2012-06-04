##' \code{InDegree}
##'
##' The number of herds with direct movements of animals to the root herd
##' during the defined time window used for tracing.
##'
##'
##' @name InDegree-methods
##' @aliases InDegree InDegree-methods InDegree,Contacts-method
##' InDegree,ContactTrace-method InDegree,list-method
##' @docType methods
##' @return An integer vector.
##' @section Methods:
##' \describe{
##'   \item{\code{signature(object = "Contacts")}}{
##'     Get the InDegree of a \code{Contacts} object with ingoing direction.
##'   }
##'
##'   \item{\code{signature(object = "ContactTrace")}}{
##'     Get the InDegree of a \code{ContactTrace} object.
##'   }
##'
##'   \item{\code{signature(object = "list")}}{
##'     Get the InDegree for a list of \code{ContactTrace} objects.
##'     Each item in the list must be a \code{ContactTrace} object.
##'   }
##' }
##' @keywords methods
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
##' InDegree(contactTrace)
##'
##' \dontrun{
##' # Perform contact tracing for all included herds
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
##' InDegree(contactTrace)
##' }
##'
setGeneric('InDegree',
           signature = 'object',
           function(object) standardGeneric('InDegree'))

setMethod('InDegree',
          signature(object = 'Contacts'),
          function (object)
      {
          if(!identical(object@direction, 'in')) {
              stop('Unable to determine InDegree for outgoing contacts')
          }

          return(length(unique(object@source[object@destination==object@root])))
      }
)

setMethod('InDegree',
          signature(object = 'ContactTrace'),
          function (object)
      {
          return(InDegree(object@ingoingContacts))
      }
)

setMethod('InDegree',
          signature(object = 'list'),
          function(object)
      {
          if(!all(sapply(object, function(x) length(x)) == 1)) {
              stop('Unexpected length of list')
          }

          if(!all(sapply(object, function(x) class(x)) == 'ContactTrace')) {
              stop('Unexpected object in list')
          }

          return(sapply(object, InDegree))
      }
)
