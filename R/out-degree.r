##' \code{OutDegree}
##'
##' The number of herds with direct movements of animals from the root herd
##' during the defined time window used for tracing
##'
##'
##' @name OutDegree-methods
##' @aliases OutDegree OutDegree-methods OutDegree,Contacts-method
##' OutDegree,ContactTrace-method OutDegree,list-method
##' @docType methods
##' @return An integer vector.
##' @section Methods:
##' \describe{
##'   \item{\code{signature(object = "Contacts")}}{
##'     Get the OutDegree of a \code{Contacts} object with outgoing direction.
##'   }
##'
##'   \item{\code{signature(object = "ContactTrace")}}{
##'     Get the OutDegree of a \code{ContactTrace} object.
##'   }
##'
##'   \item{\code{signature(object = "list")}}{
##'     Get the OutDegree for a list of \code{ContactTrace} objects.
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
##' # Load data
##' data(transfers)
##'
##' # Perform contact tracing
##' contactTrace <- Trace(movements=transfers,
##'                       root=2645,
##'                       tEnd='2005-10-31',
##'                       days=90)
##'
##' OutDegree(contactTrace)
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
##' OutDegree(contactTrace)
##' }
##'
setGeneric('OutDegree',
           signature = 'object',
           function(object) standardGeneric('OutDegree'))

setMethod('OutDegree',
          signature(object = 'Contacts'),
          function (object)
      {
          if(!identical(object@direction, 'out')) {
              stop('Unable to determine OutDegree for ingoing contacts')
          }

          return(length(unique(object@destination[object@source==object@root])))
      }
)

setMethod('OutDegree',
          signature(object = 'ContactTrace'),
          function (object)
      {
          return(OutDegree(object@outgoingContacts))
      }
)

setMethod('OutDegree',
          signature(object = 'list'),
          function(object)
      {
          if(!all(sapply(object, function(x) length(x)) == 1)) {
              stop('Unexpected length of list')
          }

          if(!all(sapply(object, function(x) class(x)) == 'ContactTrace')) {
              stop('Unexpected object in list')
          }

          return(sapply(object, OutDegree))
      }
)
