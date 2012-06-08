##' \code{OutgoingContactChain}
##'
##' The outgoing contact chain is the number of holdings in the network of
##' direct and indirect contacts from the root holding, with regard to temporal
##' and order of the contacts during the defined time window used for contact
##' tracing.
##'
##'
##' @name OutgoingContactChain-methods
##' @aliases OutgoingContactChain OutgoingContactChain-methods
##' OutgoingContactChain,Contacts-method
##' OutgoingContactChain,ContactTrace-method OutgoingContactChain,list-method
##' @docType methods
##' @return An integer vector.
##' @section Methods:
##' \describe{
##'   \item{\code{signature(object = "Contacts")}}{
##'     Get the OutgoingContactChain of a \code{Contacts} object with outgoing direction.
##'   }
##'
##'   \item{\code{signature(object = "ContactTrace")}}{
##'     Get the OutgoingContactChain of a \code{ContactTrace} object.
##'   }
##'
##'   \item{\code{signature(object = "list")}}{
##'     Get the OutgoingContactChain for a list of \code{ContactTrace} objects.
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
##' OutgoingContactChain(contactTrace)
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
##' OutgoingContactChain(contactTrace)
##' }
##'
setGeneric('OutgoingContactChain',
           signature = 'object',
           function(object) standardGeneric('OutgoingContactChain'))

setMethod('OutgoingContactChain',
          signature(object = 'Contacts'),
          function (object)
      {
          if(!identical(object@direction, 'out')) {
              stop('Unable to determine OutgoingContactChain for ingoing contacts')
          }

          return(length(setdiff(object@destination,object@root)))
      }
)

setMethod('OutgoingContactChain',
          signature(object = 'ContactTrace'),
          function (object)
      {
          OutgoingContactChain(object@outgoingContacts)
      }
)

setMethod('OutgoingContactChain',
          signature(object = 'list'),
          function(object)
      {
          if(!all(sapply(object, function(x) length(x)) == 1)) {
              stop('Unexpected length of list')
          }

          if(!all(sapply(object, function(x) class(x)) == 'ContactTrace')) {
              stop('Unexpected object in list')
          }

          return(sapply(object, OutgoingContactChain))
      }
)
