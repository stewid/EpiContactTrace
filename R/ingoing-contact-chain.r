##' \code{IngoingContactChain}
##'
##' The ingoing contact chain is the number of holdings in the network of
##' direct and indirect contacts to the root holding, with regard to temporal
##' and order of the contacts during the defined time window used for contact tracing.
##'
##'
##' @name IngoingContactChain-methods
##' @aliases IngoingContactChain IngoingContactChain-methods
##' IngoingContactChain,Contacts-method IngoingContactChain,ContactTrace-method
##' IngoingContactChain,list-method
##' @docType methods
##' @return An integer vector.
##' @section Methods:
##' \describe{
##'   \item{\code{signature(object = "Contacts")}}{
##'     Get the IngoingContactChain of a \code{Contacts} object with ingoing direction.
##'   }
##'
##'   \item{\code{signature(object = "ContactTrace")}}{
##'     Get the IngoingContactChain of a \code{ContactTrace} object.
##'   }
##'
##'   \item{\code{signature(object = "list")}}{
##'     Get the IngoingContactChain for a list of \code{ContactTrace} objects.
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
##' IngoingContactChain(contactTrace)
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
##' IngoingContactChain(contactTrace)
##' }
##'
setGeneric('IngoingContactChain',
           signature = 'object',
           function(object) standardGeneric('IngoingContactChain'))

setMethod('IngoingContactChain',
          signature(object = 'Contacts'),
          function (object)
      {
          if(!identical(object@direction, 'in')) {
              stop('Unable to determine IngoingContactChain for outgoing contacts')
          }

          return(length(setdiff(object@source,object@root)))
      }
)

setMethod('IngoingContactChain',
          signature(object = 'ContactTrace'),
          function (object)
      {
          IngoingContactChain(object@ingoingContacts)
      }
)

setMethod('IngoingContactChain',
          signature(object = 'list'),
          function(object)
      {
          if(!all(sapply(object, function(x) length(x)) == 1)) {
              stop('Unexpected length of list')
          }

          if(!all(sapply(object, function(x) class(x)) == 'ContactTrace')) {
              stop('Unexpected object in list')
          }

          return(sapply(object, IngoingContactChain))
      }
)
