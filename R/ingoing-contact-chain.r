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
##' IngoingContactChain,list-method IngoingContactChain,data.frame-method
##' @docType methods
##' @return An integer vector.
##' @section Methods:
##' \describe{
##'   \item{\code{signature(x = "Contacts")}}{
##'     Get the IngoingContactChain of a \code{Contacts} object with ingoing direction.
##'   }
##'
##'   \item{\code{signature(x = "ContactTrace")}}{
##'     Get the IngoingContactChain of a \code{ContactTrace} object.
##'   }
##'
##'   \item{\code{signature(x = "list")}}{
##'     Get the IngoingContactChain for a list of \code{ContactTrace} objects.
##'     Each item in the list must be a \code{ContactTrace} object.
##'   }
##'
##'   \item{\code{signature(x = "data.frame")}}{
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
##' ## Load data
##' data(transfers)
##'
##' ## Perform contact tracing
##' contactTrace <- Trace(movements=transfers,
##'                       root=2645,
##'                       tEnd='2005-10-31',
##'                       days=90)
##'
##' IngoingContactChain(contactTrace)
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
##' IngoingContactChain(contactTrace)
##' }
##'
setGeneric('IngoingContactChain',
           signature = 'x',
           function(x, ...) standardGeneric('IngoingContactChain'))

setMethod('IngoingContactChain',
          signature(x = 'Contacts'),
          function (x)
      {
          if(!identical(x@direction, 'in')) {
              stop('Unable to determine IngoingContactChain for outgoing contacts')
          }

          return(length(setdiff(x@source,x@root)))
      }
)

setMethod('IngoingContactChain',
          signature(x = 'ContactTrace'),
          function (x)
      {
          IngoingContactChain(x@ingoingContacts)
      }
)

setMethod('IngoingContactChain',
          signature(x = 'list'),
          function(x)
      {
          if(!all(sapply(x, function(y) length(y)) == 1)) {
              stop('Unexpected length of list')
          }

          if(!all(sapply(x, function(y) class(y)) == 'ContactTrace')) {
              stop('Unexpected object in list')
          }

          return(sapply(x, IngoingContactChain))
      }
)

setMethod('IngoingContactChain',
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
              stop('Missing parameters in call to IngoingContactChain')
          }

          return(NetworkSummary(x, root, tEnd, days)[, c('root',
                                                         'inBegin',
                                                         'inEnd',
                                                         'inDays',
                                                         'ingoingContactChain')])
      }
)

