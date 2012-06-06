##' Class \code{"ContactTrace"}
##'
##' Class to handle contact tracing.
##'
##'
##' The \code{ContactTrace} class holds information for the ingoing and outgoing
##' contact chain for a specific root within the time window used for contact
##' tracing.
##' \describe{
##'   \item{\code{root}}{
##'     A \code{character} vector of length one with the identifier of the root.
##'   }
##'   \item{\code{ingoingContacts}}{
##'     A \code{Contacts} object with the contacts for the ingoing contact chain.
##'   }
##'   \item{\code{outgoingContacts}}{
##'     A \code{Contacts} object with the contacts for the outgoing contact chain.
##'   }
##' }
##' @name ContactTrace-class
##' @docType class
##' @section Objects from the Class: Objects can be created by calls of the
##' form \code{new("ContactTrace",root, ingoingContacts, outgoingContacts,...)}
##' @keywords classes
##' @export
##' @examples
##'
##' # Load data
##' data(transfers)
##'
##' # Perform contact tracing
##' contactTrace <- Trace(movements = transfers,
##'                       root = 2645,
##'                       tEnd = '2005-10-31',
##'                       days = 90)
##'
##' # Show structure
##' str(contactTrace)
##'
setClass('ContactTrace',
         representation(root = 'character',
                        ingoingContacts = 'Contacts',
                        outgoingContacts = 'Contacts'))

setAs(from='ContactTrace',
      to='data.frame',
      def=function(from)
  {
      return(rbind(as(from@ingoingContacts, 'data.frame'),
                   as(from@outgoingContacts, 'data.frame')))
  }
)


