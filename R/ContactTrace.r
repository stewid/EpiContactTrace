##' Class \code{"ContactTrace"}
##'
##' Class to handle contact tracing.
##'
##'
##' @name ContactTrace-class
##' @docType class
##' @section Objects from the Class: Objects can be created by calls of the
##' form \code{new("ContactTrace", root, ingoingContacts, outgoingContacts,
##' ...)}. %% ~~ describe objects here ~~
##' @keywords classes
##' @export
##' @examples
##'
##' showClass("ContactTrace")
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


