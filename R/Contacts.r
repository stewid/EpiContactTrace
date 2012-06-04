##' Class \code{"Contacts"}
##'
##' Class to handle contacts.
##'
##'
##' @name Contacts-class
##' @docType class
##' @section Objects from the Class: Objects can be created by calls of the
##' form \code{new("Contacts", root, startDate, days, source, destination, t,
##' id, n, category, level, direction, ...)}.
##' @keywords classes
##' @export
##' @examples
##'
##' showClass("Contacts")
##'
setClass('Contacts',
         representation(root = 'character',
                        tBegin = 'Date',
                        tEnd = 'Date',
                        source = 'character',
                        destination = 'character',
                        t = 'Date',
                        id = 'character',
                        n = 'numeric',
                        category = 'character',
                        index = 'integer',
                        distance = 'integer',
                        direction = 'character'),
         validity = function(object) {
             retval <- NULL

             l <- unique(c(length(object@source),
                           length(object@destination),
                           length(object@t),
                           length(object@id),
                           length(object@n),
                           length(object@category)))
             if(!identical(length(l), 1L)) {
                 retval <- 'Lengths of source, destination, t, id, n and category should have equal length'
             }

             if(is.null(retval)) {
                 return(TRUE)
             }

             return(retval)
         }
)

setAs(from='Contacts',
      to='data.frame',
      def=function(from)
  {
      if(length(from@source) > 0L) {
          df1 <- data.frame(source=from@source,
                            destination=from@destination,
                            t=from@t,
                            id=from@id,
                            n=from@n,
                            category=from@category,
                            stringsAsFactors=FALSE)

          if(identical(from@direction, 'in')) {
              df2 <- data.frame(root=from@root,
                                inBegin=from@tBegin,
                                inEnd=from@tEnd,
                                outBegin=as.Date(as.character(NA)),
                                outEnd=as.Date(as.character(NA)),
                                direction='in',
                                stringsAsFactors=FALSE)
          } else {
              df2 <- data.frame(root=from@root,
                                inBegin=as.Date(as.character(NA)),
                                inEnd=as.Date(as.character(NA)),
                                outBegin=from@tBegin,
                                outEnd=from@tEnd,
                                direction='out',
                                stringsAsFactors=FALSE)
          }

          return(cbind(df2, df1))
      } else {
          # No contacts, return a zero row data.frame
          return(data.frame(root=character(0),
                            inBegin=as.Date(character(0)),
                            inEnd=as.Date(character(0)),
                            outBegin=as.Date(character(0)),
                            outEnd=as.Date(character(0)),
                            direction=character(0),
                            source=character(0),
                            destination=character(0),
                            t=as.Date(character(0)),
                            id=character(0),
                            n=numeric(0),
                            category=character(0),
                            stringsAsFactors=FALSE))
      }
  }
)

