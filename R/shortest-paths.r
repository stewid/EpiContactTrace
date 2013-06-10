## Copyright 2013 Stefan Widgren and Maria Noremark,
## National Veterinary Institute, Sweden
##
## Licensed under the EUPL, Version 1.1 or - as soon they
## will be approved by the European Commission - subsequent
## versions of the EUPL (the "Licence");
## You may not use this work except in compliance with the
## Licence.
## You may obtain a copy of the Licence at:
##
## http://ec.europa.eu/idabc/eupl
##
## Unless required by applicable law or agreed to in
## writing, software distributed under the Licence is
## distributed on an "AS IS" basis,
## WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
## express or implied.
## See the Licence for the specific language governing
## permissions and limitations under the Licence.

##' \code{ShortestPaths}
##'
##' Methods for function \code{ShortestPaths} in package \pkg{EpiContactTrace}
##' to get the shortest distance from/to the root given by the contact tracing.
##'
##' The contact tracing performs a depth first search starting at the root. The
##' \code{ShortestPaths} gives the shortest distance from root at each node.
##' The network tree structure given by the depth first search is shown by
##' \code{\link{show}}.
##'
##' @name ShortestPaths-methods
##' @aliases ShortestPaths ShortestPaths-methods ShortestPaths,Contacts-method
##' ShortestPaths,ContactTrace-method ShortestPaths,list-method
##' @docType methods
##' @return A \code{data.frame} with the following columns:
##' \describe{
##'   \item{root}{
##'     The root of the contact tracing
##'   }
##'
##'   \item{inBegin}{
##'     If the direction is ingoing, then inBegin equals inBegin in
##'     \code{\link{Trace}} else NA.
##'   }
##'
##'   \item{inEnd}{
##'     If the direction is ingoing, then inEnd equals inEnd in
##'     \code{\link{Trace}} else NA.
##'   }
##'
##'   \item{outBegin}{
##'     If the direction is outgoing, then outBegin equals outBegin in
##'     \code{\link{Trace}} else NA.
##'   }
##'
##'   \item{outEnd}{
##'     If the direction is outgoing, then outEnd equals outEnd in
##'     \code{\link{Trace}} else NA.
##'   }
##'
##'   \item{direction}{
##'     If the direction is ingoing, then direction equals 'in' else 'out'
##'   }
##'
##'   \item{source}{
##'     The source of the contacts in the depth first search
##'   }
##'
##'   \item{destination}{
##'     The destination of the contacts in the depth first search
##'   }
##'
##'   \item{distance}{
##'     The shortest distance from/to root in the depth first search
##'   }
##' }
##' @section Methods: \describe{
##'
##'   \item{\code{signature(object = "Contacts")}}{
##'     Get the shortest paths for the Contacts object.
##'   }
##'
##'   \item{\code{signature(object = "ContactTrace")}}{
##'     Get the shortest paths for the ingoing and outgoing
##'     \code{Contacts} of a \code{ContactTrace} object.
##'   }
##'
##'   \item{\code{signature(object = "list")}}{
##'     Get the shortest paths for a list of \code{ContactTrace} objects.
##'     Each item in the list must be a \code{ContactTrace} object.
##'   }
##' }
##' @seealso \code{\link{show}} and \code{\link{NetworkStructure}}.
##' @keywords methods
##' @import plyr
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
##' ShortestPaths(contactTrace)
##'
setGeneric('ShortestPaths',
           signature = 'object',
           function(object) standardGeneric('ShortestPaths'))

setMethod('ShortestPaths',
          signature(object = 'Contacts'),
          function(object)
      {
          if(identical(object@direction, 'in')) {
              ## Loop over each source and calculate minimum distance
              result <- ddply(NetworkStructure(object),c('source'), function(x) {
                  data.frame(root=object@root,
                             inBegin=object@tBegin,
                             inEnd=object@tEnd,
                             outBegin=as.Date(as.character(NA)),
                             outEnd=as.Date(as.character(NA)),
                             direction='in',
                             distance=min(x$distance),
                             destination=as.character(NA),
                             stringsAsFactors=FALSE)
              })
          } else {
              ## Loop over each destination and calculate minimum distance
              result <- ddply(NetworkStructure(object),c('destination'), function(x) {
                  data.frame(root=object@root,
                             inBegin=as.Date(as.character(NA)),
                             inEnd=as.Date(as.character(NA)),
                             outBegin=object@tBegin,
                             outEnd=object@tEnd,
                             direction='out',
                             distance=min(x$distance),
                             source=as.character(NA),
                             stringsAsFactors=FALSE)
              })
          }

          return(result[, c('root',
                            'inBegin',
                            'inEnd',
                            'outBegin',
                            'outEnd',
                            'direction',
                            'source',
                            'destination',
                            'distance')])
      }
 )

setMethod('ShortestPaths',
          signature(object = 'ContactTrace'),
          function(object)
      {
          return(rbind(ShortestPaths(object@ingoingContacts),
                       ShortestPaths(object@outgoingContacts)))
      }
)

setMethod('ShortestPaths',
          signature(object = 'list'),
          function(object)
      {
          if(!all(sapply(object, function(x) length(x)) == 1))
              stop('Unexpected length of list')

          if(!all(sapply(object, function(x) class(x)) == 'ContactTrace'))
              stop('Unexpected object in list')

          return(ldply(object, NetworkStructure)[,-1])
      }
)

