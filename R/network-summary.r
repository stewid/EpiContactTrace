##' \code{NetworkSummary}
##'
##' \code{NetworkSummary} gives a summary of the contact tracing including the
##' time-window, \code{\link{InDegree}}, \code{\link{OutDegree}},
##' \code{\link{IngoingContactChain}} and \code{\link{OutgoingContactChain}}.
##'
##' @name NetworkSummary-methods
##' @aliases NetworkSummary
##' @aliases NetworkSummary-methods
##' @aliases NetworkSummary,ContactTrace-method
##' @aliases NetworkSummary,list-method
##' @aliases NetworkSummary,data.frame-method
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
##'
##' @section Methods:
##' \describe{
##'   \item{\code{signature(x = "ContactTrace")}}{
##'     Get the network summary for the ingoing and outgoing
##'     \code{Contacts} of a ContactTrace object.
##'   }
##'
##'   \item{\code{signature(x = "list")}}{
##'     Get the network summary for a list of \code{ContactTrace} objects.
##'     Each item in the list must be a \code{ContactTrace} object.
##'   }
##'
##'   \item{\code{signature(x = "data.frame")}}{
##'     Get the network summary for a data.frame with movements, see examples.
##'   }
##' }
##'
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
##' @import plyr
##' @export
##' @useDynLib EpiContactTrace
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
##' NetworkSummary(contactTrace)
##'
##' \dontrun{
##'
##' ## When calculating the network summary for a data.frame of movements
##' ## a data.frame for each combination of root, tEnd and days are returned.
##' root <- c(1,2,3)
##' tEnd <- c("2005-09-01", "2005-10-01")
##' days <- c(30, 45)
##'
##' ## The network summary are calculated at the following
##' ## 12 combinations.
##' ## root = 1, tEnd = "2005-09-01", days = 30
##' ## root = 1, tEnd = "2005-09-01", days = 45
##' ## root = 1, tEnd = "2005-10-01", days = 30
##' ## root = 1, tEnd = "2005-10-01", days = 45
##' ## root = 2, tEnd = "2005-09-01", days = 30
##' ## root = 2, tEnd = "2005-09-01", days = 45
##' ## root = 2, tEnd = "2005-10-01", days = 30
##' ## root = 2, tEnd = "2005-10-01", days = 45
##' ## root = 3, tEnd = "2005-09-01", days = 30
##' ## root = 3, tEnd = "2005-09-01", days = 45
##' ## root = 3, tEnd = "2005-10-01", days = 30
##' ## root = 3, tEnd = "2005-10-01", days = 45
##' NetworkSummary(transfers, root, tEnd, days)
##'
##' ## Create a network summary for all included herds
##' ## First extract all source and destination from the dataset
##' root <- sort(unique(c(transfers$source,
##'                       transfers$destination)))
##'
##' ## Perform contact tracing
##' result <- NetworkSumary(transfers,
##'                         root=root,
##'                         tEnd='2005-10-31',
##'                         days=90)
##' }
##'
setGeneric('NetworkSummary',
           signature = 'x',
           function(x, ...) standardGeneric('NetworkSummary'))

setMethod('NetworkSummary',
          signature(x = 'ContactTrace'),
          function(x)
      {
          data.frame(root=x@root,
                     inBegin=x@ingoingContacts@tBegin,
                     inEnd=x@ingoingContacts@tEnd,
                     inDays=x@ingoingContacts@tEnd - x@ingoingContacts@tBegin,
                     outBegin=x@outgoingContacts@tBegin,
                     outEnd=x@outgoingContacts@tEnd,
                     outDays=x@outgoingContacts@tEnd - x@outgoingContacts@tBegin,
                     inDegree=InDegree(x@ingoingContacts),
                     outDegree=OutDegree(x@outgoingContacts),
                     ingoingContactChain=IngoingContactChain(x@ingoingContacts),
                     outgoingContactChain=OutgoingContactChain(x@outgoingContacts))
      }
)

setMethod('NetworkSummary',
          signature(x = 'list'),
          function(x)
      {
          if(!all(sapply(x, function(y) length(y)) == 1)) {
              stop('Unexpected length of list')
          }

          if(!all(sapply(x, function(y) class(y)) == 'ContactTrace')) {
              stop('Unexpected object in list')
          }

          return(ldply(x, NetworkSummary)[,-1])
      }
)

setMethod('NetworkSummary',
          signature(x = 'data.frame'),
          function(x,
                   root,
                   tEnd,
                   days)
      {
           ## Check that arguments are ok from various perspectives...
          if(any(missing(x),
                 missing(root),
                 missing(tEnd),
                 missing(days))) {
              stop('Missing parameters in call to NetworkSummary')
          }

          if(!all(c('source', 'destination', 't') %in% names(x))) {
              stop('x must contain the columns source, destination and t.')
          }

          if(any(is.factor(x$source), is.integer(x$source))) {
              x$source <- as.character(x$source)
          } else if(!is.character(x$source)) {
              stop('invalid class of column source in x')
          }

          if(any(is.factor(x$destination), is.integer(x$destination))) {
              x$destination <- as.character(x$destination)
          } else if(!is.character(x$destination)) {
              stop('invalid class of column destination in x')
          }

          if(any(is.character(x$t), is.factor(x$t))) {
              x$t <- as.Date(x$t)
          }

          if(!identical(class(x$t), 'Date')) {
              stop('invalid class of column t in x')
          }

          if(any(is.na(x$t))) {
              stop("t in x contains NA")
          }

          ## Make sure the columns are in expected order and remove
          ## non-unique observations
          x <- unique(x[, c('source', 'destination', 't')])

          if(any(is.factor(root), is.integer(root))) {
              root <- as.character(root)
          } else if(is.numeric(root)) {
              ## root is supposed to be a character or integer identifier
              ## so test that root is a integer the same way as binom.test test x
              rootr <- round(root)
              if(any(max(abs(root - rootr) > 1e-07))) {
                  stop("'root' must be an integer or character")
              }

              root <- as.character(rootr)
          } else if(!is.character(root)) {
              stop('invalid class of root')
          }

          if(any(is.character(tEnd), is.factor(tEnd))) {
              tEnd <- as.Date(tEnd)
          }

          if(!identical(class(tEnd), 'Date')) {
              stop("'tEnd' must be a Date vector")
          }

          ## Test that days is a nonnegative integer the same way as binom.test test x
          daysr <- round(days)
          if (any(is.na(days) | (days < 0)) || max(abs(days - daysr)) > 1e-07) {
              stop("'days' must be nonnegative and integer")
          }
          days <- daysr

          ## Arguments seems ok...go on with calculations

          ## Make sure root, tEnd and days are unique
          root <- unique(root)
          tEnd <- unique(tEnd)
          days <- unique(days)

          n.root <- length(root)
          n.tEnd <- length(tEnd)
          n.days <- length(days)
          n <- n.root * n.tEnd * n.days

          root <- rep(root, each=n.tEnd*n.days, length.out=n)
          tEnd <- rep(tEnd, each=n.days, length.out=n)
          days <- rep(days, each=1, length.out=n)
          tBegin = tEnd - days

          ## Make sure all nodes have a valid variable name by making
          ## a factor of source and destination
          nodes <- as.factor(unique(c(x$source,
                                      x$destination,
                                      root)))

          ## Call networkSummary in EpiContactTrace.dll
          contact_chain<- .Call("networkSummary",
                                as.integer(factor(x$source, levels=levels(nodes))),
                                as.integer(factor(x$destination, levels=levels(nodes))),
                                as.integer(julian(x$t)),
                                as.integer(factor(root, levels=levels(nodes))),
                                as.integer(julian(tBegin)),
                                as.integer(julian(tEnd)),
                                length(nodes),
                                PACKAGE = "EpiContactTrace")

          return(data.frame(root=root,
                            inBegin=tBegin,
                            inEnd=tEnd,
                            inDays=days,
                            outBegin=tBegin,
                            outEnd=tEnd,
                            outDays=days,
                            inDegree=contact_chain[['inDegree']],
                            outDegree=contact_chain[['outDegree']],
                            ingoingContactChain=contact_chain[['ingoingContactChain']],
                            outgoingContactChain=contact_chain[['outgoingContactChain']]))
     }
)
