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

##' \code{IngoingContactChain}
##'
##' The ingoing contact chain is the number of holdings in the network of
##' direct and indirect contacts to the root holding, with regard to temporal
##' and order of the contacts during the defined time window used for contact tracing.
##'
##'
##' @name IngoingContactChain-methods
##' @aliases IngoingContactChain
##' @aliases IngoingContactChain-methods
##' @aliases IngoingContactChain,Contacts-method
##' @aliases IngoingContactChain,ContactTrace-method
##' @aliases IngoingContactChain,list-method
##' @aliases IngoingContactChain,data.frame-method
##' @docType methods
##' @seealso \code{\link{NetworkSummary}}
##' @param x a ContactTrace object, or a list of ContactTrace objects
##' or a \code{data.frame} with movements of animals between holdings,
##' see \code{\link{TraceDateInterval}} for details.
##' @param root vector of roots to perform contact tracing on.
##' @param tEnd the last date to include ingoing movements
##' @param days the number of previous days before tEnd to include
##' ingoing movements
##' @return A \code{data.frame} with the following columns:
##' \describe{
##'   \item{root}{
##'     The root of the contact tracing
##'   }
##'
##'   \item{inBegin}{
##'     The first date to include ingoing movements
##'   }
##'
##'   \item{inEnd}{
##'     The last date to include ingoing movements
##'   }
##'
##'   \item{inDays}{
##'     The number of days in the interval inBegin to inEnd
##'   }
##'
##'   \item{ingoingContactChain}{
##'     The \code{\link{IngoingContactChain}} of the root within the time-interval
##'   }
##' }
##'
##' @section Methods:
##' \describe{
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
##'     Get the IngoingContactChain for a data.frame with movements, see examples.
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
##' result <- IngoingContactChain(transfers,
##'                               root=root,
##'                               tEnd='2005-10-31',
##'                               days=90)
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
          return(NetworkSummary(x)[, c('root',
                                       'inBegin',
                                       'inEnd',
                                       'inDays',
                                       'ingoingContactChain')])
      }
)

setMethod('IngoingContactChain',
          signature(x = 'list'),
          function(x)
      {
          return(NetworkSummary(x)[, c('root',
                                       'inBegin',
                                       'inEnd',
                                       'inDays',
                                       'ingoingContactChain')])
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

