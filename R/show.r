##' Show
##'
##' Shows information of the time-window used for contact tracing and summary of
##' network parameters. It also visualize the contact structure.
##'
##'
##' @name show-methods
##' @aliases show show-methods show,Contacts-method show,ContactTrace-method
##' @docType methods
##' @section Methods: \describe{
##'
##'   \item{\code{signature(object = "Contacts")}}{
##'     Show information for the Contacts object.
##'   }
##'
##'   \item{\code{signature(object = "ContactTrace")}}{
##'     Show information for the ingoing and outgoing
##'     \code{Contacts} of a \code{ContactTrace} object.
##'   }
##' }
##' @seealso \code{\link{plot}}.
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
##' show(contactTrace)
##'
setMethod('show',
          signature(object = 'Contacts'),
          function (object)
      {
          arrow <- ifelse(identical(object@direction, 'in'), '<<<', '>>>')
          prefix <- ifelse(identical(object@direction, 'in'), 'In', 'Out')

          cat(sprintf('%s %s contacts %s\n', arrow, prefix, arrow))

          cat(sprintf('%s begin date: %s\n', prefix, object@tBegin))
          cat(sprintf('%s end date:   %s\n', prefix, object@tEnd))
          cat(sprintf('%s days: %i\n', prefix, object@tEnd - object@tBegin))

          if(identical(object@direction, 'out')) {
              cat(sprintf('%s degree: %s\n', prefix, out_degree(object)))
              cat(sprintf('%sgoing contact chain: %i\n\n', prefix, outgoing_contact_chain(object)))
          } else {
              cat(sprintf('%s degree: %s\n', prefix, in_degree(object)))
              cat(sprintf('%sgoing contact chain: %i\n\n', prefix, ingoing_contact_chain(object)))
          }

          if(length(object@source) > 0L) {
              arrow <- ifelse(identical(object@direction, 'out'), '-->', '<--')
              width <- max(nchar(object@source), nchar(object@destination))
              format <- sprintf('%%s%% %is %s %% %is\n', width, arrow, width)

              ## Get network structure. The distance is used for indentation.
              ns <- NetworkStructure(object)

              ## Rename source and destination to lhs and rhs, with respect to direction
              if(identical(object@direction, 'out')) {
                  names(ns)[names(ns) == 'source'] <- 'lhs'
                  names(ns)[names(ns) == 'destination'] <- 'rhs'
              } else {
                  names(ns)[names(ns) == 'source'] <- 'rhs'
                  names(ns)[names(ns) == 'destination'] <- 'lhs'
              }

              for(i in seq_len(nrow(ns))) {
                  cat(sprintf(format,
                              paste(rep(' ', (ns$distance[i] - 1) * (width + 5)), collapse=''),
                              ns$lhs[i],
                              ns$rhs[i]))
              }

          } else {
              cat(sprintf('No %sgoing contacts during the search period.\n', object@direction))
          }

          cat('\n')
      }
)

setMethod('show',
          signature(object = 'ContactTrace'),
          function (object)
      {
          cat(sprintf('Root: %s\n\n', object@root))
          show(object@ingoingContacts)
          show(object@outgoingContacts)
      }
)
