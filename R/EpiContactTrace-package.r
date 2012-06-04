##' Epidemiological tool for contact tracing.
##'
##' Routines for epidemiological contact tracing and visualisation of network
##' of contacts.
##'
##'
##' In many countries, livestock movement data are
##' collected with the major objective to enable contact tracing during disease
##' outbreaks. Livestock movement data can also be of relevance for risk based
##' surveillance - both during outbreak or when investigating if a disease is
##' present in the population. However, the livestock movement databases are
##' not always structured in such a way that relevant information for contact
##' tracing or surveillance design is easily retrieved. EpiContactTrace uses
##' the network parameters in-degree, out-degree, ingoing contact-chain and
##' outgoing contact-chain, which are relevant for forward- and backward
##' contact-tracing respectively. The measures can also be used for identifying
##' herds with many contacts, which can be used in risk based disease
##' surveillance. Different time periods for ingoing and outgoing contacts can
##' be of interest in the contact tracing, based on possible window of
##' introduction, and this can be adjusted in the tool. The output from the
##' analysis is available as a dataset, but moreover, the tool automatically
##' generates a report on farm level. The report both contains an overview of
##' the situation on the farm, including a graph, as well as detailed
##' information including dates of movements on group or individual level on
##' all contacts.
##'
##' @name EpiContactTrace-package
##' @aliases EpiContactTrace-package EpiContactTrace
##' @docType package
##' @author Stefan Widgren Maria Noremark
##' @section Maintainer:
##' Stefan Widgren <stefan.widgren@@sva.se>
##' @references \itemize{ \item Noremark, M., et al., Network analysis of
##' cattle and pig movements in Sweden: Measures relevant for disease control
##' and riskbased surveillance.  Preventive Veterinary Medicine 2011, doi:
##' 10.1016/j.prevetmed.2010.12.009 }
##' @keywords package
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
##' show(contactTrace)
##'
##' \dontrun{
##' # Generate an html report showing details of the contact tracing for
##' # root 2645.
##' # Note: Creates the files 2645.html and 2645.png in the working
##' # directory.
##' Report(contactTrace)
##' }
##'
NULL

##' Movements Example Data
##'
##' Movements data included in the package. The data contains fictitious
##' example data of cattle movements during the period 2005-08-01 -- 2005-10-31.
##'
##'
##' @name transfers
##' @docType data
##' @usage data(transfers)
##' @format A data frame with 70190 observations on the following 6 variables.
##' \describe{
##'   \item{\code{source}}{
##'     a numeric vector with the holding identifier of the source
##'   }
##'
##'   \item{\code{destination}}{
##'     a numeric vector with holding identifier of the destination.
##'   }
##'
##'   \item{\code{id}}{
##'     a character vector with the identity of the animal.
##'     In this dataset an 5 character hexadecimal vector.
##'   }
##'
##'   \item{\code{t}}{
##'     a Date of the transfers
##'   }
##'
##'   \item{\code{n}}{
##'     a numeric vector with the number of animals moved.
##'     Always 1 in this dataset.
##'   }
##'
##'   \item{\code{category}}{
##'     a factor with levels \code{Cattle}
##'   }
##' }
##' @keywords datasets
##' @examples
##'
##' data(transfers)
##'
##' contactTrace <- Trace(movements=transfers,
##'                       root=2645,
##'                       tEnd='2005-10-31',
##'                       days=90)
##'
##' \dontrun{
##' # Plot in- and outgoing contact chain
##' plot(contactTrace)
##'
##' # Generate an html report.
##' # NOTE: creates the files '2645.html' and '2645.png' in the working directory.
##' Report(contactTrace)
##' }
##'
NULL
