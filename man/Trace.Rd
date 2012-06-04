\name{Trace}
\alias{Trace}
\title{Trace Contacts.}
\usage{
  Trace(movements, root, tEnd, days)
}
\arguments{
  \item{movements}{a \code{\link{data.frame}} with contacts
  due to animal movements between holdings, see
  \code{\link{TraceDateInterval}} for details.}

  \item{root}{vector of roots to perform contact tracing
  on.}

  \item{tEnd}{the last date to include ingoing and outgoing
  movements}

  \item{days}{the number of previous days before tEnd to
  include ingoing and outgoing movements}
}
\description{
  Contact tracing for a specied node(s) (root) during a
  specfied time period. The time period is divided into two
  parts, one for ingoing contacts and one for outgoing
  contacts. For ingoing contacts the time period ends at
  inEndDate and the number of inDays is number of previous
  days. For outgoing contacts the time period starts at
  outStartDate and outDays is the number of days to include
  after that date.
}
\examples{
# Load data
data(transfers)

# Perform contact tracing
contactTrace <- Trace(movements=transfers,
                      root=2645,
                      tEnd='2005-10-31',
                      days=90)

# Show result of contact tracing
show(contactTrace)

\dontrun{
# Plot in- and outgoing contact chain
plot(contactTrace)

# Create a network summary for all included herds
# First extract all source and destination from the dataset
root <- sort(unique(c(transfers$source,
                      transfers$destination)))

# Perform contact tracing
contactTrace <- Trace(movements=transfers,
                      root=root,
                      tEnd='2005-10-31',
                      days=90)

NetworkSummary(contactTrace)
}
}
\references{
  \itemize{ \item Noremark, M., et al., Network analysis of
  cattle and pig movements in Sweden: Measures relevant for
  disease control and riskbased surveillance.  Preventive
  Veterinary Medicine 2011, doi:
  10.1016/j.prevetmed.2010.12.009 }
}
\seealso{
  \code{\link{TraceDateInterval}}.
}

