\docType{methods}
\name{NetworkSummary-methods}
\alias{NetworkSummary}
\alias{NetworkSummary-methods}
\alias{NetworkSummary,ContactTrace-method}
\alias{NetworkSummary,list-method}
\title{\code{NetworkSummary}}
\value{
  A \code{\link{data.frame}} with the following columns:
  \describe{ \item{root}{ The root of the contact tracing }

  \item{inBegin}{ Equals inBegin in
  \code{\link{TraceDateInterval}} }

  \item{inEnd}{ Equals inEnd in
  \code{\link{TraceDateInterval}} }

  \item{outBegin}{ Equals outBegin in
  \code{\link{TraceDateInterval}} }

  \item{outEnd}{ Equals outEnd in
  \code{\link{TraceDateInterval}} }

  \item{inDegree}{ The \code{\link{InDegree}} of the
  contact tracing }

  \item{outDegree}{ The \code{\link{OutDegree}} of the
  contact tracing }

  \item{ingoingContactChain}{ The
  \code{\link{IngoingContactChain}} of the contact tracing
  }

  \item{outgoingContactChain}{ The
  \code{\link{OutgoingContactChain}} of the contact tracing
  } }
}
\description{
  \code{NetworkSummary} gives a summary of the contact
  tracing including the time-window,
  \code{\link{InDegree}}, \code{\link{OutDegree}},
  \code{\link{IngoingContactChain}} and
  \code{\link{OutgoingContactChain}}.
}
\section{Methods}{
  \describe{ \item{\code{signature(object =
  "ContactTrace")}}{ Get the network summary for the
  ingoing and outgoing \code{Contacts} of a ContactTrace
  object.  }

  \item{\code{signature(object = "list")}}{ Get the network
  summary for a list of \code{ContactTrace} objects.  Each
  item in the list must be a \code{ContactTrace} object.  }
  }
}
\examples{
# Load data
data(transfers)

# Perform contact tracing
contactTrace <- Trace(movements=transfers,
                      root=2645,
                      tEnd='2005-10-31',
                      days=90)

NetworkSummary(contactTrace)

\dontrun{
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
\keyword{methods}

