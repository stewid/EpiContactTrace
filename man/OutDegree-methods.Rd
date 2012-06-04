\docType{methods}
\name{OutDegree-methods}
\alias{OutDegree}
\alias{OutDegree-methods}
\alias{OutDegree,Contacts-method}
\alias{OutDegree,ContactTrace-method}
\alias{OutDegree,list-method}
\title{\code{OutDegree}}
\value{
  An integer vector.
}
\description{
  The number of herds with direct movements of animals from
  the root herd during the defined time window used for
  tracing
}
\section{Methods}{
  \describe{ \item{\code{signature(object = "Contacts")}}{
  Get the OutDegree of a \code{Contacts} object with
  outgoing direction.  }

  \item{\code{signature(object = "ContactTrace")}}{ Get the
  OutDegree of a \code{ContactTrace} object.  }

  \item{\code{signature(object = "list")}}{ Get the
  OutDegree for a list of \code{ContactTrace} objects.
  Each item in the list must be a \code{ContactTrace}
  object.  } }
}
\examples{
# Load data
data(transfers)

# Perform contact tracing
contactTrace <- Trace(movements=transfers,
                      root=2645,
                      tEnd='2005-10-31',
                      days=90)

OutDegree(contactTrace)

\dontrun{
# Perform contact tracing for all included herds
# First extract all source and destination from the dataset
root <- sort(unique(c(transfers$source,
                      transfers$destination)))

# Perform contact tracing
contactTrace <- Trace(movements=transfers,
                      root=root,
                      tEnd='2005-10-31',
                      days=90)

OutDegree(contactTrace)
}
}
\keyword{methods}

