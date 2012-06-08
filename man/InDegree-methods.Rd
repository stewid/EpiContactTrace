\docType{methods}
\name{InDegree-methods}
\alias{InDegree}
\alias{InDegree-methods}
\alias{InDegree,Contacts-method}
\alias{InDegree,ContactTrace-method}
\alias{InDegree,list-method}
\title{\code{InDegree}}
\value{
  An integer vector.
}
\description{
  The number of herds with direct movements of animals to
  the root herd during the defined time window used for
  tracing.
}
\section{Methods}{
  \describe{ \item{\code{signature(object = "Contacts")}}{
  Get the InDegree of a \code{Contacts} object with ingoing
  direction.  }

  \item{\code{signature(object = "ContactTrace")}}{ Get the
  InDegree of a \code{ContactTrace} object.  }

  \item{\code{signature(object = "list")}}{ Get the
  InDegree for a list of \code{ContactTrace} objects.  Each
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

InDegree(contactTrace)

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

InDegree(contactTrace)
}
}
\references{
  \itemize{ \item Dube, C., et al., A review of network
  analysis terminology and its application to
  foot-and-mouth disease modelling and policy development.
  Transbound Emerg Dis 56 (2009) 73-85, doi:
  10.1111/j.1865-1682.2008.01064.x

  \item Noremark, M., et al., Network analysis of cattle
  and pig movements in Sweden: Measures relevant for
  disease control and riskbased surveillance.  Preventive
  Veterinary Medicine 99 (2011) 78-90, doi:
  10.1016/j.prevetmed.2010.12.009 }
}
\keyword{methods}

