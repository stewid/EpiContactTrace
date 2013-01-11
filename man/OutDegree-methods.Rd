\docType{methods}
\name{OutDegree-methods}
\alias{OutDegree}
\alias{OutDegree-methods}
\alias{OutDegree,Contacts-method}
\alias{OutDegree,ContactTrace-method}
\alias{OutDegree,data.frame-method}
\alias{OutDegree,list-method}
\title{\code{OutDegree}}
\arguments{
  \item{x}{a ContactTrace object, or a list of ContactTrace
  objects or a \code{data.frame} with movements of animals
  between holdings, see \code{\link{TraceDateInterval}} for
  details.}

  \item{root}{vector of roots to perform contact tracing
  on.}

  \item{tEnd}{the last date to include outgoing movements}

  \item{days}{the number of previous days before tEnd to
  include outgoing movements}
}
\value{
  A \code{data.frame} with the following columns:
  \describe{ \item{root}{ The root of the contact tracing }

  \item{outBegin}{ The first date to include outgoing
  movements }

  \item{outEnd}{ The last date to include outgoing
  movements }

  \item{outDays}{ The number of days in the interval
  outBegin to outEnd }

  \item{outDegree}{ The \code{\link{OutDegree}} of the root
  within the time-interval } }
}
\description{
  The number of herds with direct movements of animals from
  the root herd during the defined time window used for
  tracing
}
\section{Methods}{
  \describe{ \item{\code{signature(x = "ContactTrace")}}{
  Get the OutDegree of a \code{ContactTrace} object.  }

  \item{\code{signature(x = "list")}}{ Get the OutDegree
  for a list of \code{ContactTrace} objects.  Each item in
  the list must be a \code{ContactTrace} object.  }

  \item{\code{signature(x = "data.frame")}}{ Get the
  OutDegree for a data.frame with movements, see examples.
  } }
}
\examples{
## Load data
data(transfers)

## Perform contact tracing
contactTrace <- Trace(movements=transfers,
                      root=2645,
                      tEnd='2005-10-31',
                      days=90)

OutDegree(contactTrace)

\dontrun{
## Perform contact tracing for all included herds
## First extract all source and destination from the dataset
root <- sort(unique(c(transfers$source,
                      transfers$destination)))

## Perform contact tracing
result <- OutDegree(transfers,
                    root=root,
                    tEnd='2005-10-31',
                    days=90)
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
\seealso{
  \code{\link{NetworkSummary}}
}
\keyword{methods}

