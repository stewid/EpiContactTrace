\name{TraceDateInterval}
\alias{TraceDateInterval}
\title{Trace Contacts.}
\usage{
  TraceDateInterval(movements, root, inBegin, inEnd,
    outBegin, outEnd)
}
\arguments{
  \item{movements}{a \code{data.frame} data.frame with
  movements, see details.}

  \item{root}{vector of roots to perform contact tracing
  for.}

  \item{inBegin}{the last date to include ingoing
  movements}

  \item{inEnd}{the number of previous days before inEndDate
  to include ingoing movements}

  \item{outBegin}{the first date to include outgoing
  movements}

  \item{outEnd}{the number of days from endStartDate to
  include outgoing movements}
}
\description{
  Contact tracing for a specied node(s) (root) during a
  specfied time period. The time period is divided into two
  parts, one for ingoing contacts and one for outgoing
  contacts. For ingoing contacts the time period starts at
  inBegin and ends at inEndDate.  For outgoing contacts the
  time period starts at outBegin and ends at outEnd.
}
\details{
  The argument movements in TraceDateInterval is a
  \code{data.frame} with the following columns: \describe{

  \item{source}{ an integer or character identifier of the
  source holding.  }

  \item{destination}{ an integer or character identifier of
  the destination holding.  }

  \item{t}{ the Date of the transfer }

  \item{id}{ an optional character vector with the identity
  of the animal.  }

  \item{n}{ an optional numeric vector with the number of
  animals moved.  }

  \item{category}{ an optional character or factor with
  category of the animal e.g. Cattle.  } }
}
\examples{
## Load data
data(transfers)

## Perform contact tracing
contactTrace <- TraceDateInterval(movements=transfers,
                                  root=2645,
                                  inBegin='2005-08-01',
                                  inEnd='2005-10-31',
                                  outBegin='2005-08-01',
                                  outEnd='2005-10-31')

## Show result of contact tracing
show(contactTrace)

\dontrun{
## Plot in- and outgoing contact chain
plot(contactTrace)

## Create a network summary for all included herds
## First extract all source and destination from the dataset
root <- sort(unique(c(transfers$source,
                      transfers$destination)))

## Perform contact tracing
contactTrace <- TraceDateInterval(movements=transfers,
                                  root=root,
                                  inBegin='2005-08-01',
                                  inEnd='2005-10-31',
                                  outBegin='2005-08-01',
                                  outEnd='2005-10-31')

NetworkSummary(contactTrace)
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
  \code{\link{Trace}}.
}

