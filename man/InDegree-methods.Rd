% Copyright 2013 Stefan Widgren and Maria Noremark,
% National Veterinary Institute, Sweden
%
% Licensed under the EUPL, Version 1.1 or - as soon they
% will be approved by the European Commission - subsequent
% versions of the EUPL (the "Licence");
% You may not use this work except in compliance with the
% Licence.
% You may obtain a copy of the Licence at:
%
% http://ec.europa.eu/idabc/eupl
%
% Unless required by applicable law or agreed to in
% writing, software distributed under the Licence is
% distributed on an "AS IS" basis,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
% express or implied.
% See the Licence for the specific language governing
% permissions and limitations under the Licence.

\docType{methods}
\name{InDegree-methods}
\alias{InDegree}
\alias{InDegree,Contacts-method}
\alias{InDegree,ContactTrace-method}
\alias{InDegree,data.frame-method}
\alias{InDegree,list-method}
\alias{InDegree-methods}
\title{\code{InDegree}}
\arguments{
  \item{x}{a ContactTrace object, or a list of ContactTrace
  objects or a \code{data.frame} with movements of animals
  between holdings, see \code{\link{Trace}} for details.}

  \item{root}{vector of roots to calculate indegree for.}

  \item{tEnd}{the last date to include ingoing movements.
  Defaults to \code{NULL}}

  \item{days}{the number of previous days before tEnd to
  include ingoing movements. Defaults to \code{NULL}}

  \item{inBegin}{the first date to include ingoing
  movements. Defaults to \code{NULL}}

  \item{inEnd}{the last date to include ingoing movements.
  Defaults to \code{NULL}}
}
\value{
  A \code{data.frame} with the following columns:
  \describe{ \item{root}{ The root of the contact tracing }

  \item{inBegin}{ The first date to include ingoing
  movements }

  \item{inEnd}{ The last date to include ingoing movements
  }

  \item{inDays}{ The number of days in the interval inBegin
  to inEnd }

  \item{inDegree}{ The \code{\link{InDegree}} of the root
  within the time-interval } }
}
\description{
  The number of herds with direct movements of animals to
  the root herd during the defined time window used for
  tracing.
}
\details{
  The time period used for \code{InDegree} can either be
  specified using \code{tEnd} and \code{days} or
  \code{inBegin} and \code{inEnd}.

  If using \code{tEnd} and \code{days}, the time period for
  ingoing contacts ends at \code{tEnd} and starts at
  \code{days} prior to \code{tEnd}. The indegree will be
  calculated for each combination of \code{root},
  \code{tEnd} and \code{days}.

  An alternative way is to use \code{inBegin} and
  \code{inEnd}.  The time period for ingoing contacts
  starts at inBegin and ends at inEndDate. The vectors
  \code{root} \code{inBegin}, \code{inEnd} must have the
  same lengths and the indegree will be calculated for each
  index of them.

  The movements in \code{InDegree} is a \code{data.frame}
  with the following columns: \describe{

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
\section{Methods}{
  \describe{ \item{\code{signature(x = "ContactTrace")}}{
  Get the InDegree of a \code{ContactTrace} object.  }

  \item{\code{signature(x = "list")}}{ Get the InDegree for
  a list of \code{ContactTrace} objects.  Each item in the
  list must be a \code{ContactTrace} object.  }

  \item{\code{signature(x = "data.frame")}}{ Get the
  InDegree for a data.frame with movements, see details and
  examples.  } }
}
\examples{
## Load data
data(transfers)

## Perform contact tracing using tEnd and days
contactTrace <- Trace(movements=transfers,
                      root=2645,
                      tEnd='2005-10-31',
                      days=91)

## Calculate indegree from a ContactTrace object
id.1 <- InDegree(contactTrace)

## Calculate indegree using tEnd and days
id.2 <- InDegree(transfers,
                 root=2645,
                 tEnd='2005-10-31',
                 days=91)

## Check that the result is identical
identical(id.1, id.2)

\dontrun{
## Calculate indegree for all included herds
## First extract all source and destination from the dataset
root <- sort(unique(c(transfers$source,
                      transfers$destination)))

## Calculate indegree
result <- InDegree(transfers,
                   root=root,
                   tEnd='2005-10-31',
                   days=91)
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

