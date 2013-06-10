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
\name{NetworkSummary-methods}
\alias{NetworkSummary}
\alias{NetworkSummary,ContactTrace-method}
\alias{NetworkSummary,data.frame-method}
\alias{NetworkSummary,list-method}
\alias{NetworkSummary-methods}
\title{\code{NetworkSummary}}
\value{
  A \code{data.frame} with the following columns:
  \describe{ \item{root}{ The root of the contact tracing }

  \item{inBegin}{ Equals inBegin in \code{\link{Trace}} }

  \item{inEnd}{ Equals inEnd in \code{\link{Trace}} }

  \item{outBegin}{ Equals outBegin in \code{\link{Trace}} }

  \item{outEnd}{ Equals outEnd in \code{\link{Trace}} }

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
  \describe{ \item{\code{signature(x = "ContactTrace")}}{
  Get the network summary for the ingoing and outgoing
  \code{Contacts} of a ContactTrace object.  }

  \item{\code{signature(x = "list")}}{ Get the network
  summary for a list of \code{ContactTrace} objects.  Each
  item in the list must be a \code{ContactTrace} object.  }

  \item{\code{signature(x = "data.frame")}}{ Get the
  network summary for a data.frame with movements, see
  examples.  } }
}
\examples{
## Load data
data(transfers)

## Perform contact tracing
contactTrace <- Trace(movements=transfers,
                      root=2645,
                      tEnd='2005-10-31',
                      days=90)

NetworkSummary(contactTrace)

\dontrun{

## When calculating the network summary for a data.frame of movements
## a data.frame for each combination of root, tEnd and days are returned.
root <- c(1,2,3)
tEnd <- c("2005-09-01", "2005-10-01")
days <- c(30, 45)

## The network summary are calculated at the following
## 12 combinations.
## root = 1, tEnd = "2005-09-01", days = 30
## root = 1, tEnd = "2005-09-01", days = 45
## root = 1, tEnd = "2005-10-01", days = 30
## root = 1, tEnd = "2005-10-01", days = 45
## root = 2, tEnd = "2005-09-01", days = 30
## root = 2, tEnd = "2005-09-01", days = 45
## root = 2, tEnd = "2005-10-01", days = 30
## root = 2, tEnd = "2005-10-01", days = 45
## root = 3, tEnd = "2005-09-01", days = 30
## root = 3, tEnd = "2005-09-01", days = 45
## root = 3, tEnd = "2005-10-01", days = 30
## root = 3, tEnd = "2005-10-01", days = 45
NetworkSummary(transfers, root, tEnd, days)

## Create a network summary for all included herds
## First extract all source and destination from the dataset
root <- sort(unique(c(transfers$source,
                      transfers$destination)))

## Perform contact tracing using tEnd and days
result.1 <- NetworkSummary(transfers,
                           root=root,
                           tEnd='2005-10-31',
                           days=90)

## Perform contact tracing using inBegin, inEnd, outBegin and outEnd.
result.2 <- NetworkSummary(transfers,
                           root=root,
                           inBegin=rep('2005-08-02', length(root)),
                           inEnd=rep('2005-10-31', length(root)),
                           outBegin=rep('2005-08-02', length(root)),
                           outEnd=rep('2005-10-31', length(root)))
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

