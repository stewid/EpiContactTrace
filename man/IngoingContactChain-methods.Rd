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
\name{IngoingContactChain-methods}
\alias{IngoingContactChain}
\alias{IngoingContactChain,Contacts-method}
\alias{IngoingContactChain,ContactTrace-method}
\alias{IngoingContactChain,data.frame-method}
\alias{IngoingContactChain,list-method}
\alias{IngoingContactChain-methods}
\title{\code{IngoingContactChain}}
\arguments{
  \item{x}{a ContactTrace object, or a list of ContactTrace
  objects or a \code{data.frame} with movements of animals
  between holdings, see \code{\link{TraceDateInterval}} for
  details.}

  \item{root}{vector of roots to perform contact tracing
  on.}

  \item{tEnd}{the last date to include ingoing movements}

  \item{days}{the number of previous days before tEnd to
  include ingoing movements}
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

  \item{ingoingContactChain}{ The
  \code{\link{IngoingContactChain}} of the root within the
  time-interval } }
}
\description{
  The ingoing contact chain is the number of holdings in
  the network of direct and indirect contacts to the root
  holding, with regard to temporal and order of the
  contacts during the defined time window used for contact
  tracing.
}
\section{Methods}{
  \describe{ \item{\code{signature(x = "ContactTrace")}}{
  Get the IngoingContactChain of a \code{ContactTrace}
  object.  }

  \item{\code{signature(x = "list")}}{ Get the
  IngoingContactChain for a list of \code{ContactTrace}
  objects.  Each item in the list must be a
  \code{ContactTrace} object.  }

  \item{\code{signature(x = "data.frame")}}{ Get the
  IngoingContactChain for a data.frame with movements, see
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

IngoingContactChain(contactTrace)

\dontrun{
## Perform contact tracing for all included herds
## First extract all source and destination from the dataset
root <- sort(unique(c(transfers$source,
                      transfers$destination)))

## Perform contact tracing
result <- IngoingContactChain(transfers,
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

