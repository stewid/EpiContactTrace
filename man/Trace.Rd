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

\name{Trace}
\alias{Trace}
\title{Trace Contacts.}
\usage{
  Trace(movements, root, tEnd, days)
}
\arguments{
  \item{movements}{a \code{data.frame} with contacts due to
  animal movements between holdings, see
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
## Load data
data(transfers)

## Perform contact tracing
contactTrace <- Trace(movements=transfers,
                      root=2645,
                      tEnd='2005-10-31',
                      days=90)

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
contactTrace <- Trace(movements=transfers,
                      root=root,
                      tEnd='2005-10-31',
                      days=90)

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
  \code{\link{TraceDateInterval}}.
}

