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
\name{plot-methods}
\alias{plot}
\alias{plot,Contacts-method}
\alias{plot,ContactTrace-method}
\alias{plot-methods}
\title{plot,-method}
\description{
  The contact structure can be visualized graphically with
  a plot. The plot gives an overview of the number of
  ingoing and outgoing holdings connected to the root
  holding. The black node is the root holding and all white
  nodes represent holdings that are direct or indirect
  holdings with ingoing contacts to root. Grey nodes
  represent holdings that are direct or indirect holdings
  with outgoing contacts from root.
}
\examples{
## Load data
data(transfers)

## Perform contact tracing
contactTrace <- Trace(movements=transfers,
                      root=2645,
                      tEnd='2005-10-31',
                      days=90)

\dontrun{
## Plot in- and outgoing contact chain for the root 2645
plot(contactTrace)
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
  \code{\link{show}}.
}

