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

\docType{class}
\name{ContactTrace-class}
\alias{ContactTrace-class}
\title{Class \code{"ContactTrace"}}
\description{
  Class to handle contact tracing.
}
\details{
  The \code{ContactTrace} class holds information for the
  ingoing and outgoing contact chain for a specific root
  within the time window used for contact tracing.
}
\section{Slots}{
  \describe{ \item{root}{ A \code{character} vector of
  length one with the identifier of the root.  }
  \item{ingoingContacts}{ A \code{Contacts} object with the
  contacts for the ingoing contact chain.  }
  \item{outgoingContacts}{ A \code{Contacts} object with
  the contacts for the outgoing contact chain.  } }
}

\section{Objects from the Class}{
  Objects can be created by calls of the form
  \code{new("ContactTrace",root, ingoingContacts,
  outgoingContacts,...)}
}
\examples{
## Load data
data(transfers)

## Perform contact tracing
contactTrace <- Trace(movements = transfers,
                      root = 2645,
                      tEnd = '2005-10-31',
                      days = 90)

## Show structure
str(contactTrace)
}
\keyword{classes}

