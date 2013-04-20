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

\docType{data}
\name{transfers}
\alias{transfers}
\title{Movement Example Data}
\format{A data frame with 70190 observations on the following 6 variables.
\describe{
  \item{source}{
    a numeric vector with the holding identifier of the source
  }

  \item{destination}{
    a numeric vector with holding identifier of the destination.
  }

  \item{id}{
    a character vector with the identity of the animal.
    In this dataset an 5 character hexadecimal vector.
  }

  \item{t}{
    a Date of the transfers
  }

  \item{n}{
    a numeric vector with the number of animals moved.
    Always 1 in this dataset.
  }

  \item{category}{
    a factor describing the category of the animal.
    Always \code{Cattle} in this dataset.
  }
}}
\description{
  Movement data included in the package. The data contains
  fictitious example data of cattle movements during the
  period 2005-08-01 -- 2005-10-31.
}
\examples{
data(transfers)

contactTrace <- Trace(movements=transfers,
                      root=2645,
                      tEnd='2005-10-31',
                      days=90)

\dontrun{
## Plot in- and outgoing contact chain
plot(contactTrace)

## Generate an html report.
## NOTE: creates the files '2645.html' and '2645.png' in the working directory.
Report(contactTrace)
}
}
\keyword{datasets}

