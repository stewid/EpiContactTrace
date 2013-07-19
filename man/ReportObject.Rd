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

\name{ReportObject}
\alias{ReportObject}
\title{Get current \code{ContactTrace} object when generating a report}
\usage{
  ReportObject()
}
\value{
  The current \code{ContactTrace} object when generating a
  report
}
\description{
  EpiContatTrace contains report templates to generate pdf-
  or html reports for the farm specific contacts. These
  reports can be useful for hands-on disease tracing in the
  field. The templates are used by Sweave and can be
  adapted by the end user. This method enables
  communication of the current \code{ContactTrace} object
  to the report.
}

