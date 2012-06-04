\docType{methods}
\name{Report-methods}
\alias{Report}
\alias{Report-methods}
\alias{Report,ContactTrace-method}
\alias{Report,list-method}
\title{Generate a contact tracing \code{Report}}
\description{
  EpiContatTrace contains report templates to generate pdf-
  or html reports for the farm specific contacts. These
  reports can be useful for hands-on disease tracing in the
  field. The templates are used by Sweave and can be
  adapted by the end user. However, in the default setting
  the report has the following layout; first the contacts
  are visualised graphically in a plot, as to give an
  immediate signal to the reader of the report of the
  number of contacts. In the following, the contact data
  are presented with different levels of detail split by
  ingoing and outgoing contacts. The first includes
  collapsed data and the sequential contact structure at
  group level (i.e. no information on individuals or
  dates). In this summary, the sequential structure of each
  part of the chain is included, and a holding that appears
  in several different parts of the chain can therefore be
  included more than once in the summary. The reason for
  this is to facilitate sequential tracing and getting an
  overview of each part of the chain. After the summary all
  details of all contacts included in the contact chains is
  presented, i.e. date of contact and data on individual
  level when available.
}
\section{Methods}{
  \describe{

  \item{\code{signature(object = "ContactTrace")}}{ %%
  ~~describe this method here~~ }

  \item{\code{signature(object = "list")}}{ %% ~~describe
  this method here~~ } }
}
\examples{
# Load data
data(transfers)

# Perform contact tracing
contactTrace <- Trace(movements=transfers,
                      root=2645,
                      tEnd='2005-10-31',
                      days=90)

\dontrun{
# Generate an html report showing details of the contact tracing for
# root 2646.
# Note: Creates the files 2645.html and 2645.png in the working
# directory.
Report(contactTrace)

# It's possible to generate reports for a list of ContactTrace objects.
# Perform contact tracing for ten of the included herds
root <- sort(unique(c(transfers$source, transfers$destination)))[1:10]

# Perform contact tracing
contactTrace <- Trace(movements=transfers,
                      root=root,
                      tEnd='2005-10-31',
                      days=90)

# Generate reports
# Note: Creates the files 1.html, 2.html, ..., 10.html and
# 1.png, 2.png, ..., 10.png in the working directory
Report(contactTrace)
}
}
\keyword{methods}

