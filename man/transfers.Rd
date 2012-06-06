\docType{data}
\name{transfers}
\alias{transfers}
\title{Movements Example Data}
\format{A data frame with 70190 observations on the following 6 variables.
\describe{
  \item{\code{source}}{
    a numeric vector with the holding identifier of the source
  }

  \item{\code{destination}}{
    a numeric vector with holding identifier of the destination.
  }

  \item{\code{id}}{
    a character vector with the identity of the animal.
    In this dataset an 5 character hexadecimal vector.
  }

  \item{\code{t}}{
    a Date of the transfers
  }

  \item{\code{n}}{
    a numeric vector with the number of animals moved.
    Always 1 in this dataset.
  }

  \item{\code{category}}{
    a factor with levels \code{Cattle}
  }
}}
\description{
  Movements data included in the package. The data contains
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
# Plot in- and outgoing contact chain
plot(contactTrace)

# Generate an html report.
# NOTE: creates the files '2645.html' and '2645.png' in the working directory.
Report(contactTrace)
}
}
\keyword{datasets}

