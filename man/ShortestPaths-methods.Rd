\docType{methods}
\name{ShortestPaths-methods}
\alias{ShortestPaths}
\alias{ShortestPaths-methods}
\alias{ShortestPaths,Contacts-method}
\alias{ShortestPaths,ContactTrace-method}
\alias{ShortestPaths,list-method}
\title{\code{ShortestPaths}}
\value{
  A \code{\link{data.frame}} with the following columns:
  \describe{ \item{root}{ The root of the contact tracing }

  \item{inBegin}{ If the direction is ingoing, then inBegin
  equals inBegin in \code{\link{TraceDateInterval}} else
  NA.  }

  \item{inEnd}{ If the direction is ingoing, then inEnd
  equals inEnd in \code{\link{TraceDateInterval}} else NA.
  }

  \item{outBegin}{ If the direction is outgoing, then
  outBegin equals outBegin in
  \code{\link{TraceDateInterval}} else NA.  }

  \item{outEnd}{ If the direction is outgoing, then outEnd
  equals outEnd in \code{\link{TraceDateInterval}} else NA.
  }

  \item{direction}{ If the direction is ingoing, then
  direction equals 'in' else 'out' }

  \item{source}{ The source of the contacts in the depth
  first search }

  \item{destination}{ The destination of the contacts in
  the depth first search }

  \item{distance}{ The shortest distance from/to root in
  the depth first search } }
}
\description{
  Methods for function \code{ShortestPaths} in package
  \pkg{EpiContactTrace} to get the shortest distance
  from/to the root given by the contact tracing.
}
\details{
  The contact tracing performs a depth first search
  starting at the root. The \code{ShortestPaths} gives the
  shortest distance from root at each node. The network
  tree structure given by the depth first search is shown
  by \code{\link{show}}.
}
\section{Methods}{
  \describe{

  \item{\code{signature(object = "Contacts")}}{ Get the
  shortest paths for the Contacts object.  }

  \item{\code{signature(object = "ContactTrace")}}{ Get the
  shortest paths for the ingoing and outgoing
  \code{Contacts} of a \code{ContactTrace} object.  }

  \item{\code{signature(object = "list")}}{ Get the
  shortest paths for a list of \code{ContactTrace} objects.
  Each item in the list must be a \code{ContactTrace}
  object.  } }
}
\examples{
# Load data
data(transfers)

# Perform contact tracing
contactTrace <- Trace(movements=transfers,
                      root=2645,
                      tEnd='2005-10-31',
                      days=90)

ShortestPaths(contactTrace)
}
\seealso{
  \code{\link{show}} and \code{\link{NetworkStructure}}.
}
\keyword{methods}

