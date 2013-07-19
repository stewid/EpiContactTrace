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

\name{Animate}
\alias{Animate}
\title{Animate}
\usage{
  Animate(movements, coords, map,
    interval = c("all", "day", "week", "month", "quarter", "year"),
    outdir = getwd(), title = "Animation of contacts")
}
\arguments{
  \item{movements}{a \code{data.frame} data.frame with
  movements, see details.}

  \item{coords}{a \code{data.frame} data.frame with
  coordinates, see details.}

  \item{map}{a ggmap object to use as background map, see
  \code{\link[ggmap]{get_map}}.}

  \item{interval}{the time interval to aggregate movements
  in the animation. Can be any of 'all', 'day', 'week',
  'month', 'quarter' or 'year'. Defaults to 'all', which
  aggregates all movements on one map.}

  \item{outdir}{the output directory for the animation, see
  \code{\link[animation]{ani.options}}. Defaults to
  \code{getwd()}.}

  \item{title}{the title of the animation in the HTML, see
  \code{\link[animation]{ani.options}}. Defaults to
  'Animation of contacts'.}
}
\value{
  invisible(NULL)
}
\description{
  Visualize and animate movements on a map.
}
\details{
  The argument movements in Animate is a \code{data.frame}
  with the following columns: \describe{

  \item{source}{ an integer or character identifier of the
  source holding.  }

  \item{destination}{ an integer or character identifier of
  the destination holding.  }

  \item{t}{ the Date of the transfer } }

  The argument coords in Animate is a \code{data.frame}
  with the following columns: \describe{

  \item{id}{ an integer or character identifier of the
  holding.  }

  \item{lat}{ the latitude of holding.  }

  \item{lon}{ the longitude of the holding.  } }
}
\note{
  The packages \code{animation} and \code{ggmap} must be
  installed for this functionality.
}
\examples{
\dontrun{
require(ggmap)

data(transfers)

## First extract all source and destination from the dataset
root <- unique(c(transfers$source, transfers$destination))

## For this example, generate a random coordinate for each
## holding. Note that some coordinates might end up in water,
## but the coordinates are only for demonstrating the animation.
ngen <- length(root)
set.seed(123)
lon_min <- 13
lon_max <- 17
lat_min <- 56
lat_max <- 63

lon <- lon_min + runif(ngen) * (lon_max - lon_min)
lat <- lat_min + runif(ngen) * (lat_max - lat_min)
coords <- data.frame(id=root, lon, lat)

## Fetch a map over Sweden
sweden <- get_map('Sweden', zoom=5)

## Select a subset of all movements to visualize
i <- sample(seq_len(nrow(transfers)), 100, replace=FALSE)

## Perform the animation and view the movements aggregated by week
## in a web-browser.
Animate(transfers[i,], coords, sweden, "week")
}
}
\references{
  \itemize{ \item Yihui Xie (2013). animation: An R Package
  for Creating Animations and Demonstrating Statistical
  Methods. Journal of Statistical Software, 53(1), 1-27.
  URL http://www.jstatsoft.org/v53/i01/

  \item Kahle, D. and Wickham, H. Manual package 'ggmap' -
  A package for spatial visualization with Google Maps and
  OpenStreetMap.  URL
  http://cran.r-project.org/web/packages/ggmap/ggmap.pdf

  \item Widgren, S. and Frossling, J., Spatio-temporal
  evaluation of cattle trade in Sweden: description of a
  grid network visualization technique.  Geospatial Health
  5(1), 2010, pp 119-130.  URL
  http://www.geospatialhealth.unina.it/articles/v5i1/gh-v5i1-12-widgren.pdf
  }
}

