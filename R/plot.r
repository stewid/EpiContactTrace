## Copyright 2013 Stefan Widgren and Maria Noremark,
## National Veterinary Institute, Sweden
##
## Licensed under the EUPL, Version 1.1 or - as soon they
## will be approved by the European Commission - subsequent
## versions of the EUPL (the "Licence");
## You may not use this work except in compliance with the
## Licence.
## You may obtain a copy of the Licence at:
##
## http://ec.europa.eu/idabc/eupl
##
## Unless required by applicable law or agreed to in
## writing, software distributed under the Licence is
## distributed on an "AS IS" basis,
## WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
## express or implied.
## See the Licence for the specific language governing
## permissions and limitations under the Licence.

##' plot,-method
##'
##' The contact structure can be visualized graphically with a plot. The plot
##' gives an overview of the number of ingoing and outgoing holdings connected
##' to the root holding. The black node is the root holding and all white nodes
##' represent holdings that are direct or indirect holdings with ingoing
##' contacts to root. Grey nodes represent holdings that are direct or indirect
##' holdings with outgoing contacts from root.
##'
##'
##' @name plot-methods
##' @aliases plot plot-methods plot,Contacts-method plot,ContactTrace-method
##' @docType methods
##' @seealso \code{\link{show}}.
##' @references \itemize{
##'   \item Dube, C., et al., A review of network analysis terminology
##'     and its application to foot-and-mouth disease modelling and policy
##'     development. Transbound Emerg Dis 56 (2009) 73-85, doi:
##'     10.1111/j.1865-1682.2008.01064.x
##'
##'   \item Noremark, M., et al., Network analysis
##'     of cattle and pig movements in Sweden: Measures relevant for
##'     disease control and riskbased surveillance.  Preventive Veterinary
##'     Medicine 99 (2011) 78-90, doi: 10.1016/j.prevetmed.2010.12.009
##' }
##' @import igraph0
##' @importFrom graphics plot
##' @export
##' @examples
##'
##' ## Load data
##' data(transfers)
##'
##' ## Perform contact tracing
##' contactTrace <- Trace(movements=transfers,
##'                       root=2645,
##'                       tEnd='2005-10-31',
##'                       days=90)
##'
##' \dontrun{
##' ## Plot in- and outgoing contact chain for the root 2645
##' plot(contactTrace)
##' }
##'
setMethod('plot',
  signature(x = 'Contacts'),
  function(x,
           vertex.size=8,
           edge.color="black",
           edge.arrow.size=0.5,
           ...)
  {
    tmp <- unique(data.frame(source=x@source, destination=x@destination))

    if(nrow(tmp) > 0L) {
      g <- graph.data.frame(tmp)

      V(g)$label = V(g)$name
      V(g)$color <- ifelse(V(g)$label == x@root, "black", "white")

      root <- which(V(g)$name == x@root) - 1L
      layout <- layout.reingold.tilford(g, root=root)

      V(g)$label <- ''

      if(identical(x@direction, 'out')) {
        layout[,2] <- max(layout[,2]) - layout[,2]
        layout[,2] <- layout[,2] - max(layout[,2])
      }

      plot(g,
           layout=layout,
           vertex.size=vertex.size,
           edge.color=edge.color,
           edge.arrow.size=edge.arrow.size,
           ...)
    }
  }
)

setMethod('plot',
          signature(x = 'ContactTrace'),
          function(x,
                   vertex.size=8,
                   edge.color="black",
                   edge.arrow.size=0.5,
                   ...)
      {
          tmpIn <- unique(data.frame(source=x@ingoingContacts@source,
                                     destination=x@ingoingContacts@destination,
                                     stringsAsFactors=FALSE))

          ## Prefix all herds except root with 'In'
          tmpIn$source <- ifelse(tmpIn$source == x@ingoingContacts@root, tmpIn$source, paste('In', tmpIn$source, sep=''))
          tmpIn$destination <- ifelse(tmpIn$destination == x@ingoingContacts@root, tmpIn$destination, paste('In', tmpIn$destination, sep=''))

          tmpOut <- unique(data.frame(source=x@outgoingContacts@source,
                                      destination=x@outgoingContacts@destination,
                                      stringsAsFactors=FALSE))

          ## Prefix all herds except root with 'Out'
          tmpOut$source <- ifelse(tmpOut$source == x@outgoingContacts@root, tmpOut$source, paste('Out', tmpOut$source, sep=''))
          tmpOut$destination <- ifelse(tmpOut$destination == x@outgoingContacts@root, tmpOut$destination, paste('Out', tmpOut$destination, sep=''))

          if(nrow(tmpIn) > 0L) {
              ## In
              gIn <- graph.data.frame(tmpIn)
              rootIn <- which(V(gIn)$name == x@ingoingContacts@root) - 1L
              layoutIn <- layout.reingold.tilford(gIn, root=rootIn)

              if(nrow(tmpOut) > 0L) {
                  ## Out
                  gOut <- graph.data.frame(tmpOut)
                  rootOut <- which(V(gOut)$name == x@outgoingContacts@root) - 1L
                  layoutOut <- layout.reingold.tilford(gOut, root=rootOut)
                  layoutOut[,2] <- max(layoutOut[,2]) - layoutOut[,2]
                  layoutOut[,2] <- layoutOut[,2] - max(layoutOut[,2])

                  ## Combine in and out
                  g <- graph.data.frame(rbind(tmpIn, tmpOut))
                  V(g)$color <- ifelse(V(g)$name == x@ingoingContacts@root, "black", ifelse(grepl('^In', V(g)$name), "white", "gray"))
                  V(g)$label <- sub('(^In)|(^Out)', '', V(g)$name)
                  V(g)$label <- ''
                  layout <- rbind(layoutIn, layoutOut[-(rootOut+1),])

                  plot(g,
                       layout=layout,
                       vertex.size=vertex.size,
                       edge.color=edge.color,
                       edge.arrow.size=edge.arrow.size,
                       ...)
              } else {
                  V(gIn)$color <- ifelse(V(gIn)$name == x@ingoingContacts@root, "black", "white")
                  V(gIn)$label <- sub('^In', '', V(gIn)$name)
                  V(gIn)$label <- ''

                  plot(gIn,
                       layout=layoutIn,
                       vertex.size=vertex.size,
                       edge.color=edge.color,
                       edge.arrow.size=edge.arrow.size,
                       ...)
              }
          } else if(nrow(tmpOut) > 0L) {
              ## Out
              gOut <- graph.data.frame(tmpOut)
              rootOut <- which(V(gOut)$name == x@outgoingContacts@root) - 1L
              layoutOut <- layout.reingold.tilford(gOut, root=rootOut)
              layoutOut[,2] <- max(layoutOut[,2]) - layoutOut[,2]
              layoutOut[,2] <- layoutOut[,2] - max(layoutOut[,2])

              V(gOut)$color <- ifelse(V(gOut)$name == x@outgoingContacts@root, "black", "gray")
              V(gOut)$label <- sub('^Out', '', V(gOut)$name)
              V(gOut)$label <- ''

              plot(gOut,
                   layout=layoutOut,
                   vertex.size=vertex.size,
                   edge.color=edge.color,
                   edge.arrow.size=edge.arrow.size,
                   ...)
          }
      }
)
