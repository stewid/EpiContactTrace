## Copyright 2013-2014 Stefan Widgren and Maria Noremark,
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

##' Build a graph tree from the NetworkStructure
##'
##' @param network_structure a data.frame from the call
##' \code{NetworkStructure} with a \code{ContactTrace} object
##' @return A \code{list} with the two fields \code{ingoing} and
##' \code{outgoing}. The fields are \code{NULL} or contain a
##' \code{data.frame} with the tree. The fields are \code{NULL} if
##' there are no in- or outgoing contacts.
##' @keywords internal
build_tree <- function(network_structure)
{
    stopifnot(is.data.frame(network_structure))
    root <- unique(network_structure$root)
    stopifnot(identical(length(root), 1L))

    tree.in <- network_structure[network_structure$direction == "in",]
    tree.out <- network_structure[network_structure$direction == "out",]

    result <- list(ingoing=NULL, outgoing=NULL)

    root_node <- data.frame(node = root[1],
                            parent = NA_character_,
                            level = 0,
                            stringsAsFactors = FALSE)

    if(nrow(tree.in)) {
        i <- order(tree.in$distance, tree.in$source)
        tree.in <- tree.in[i, c('source', 'distance')]
        tree.in <- tree.in[!duplicated(tree.in$source),]
        tree.in$parent <- NA_character_
        colnames(tree.in)[1:2] <- c('node', 'level')
        tree.in <- tree.in[, colnames(root_node)]

        for(lev in rev(seq_len(max(tree.in$level)))) {
            for(src in tree.in$node[tree.in$level == lev]) {
                if(lev > 1) {
                    i <- which(network_structure$direction == "in"
                               & network_structure$distance == lev)
                    dst <- network_structure$destination[i]
                    dst <- unique(dst)
                } else {
                    dst <- root
                }

                stopifnot(length(dst)>0)
                tree.in$parent[tree.in$level == lev
                               & tree.in$node == src] <- dst[1]
            }
        }

        tree.in <- rbind(root_node, tree.in)
        rownames(tree.in) <- NULL
        result$ingoing <- tree.in
    }

    if(nrow(tree.out)) {
        i <- order(tree.out$distance, tree.out$destination)
        tree.out <- tree.out[i, c('destination', 'distance')]
        tree.out <- tree.out[!duplicated(tree.out$destination),]
        tree.out$parent <- NA_character_
        colnames(tree.out)[1:2] <- c('node', 'level')
        tree.out <- tree.out[, colnames(root_node)]

        for(lev in rev(seq_len(max(tree.out$level)))) {
            for(dst in tree.out$node[tree.out$level == lev]) {
                if(lev > 1) {
                    i <- which(network_structure$direction == "out"
                               & network_structure$distance == lev)
                    src <- network_structure$source[i]
                    src <- unique(src)
                } else {
                    src <- root
                }

                stopifnot(length(src)>0)
                tree.out$parent[tree.out$level == lev
                                & tree.out$node == dst] <- src[1]
            }
        }

        tree.out <- rbind(root_node, tree.out)
        rownames(tree.out) <- NULL
        result$outgoing <- tree.out
    }

    return(result)
}
