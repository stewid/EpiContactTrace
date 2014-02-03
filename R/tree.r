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

##' Position nodes in a tree
##'
##' This function determines the coordinates for each node in a
##' tree. A pointer to the apex node of the tree is passed as
##' @param tree
##' @param sibling_separation
##' @param x_top_adjustment
##' @param y_top_adjustemnt
##' @keywords internal
##' @references \itemize{
##'   \item John Q. Walker II, A node positioning algorithm for general tress.\cr
##'   \url{http://www.cs.unc.edu/techreports/89-034.pdf}
##'}
position_tree <- function(tree,
                          sibling_separation=4,
                          level_separation=1,
                          x_top_adjustment=0,
                          y_top_adjustment=0)
{
    ## Clean up the positioning of small sibling subtrees
    apportion <- function(node, level) {
        left_most <- first_child(node, level)
        neighbor <- left_neighbor(left_most, level + 1)
        compare_depth <- 1
        depth_to_stop <- max(tree$level) - level

        while(all(!is.null(left_most),
                  !is.null(neighbor),
                  compare_depth <= depth_to_stop)) {
            ## Compute the location of left_most and where it
            ## should be with respect to neighbor.
            left_modsum <- 0
            right_modsum <- 0
            ancestor_left_most <- left_most
            ancestor_neighbor <- neighbor

            stop('Not implemented')
        }
    }

    ##
    ## Help functions to work with nodes
    ##
    is_leaf <- function(node, level) {
        if(level < max(tree$level))
            return(!any(tree$parent == node & tree$level == (level+1)))
        return(TRUE)
    }

    first_child <- function(node, level) {
        stopifnot(level < max(tree$level))
        children <- tree$node[tree$parent == node & tree$level == (level+1)]
        stopifnot(length(children)>0)
        return(children[1])
    }

    has_child <- function(node, level) {
        if(level < max(tree$level)) {
            children <- tree$node[tree$parent == node &
                                  tree$level == (level+1)]
            return(length(children)>0)
        }
        return(FALSE)
    }

    node_index <- function(node, level) {
        ## Deterimine row index to the node
        i <- which(tree$node == node & tree$level == level)
        stopifnot(identical(length(i), 1L))
        return(i[1])
    }

    left_size <- function(node) {
        return(2)
    }

    right_size <- function(node) {
        return(2)
    }

    left_neighbor <- function(node, level) {
        prelim <- tree$prelim[tree$level == level]
        i <- which(tree$prelim[node_index(node, level)] == prelim)
        browser()
        stopifnot(identical(length(i), 1L))
        if(i[1] > 1) {
            prelim <- prelim[i[1]-1]
            i <- which(tree$prelim == prelim & tree$level == level)
            stop('Not implemented')
            stopifnot(identical(length(i), 1L))
            return(i)
        }
        return(NULL)
    }

    ## This function returns the mean size of the two passed
    ## nodes. It adds the size of the right half of lefthand node
    ## to the left half of righthand node.
    mean_node_size <- function(left_node, right_node) {
        node_size <- 0
        if(!is.null(left_node))
            node_size <- node_size + right_size(left_node)
        if(!is.null(right_node))
            node_size <- node_size + left_size(right_node)
        return(node_size)
    }

    ##
    ## Help functions to work with siblings
    ##
    siblings <- function(node, level) {
        i <- node_index(node, level)
        parent <- tree$parent[i]
        if(is.na(parent)) {
            ## Check that node is root
            stopifnot(identical(level, 0))
            siblings <- node
        } else {
            siblings <- tree$node[tree$parent == tree$parent[i] &
                                  tree$level == level]
        }
        stopifnot(node %in% siblings)
        return(siblings)
    }

    has_left_sibling <- function(node, level) {
        return(!is.null(left_sibling(node, level)))
    }

    has_right_sibling <- function(node, level) {
        return(!is.null(right_sibling(node, level)))
    }

    left_sibling <- function(node, level) {
        s <- siblings(node, level)
        i <- which(node == s)
        stopifnot(identical(length(i), 1L))
        if(i[1] > 1)
            return(node_index(s[i[1]-1], level))
        return(NULL)
    }

    right_sibling <- function(node, level) {
        s <- siblings(node, level)
        i <- which(node == s)
        stopifnot(identical(length(i), 1L))
        if(i[1] < length(s))
            return(node_index(s[i[1]+1], level))
        return(NULL)
    }

    ## Every node of the tree is assigned a preliminary x-coordinate
    ## (held in column prelim). In addition, internal nodes are given
    ## modifiers, which will be used to move their offspring to the
    ## right (held in column modifier).
    first_walk <- function(node, level) {
        i <- node_index(node, level)

        ## Set the default modifier value.
        tree$modifier[i] <<- 0

        if(is_leaf(node, level)) {
            if(has_left_sibling(node, level)) {
                ## Determine the preliminary x-coordinate based on:
                ##  - the preliminary x-coordinate of the left sibling,
                ##  - the separation between sibling nodes, and
                ##  - the mean size of left sibling and current node.
                tree$prelim[i] <<- (
                    tree$prelim[left_sibling(node, level)] +
                    sibling_separation +
                    mean_node_size(left_sibling(node, level), i))
            } else {
                ## No sibling on the left to worry about.
                tree$prelim[i] <<- 0
            }
        } else {
            ## This node is not a leaf, so call this procedure
            ## recursively for each of its offspring.
            right_most <- first_child(node, level)
            left_most <- right_most
            first_walk(left_most, level+1)
            while(has_right_sibling(right_most, level+1)) {
                right_most <- tree$node[right_sibling(right_most, level+1)]
                first_walk(right_most, level+1)
            }

            mid_point <- (tree$prelim[node_index(left_most, level+1)] +
                          tree$prelim[node_index(right_most, level+1)]) / 2

            if(has_left_sibling(node, level)) {
                tree$prelim[i] <<- (
                    tree$prelim[left_sibling(node, level)] +
                    sibling_separation +
                    mean_node_size(left_sibling(node, level), i))

                tree$modifier <<- tree$prelim[i] - mid_point

                apportion(node, level)
            } else {
                tree$prelim[i] <<- mid_point
            }
        }
    }

    check_extents_range <- function(x_temp, y_temp) {
        return(TRUE)
    }

    ## Each node is given a final x-coordinate by summing its
    ## preliminary x-coordinate and the modifiers of all the node's
    ## ancestors. The y-coordinate depends on the height of the
    ## tree. If the actual position of an interior node is right of
    ## its preliminary place, the subtree rooted at the node must be
    ## moved right to center the sons around the father. Rather than
    ## immediately readjust all the nodes in the subtree, each node
    ## remembers the distance to the provisional place in a modifier
    ## field. In this second pass down the tree, modifiers are
    ## accumulated and applied to every node.
    second_walk <- function(node, level, modsum) {
        if(level <= max(tree$level)) {
            i <- node_index(node, level)

            x_temp <- x_top_adjustment + tree$prelim[i] + modsum
            y_temp <- y_top_adjustment + (level * level_separation)

            ## Check that x_temp and y_temp are of the proper size.
            if(check_extents_range(x_temp, y_temp)) {
                tree$x[i] <<- x_temp
                tree$y[i] <<- y_temp
                result <- TRUE

                if(has_child(node, level)) {
                    ## Apply the modifier value for this node to
                    ## all its offspring.
                    result <- second_walk(first_child(node, level),
                                          level + 1,
                                          modsum + tree$modifier[i])
                }

                if(all(identical(result, TRUE),
                       has_right_sibling(node, level))) {
                    node <- tree$node[right_sibling(node, level)]
                    result <- second_walk(node, level, modsum)
                }
            } else {
                ## Continuing would put the tree outside of the
                ## drawable extents range.
                result <- FALSE
            }
        } else {
            ## We are at a level deeper than what we want to draw.
            result <- TRUE
        }

        return(result)
    }

    tree$x <- NA_real_
    tree$y <- NA_real_
    tree$prelim <- NA_real_
    tree$modifier <- NA_real_

    first_walk(tree$node[1], 0)
    ## second_walk(tree$node[1], 0, 0)

    return(tree)
}
