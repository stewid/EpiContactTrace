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
##' @param left_size
##' @param right_size
##' @keywords internal
##' @references \itemize{
##'   \item John Q. Walker II, A node positioning algorithm for general tress.\cr
##'   \url{http://www.cs.unc.edu/techreports/89-034.pdf}
##'}
position_tree <- function(tree,
                          sibling_separation=4,
                          level_separation=1,
                          x_top_adjustment=0,
                          y_top_adjustment=0,
                          left_size=1,
                          right_size=1)
{
    ## Clean up the positioning of small sibling subtrees
    apportion <- function(node) {
        return(NULL)
        left_most <- first_child(node)
        neighbor <- left_neighbor(left_most)
        compare_depth <- 1
        depth_to_stop <- max(tree$level) - node_level(node)

        while(all(!is.null(left_most),
                  !is.null(neighbor),
                  compare_depth <= depth_to_stop)) {
            ## Compute the location of left_most and where it
            ## should be with respect to neighbor.
            left_modsum <- 0
            right_modsum <- 0
            ancestor_left_most <- left_most
            ancestor_neighbor <- neighbor

            for(i in seq_len(compare_depth)) {
                j <- node_index(ancestor_left_most)
                k <- node_index(ancestor_neighbor)

                ancestor_left_most <- tree$parent[j]
                ancestor_neighbor <- tree$parent[k]
                right_modsum <- right_modsum + tree$modifier[j]
                left_modsum <- left_modsum + tree$modifier[k]
            }

            ## Find the move_distance, and apply it to Node's subtree.
            ## Add appropriate portions to smaller interior subtrees.
            move_distance <- ((tree$prelim[node_index(neighbor)] +
                               left_modsum +
                               subtree_separation +
                               mean_node_size(left_most, neighbor)) -
                              (tree$prelim[node_index(left_most)] +
                               right_modsum))

            if(move_distance > 0) {
                ## Count interior sibling subtrees in left siblings
                temp_node <- node
                left_siblings <- 0

                while(all(!is.null(temp_node),
                          !identical(temp_node, ancestor_neighbor))) {
                    left_siblings <- left_siblings + 1
                    temp_node <- left_sibling(temp_node)
                }

                if(!is.null(temp_node)) {
                    ## Apply portions to appropriate leftsibling
                    ## subtrees
                    portion <- move_distance / left_siblings
                    temp_node <- node

                    while(stop('Fix')) {
                        i <- node_index(temp_node)
                        tree$prelim[i] <<- tree$prelim[i] + move_distance
                        tree$modifier[i] <<- tree$modifier[i] + move_distance
                        move_distance <- move_distance - portion
                        temp_node <- left_sibling(temp_node)
                    }
                } else {
                    return(NULL)
                }
            }

            compare_depth <- compare_depth + 1
            if(is_leaf(left_most)) {
                left_most <- get_left_most(node, 0, compare_depth)
            } else {
                left_most <- first_child(left_most)
            }
        }
    }

    ##
    ## Help functions to work with nodes
    ##
    is_leaf <- function(node) {
        return(!has_child(node))
    }

    first_child <- function(node) {
        children <- tree$node[!is.na(tree$parent) & (tree$parent == node)]
        if(length(children)>0) {
            return(children[1])
        }
        return(NULL)
    }

    has_child <- function(node) {
        return(!is.null(first_child(node)))
    }

    node_index <- function(node) {
        ## Deterimine row index to the node
        i <- which(tree$node == node)
        stopifnot(identical(length(i), 1L))
        return(i[1])
    }

    node_level <- function(node) {
        return(tree$level[node_index(node)])
    }

    left_neighbor <- function(node) {
        n <- tree$node[tree$level == node_level(node)]
        stopifnot(node %in% n)
        i <- which(node == n)
        stopifnot(identical(length(i), 1L))
        if(i[1] > 1)
            return(n[i[1]-1])
        return(NULL)
    }

    ## This function returns the mean size of the two passed
    ## nodes. It adds the size of the right half of lefthand node
    ## to the left half of righthand node.
    mean_node_size <- function(left_node, right_node) {
        node_size <- 0
        if(!is.null(left_node))
            node_size <- node_size + right_size
        if(!is.null(right_node))
            node_size <- node_size + left_size
        return(node_size)
    }

    ##
    ## Help functions to work with siblings
    ##
    siblings <- function(node) {
        i <- node_index(node)
        parent <- tree$parent[i]
        if(is.na(parent)) {
            ## Check that node is root
            stopifnot(identical(node_level(node), 0L))
            siblings <- node
        } else {
            siblings <- tree$node[!is.na(tree$parent) &
                                  (tree$parent == parent)]
        }
        stopifnot(node %in% siblings)
        return(siblings)
    }

    has_left_sibling <- function(node) {
        return(!is.null(left_sibling(node)))
    }

    has_right_sibling <- function(node) {
        return(!is.null(right_sibling(node)))
    }

    left_sibling <- function(node) {
        s <- siblings(node)
        i <- which(node == s)
        stopifnot(identical(length(i), 1L))
        if(i[1] > 1)
            return(s[i[1]-1])
        return(NULL)
    }

    right_sibling <- function(node) {
        s <- siblings(node)
        i <- which(node == s)
        stopifnot(identical(length(i), 1L))
        if(i[1] < length(s))
            return(s[i[1]+1])
        return(NULL)
    }

    ## Every node of the tree is assigned a preliminary x-coordinate
    ## (held in column prelim). In addition, internal nodes are given
    ## modifiers, which will be used to move their offspring to the
    ## right (held in column modifier).
    first_walk <- function(node) {
        i <- node_index(node)

        ## Set the default modifier value.
        tree$modifier[i] <<- 0

        if(is_leaf(node)) {
            if(has_left_sibling(node)) {
                ## Determine the preliminary x-coordinate based on:
                ##  - the preliminary x-coordinate of the left sibling,
                ##  - the separation between sibling nodes, and
                ##  - the mean size of left sibling and current node.
                tree$prelim[i] <<- (
                    tree$prelim[node_index(left_sibling(node))] +
                    sibling_separation +
                    mean_node_size(left_sibling(node), node))
            } else {
                ## No sibling on the left to worry about.
                tree$prelim[i] <<- 0
            }
        } else {
            ## This node is not a leaf, so call this procedure
            ## recursively for each of its offspring.
            right_most <- first_child(node)
            left_most <- right_most
            first_walk(left_most)
            while(has_right_sibling(right_most)) {
                right_most <- tree$node[node_index(right_sibling(right_most))]
                first_walk(right_most)
            }

            mid_point <- (tree$prelim[node_index(left_most)] +
                          tree$prelim[node_index(right_most)]) / 2

            if(has_left_sibling(node)) {
                tree$prelim[i] <<- (
                    tree$prelim[node_index(left_sibling(node))] +
                    sibling_separation +
                    mean_node_size(left_sibling(node), node))

                tree$modifier[i] <<- tree$prelim[i] - mid_point

                apportion(node)
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
            i <- node_index(node)

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

    tree$level <- as.integer(tree$level)
    tree$x <- NA_real_
    tree$y <- NA_real_
    tree$prelim <- NA_real_
    tree$modifier <- NA_real_

    first_walk(tree$node[1])
    ## second_walk(tree$node[1], 0, 0)

    return(tree)
}
