##' Trace Contacts.
##'
##' Contact tracing for a specied node(s) (root) during a specfied time period.
##' The time period is divided into two parts, one for ingoing contacts and one
##' for outgoing contacts. For ingoing contacts the time period starts at
##' inBegin and ends at inEndDate.  For outgoing contacts the time period
##' starts at outBegin and ends at outEnd.
##'
##'
##' The argument movements in TraceDateInterval is a \code{data.frame}
##' with the following columns:
##' \describe{
##'
##'   \item{source}{
##'     an integer or character identifier of the source holding.
##'   }
##'
##'   \item{destination}{
##'     an integer or character identifier of the destination holding.
##'   }
##'
##'   \item{t}{
##'     the Date of the transfer
##'   }
##'
##'   \item{id}{
##'     an optional character vector with the identity of the animal.
##'   }
##'
##'   \item{n}{
##'     an optional numeric vector with the number of animals moved.
##'   }
##'
##'   \item{category}{
##'     an optional character or factor with category of the animal e.g. Cattle.
##'   }
##' }
##'
##' @usage TraceDateInterval(movements, root, inBegin, inEnd, outBegin, outEnd)
##' @param movements a \code{data.frame} data.frame with movements, see
##' details.
##' @param root vector of roots to perform contact tracing for.
##' @param inBegin the last date to include ingoing movements
##' @param inEnd the number of previous days before inEndDate to include
##' ingoing movements
##' @param outBegin the first date to include outgoing movements
##' @param outEnd the number of days from endStartDate to include outgoing
##' movements
##' @seealso \code{\link{Trace}}.
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
##' @export
##' @examples
##'
##' # Load data
##' data(transfers)
##'
##' # Perform contact tracing
##' contactTrace <- TraceDateInterval(movements=transfers,
##'                                   root=2645,
##'                                   inBegin='2005-08-01',
##'                                   inEnd='2005-10-31',
##'                                   outBegin='2005-08-01',
##'                                   outEnd='2005-10-31')
##'
##' # Show result of contact tracing
##' show(contactTrace)
##'
##' \dontrun{
##' # Plot in- and outgoing contact chain
##' plot(contactTrace)
##'
##' # Create a network summary for all included herds
##' # First extract all source and destination from the dataset
##' root <- sort(unique(c(transfers$source,
##'                       transfers$destination)))
##'
##' # Perform contact tracing
##' contactTrace <- TraceDateInterval(movements=transfers,
##'                                   root=root,
##'                                   inBegin='2005-08-01',
##'                                   inEnd='2005-10-31',
##'                                   outBegin='2005-08-01',
##'                                   outEnd='2005-10-31')
##'
##' NetworkSummary(contactTrace)
##' }
##'
TraceDateInterval <- function(movements,
                              root,
                              inBegin,
                              inEnd,
                              outBegin,
                              outEnd)
{
    # Before doing any contact tracing check that arguments are ok
    # from various perspectives.
    if(any(missing(movements),
           missing(root),
           missing(inBegin),
           missing(inEnd),
           missing(outBegin),
           missing(outEnd))) {
        stop('Missing parameters in call to TraceDateInterval')
    }

    if(!is.data.frame(movements)) {
        stop('movements must be a data.frame')
    }

    if(!all(c('source', 'destination', 't') %in% names(movements))) {
        stop('movements must contain the columns source, destination and t.')
    }

    if(any(is.factor(movements$source), is.integer(movements$source))) {
        movements$source <- as.character(movements$source)
    } else if(!is.character(movements$source)) {
        stop('invalid class of column source in movements')
    }

    if(any(is.factor(movements$destination), is.integer(movements$destination))) {
        movements$destination <- as.character(movements$destination)
    } else if(!is.character(movements$destination)) {
        stop('invalid class of column destination in movements')
    }

    if(!identical(class(movements$t), 'Date')) {
        stop('invalid class of column t in movements')
    }
    movements$t <- as.integer(julian(movements$t))

    if('n' %in% names(movements)) {
        if(is.integer(movements$n)) {
            movements$n <- as.numeric(movements$n)
        } else if(!is.numeric(movements$n)) {
            stop('invalid class of column n in movements')
        }
    } else {
        movements$n <- as.numeric(NA)
    }

    if('id' %in% names(movements)) {
        if(any(is.factor(movements$id), is.integer(movements$id))) {
            movements$id <- as.character(movements$id)
        } else if(!is.character(movements$id)) {
            stop('invalid class of column id in movements')
        }
    } else {
        movements$id <- as.character(NA)
    }

    if('category' %in% names(movements)) {
        if(any(is.factor(movements$category), is.integer(movements$category))) {
            movements$category <- as.character(movements$category)
        } else if(!is.character(movements$category)) {
            stop('invalid class of column category in movements')
        }
    } else {
        movements$category <- as.character(NA)
    }

    # Make sure the columns are in expected order
    if(!identical(names(movements), c('source',
                                      'destination',
                                      't',
                                      'id',
                                      'n',
                                      'category'))) {
        movements <- movements[, c('source',
                                   'destination',
                                   't',
                                   'id',
                                   'n',
                                   'category')]
    }

    if(any(is.factor(root), is.integer(root))) {
        root <- as.character(root)
    } else if(is.numeric(root)) {
        # root is supposed to be a character or integer identifier
        # so test that root is a integer the same way as binom.test test x
        rootr <- round(root)
        if(any(max(abs(root - rootr) > 1e-07))) {
            stop("'root' must be an integer or character")
        }

        root <- as.character(rootr)
    } else if(!is.character(root)) {
        stop('invalid class of root')
    }

    if(any(is.character(inBegin), is.factor(inBegin))) {
        inBegin <- as.Date(inBegin)
    }

    if(!identical(class(inBegin), 'Date')) {
        stop('inBegin must be a Date vector')
    }

    if(any(is.character(inEnd), is.factor(inEnd))) {
        inEnd <- as.Date(inEnd)
    }

    if(!identical(class(inEnd), 'Date')) {
        stop('inEnd must be a Date vector')
    }

    if(any(is.character(outBegin), is.factor(outBegin))) {
        outBegin <- as.Date(outBegin)
    }

    if(!identical(class(outBegin), 'Date')) {
        stop('outBegin must be a Date vector')
    }

    if(any(is.character(outEnd), is.factor(outEnd))) {
        outEnd <- as.Date(outEnd)
    }

    if(!identical(class(outEnd), 'Date')) {
        stop('outEnd must be a Date vector')
    }

    if(any(inEnd < inBegin)) {
        stop('inEnd < inBegin')
    }

    if(any(outEnd < outBegin)) {
        stop('outEnd < outBegin')
    }

    # Arguments seems ok...go on with contact tracing

    result <- list()
    searchContacts <- SearchContacts(movements)
    rm(movements)

    timeWindow <- data.frame(root = root,
                             inBegin = inBegin,
                             inEnd = inEnd,
                             outBegin = outBegin,
                             outEnd = outEnd,
                             stringsAsFactors = FALSE)

    for(i in seq_len(nrow(timeWindow))) {
        ingoingContacts <- searchContacts$Ingoing(root = timeWindow$root[i],
                                                  tBegin = as.integer(julian(timeWindow$inBegin[i])),
                                                  tEnd = as.integer(julian(timeWindow$inEnd[i])))

        outgoingContacts <- searchContacts$Outgoing(root = timeWindow$root[i],
                                                    tBegin = as.integer(julian(timeWindow$outBegin[i])),
                                                    tEnd = as.integer(julian(timeWindow$outEnd[i])))

        result <- append(result, new('ContactTrace',
                                     root = timeWindow$root[i],
                                     ingoingContacts = ingoingContacts,
                                     outgoingContacts = outgoingContacts))
    }

    # Name each list item with ContactTrace objects to the name of the ContactTrace root.
    names(result) <-  sapply(result, function(listItem) listItem@ingoingContacts@root)

    if(identical(length(result), 1L))
      return(result[[1]])

    return(result)
}

##' Trace Contacts.
##'
##' Contact tracing for a specied node(s) (root) during a specfied time period.
##' The time period is divided into two parts, one for ingoing contacts and one
##' for outgoing contacts. For ingoing contacts the time period ends at
##' inEndDate and the number of inDays is number of previous days. For outgoing
##' contacts the time period starts at outStartDate and outDays is the number
##' of days to include after that date.
##'
##'
##' @usage Trace(movements, root, tEnd, days)
##' @param movements a \code{data.frame} with contacts due to animal
##' movements between holdings, see \code{\link{TraceDateInterval}} for
##' details.
##' @param root vector of roots to perform contact tracing on.
##' @param tEnd the last date to include ingoing and outgoing movements
##' @param days the number of previous days before tEnd to include ingoing and
##' outgoing movements
##' @seealso \code{\link{TraceDateInterval}}.
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
##' @export
##' @examples
##'
##' # Load data
##' data(transfers)
##'
##' # Perform contact tracing
##' contactTrace <- Trace(movements=transfers,
##'                       root=2645,
##'                       tEnd='2005-10-31',
##'                       days=90)
##'
##' # Show result of contact tracing
##' show(contactTrace)
##'
##' \dontrun{
##' # Plot in- and outgoing contact chain
##' plot(contactTrace)
##'
##' # Create a network summary for all included herds
##' # First extract all source and destination from the dataset
##' root <- sort(unique(c(transfers$source,
##'                       transfers$destination)))
##'
##' # Perform contact tracing
##' contactTrace <- Trace(movements=transfers,
##'                       root=root,
##'                       tEnd='2005-10-31',
##'                       days=90)
##'
##' NetworkSummary(contactTrace)
##' }
##'
Trace <- function(movements,
                  root,
                  tEnd,
                  days)
{
    if(any(missing(movements),
           missing(root),
           missing(tEnd),
           missing(days))) {
        stop('Missing parameters in call to Trace')
    }

    if(any(is.character(tEnd), is.factor(tEnd))) {
        tEnd <- as.Date(tEnd)
    }

    if(!identical(class(tEnd), 'Date')) {
        stop('tEnd must be a Date vector')
    }

    # Test that days is a nonnegative integer the same way as binom.test test x
    daysr <- round(days)
    if (any(is.na(days) | (days < 0)) || max(abs(days - daysr)) > 1e-07) {
        stop("'days' must be nonnegative and integer")
    }
    days <- daysr

    return(TraceDateInterval(movements = movements,
                             root = root,
                             inBegin = tEnd - days,
                             inEnd = tEnd,
                             outBegin = tEnd - days,
                             outEnd = tEnd))
}

SearchContacts <- function(movements) {
    # Build a lookup table of all movements both for ingoing and outgoing contacts.
    # The lookup tables are built in environments using hashes

    # Make sure all nodes have a valid variable name by making
    # a factor of source and destination
    nodes <- as.factor(unique(c(movements$source, movements$destination)))

    # Now convert the source to an integer vector with indices to levels(nodes)
    movements$source <- factor(movements$source, levels=levels(nodes))
    movements$source <- as.integer(movements$source)

    # Now convert the destination to an integer vector with indices to levels(nodes)
    movements$destination <- factor(movements$destination, levels=levels(nodes))
    movements$destination <- as.integer(movements$destination)

    movements$n <- as.factor(movements$n)
    nLevels <- levels(movements$n)
    movements$n <- as.integer(movements$n)

    movements$id <- as.factor(movements$id)
    idLevels <- levels(movements$id)
    movements$id <- as.integer(movements$id)

    movements$category <- as.factor(movements$category)
    categoryLevels <- levels(movements$category)
    movements$category <- as.integer(movements$category)

    movements <- data.matrix(unique(movements))
    dimnames(movements) <- NULL

    # Create a lookup table for the ingoing contacts...

    # Order the movements matrix based on destination
    movements <- movements[order(movements[,2]),]

    # Build an index table to first and last index of each destination.
    # First, determine the frequency of each destination
    index <- tabulate(movements[,2])
    index <- index[index > 0]

    # Determine the last index of each destination
    index <- cumsum(index)

    # Now create a matrix with destination, first and last index to destination
    index <- matrix(c(sort(unique(movements[,2])),
                    c(1L, index[-length(index)] + 1L),
                    index),
                    ncol=3)

    # Create an environment for the lookup table of ingoing contacts
    ingoing <- new.env(hash=TRUE, size=nrow(index))
    resultIngoing <- new.env(hash=TRUE, size=nrow(index))

    # Add all ingoing contacts to the lookup table
    lapply(seq_len(nrow(index)), function(i) {
        assign(as.character(index[i,1]),
               movements[index[i,2]:index[i,3],,drop=FALSE],
               envir=ingoing)
    })

    # Create a lookup table for the outgoing contacts...

    # Order the movements matrix based on source
    movements <- movements[order(movements[,1]),]

    # Determine the frequency of each source
    index <- tabulate(movements[,1])
    index <- index[index > 0]

    # Determine the last index of each destination
    index <- cumsum(index)

    # Now create a matrix with source, first and last index to destination
    index <- matrix(c(sort(unique(movements[,1])),
                    c(1L, index[-length(index)] + 1L),
                    index),
                    ncol=3)

    # Create an environment for the lookup table of outgoing contacts
    outgoing <- new.env(hash=TRUE, size=nrow(index))
    resultOutgoing <- new.env(hash=TRUE, size=nrow(index))

    # Add all ingoing contacts to the lookup table
    lapply(seq_len(nrow(index)), function(i) {
        assign(as.character(index[i,1]),
               movements[index[i,2]:index[i,3],,drop=FALSE],
               envir=outgoing)
    })

    rm(movements)
    rm(index)

    searchIngoingContacts <- function(node,
                                      tBegin,
                                      tEnd,
                                      visitedNodes,
                                      distance)
    {
        contacts <- ingoing[[as.character(node)]]

        if(is.null(contacts)) {return(NULL)}

        visitedNodes <- c(visitedNodes, node)

        # We are only interested in contacts within the specified time period
        # and not going in loops or backwards in the search path.
        contacts <- contacts[contacts[,3] <= tEnd &
                             contacts[,3] >= tBegin &
                             !(contacts[,1] %in% visitedNodes),,drop=FALSE]

        # Did the search result in any ingoing contacts
        if(length(contacts) > 0L) {
            # Add a column for distance
            contacts <- cbind(contacts, distance, deparse.level=0)

            # Continue to search for ingoing contacts to source
            result <- tapply(seq_len(nrow(contacts)), contacts[,1], function(i) {
                # Extract information
                m1 <- contacts[i,,drop=FALSE]

                # Continue to search for more contacts
                m2 <- searchIngoingContacts(node=m1[1,1],
                                            tBegin=tBegin,
                                            tEnd=max(m1[,3]),
                                            visitedNodes=visitedNodes,
                                            distance=distance + 1L)

                # Combine the matrices
                return(rbind(m1, m2))
            })

            result <- do.call(rbind, result)
            dimnames(result) <- NULL

            return(result)
        }

        return(NULL)
    }

    searchIngoingContacts <- compiler::cmpfun(searchIngoingContacts)

    searchOutgoingContacts <- function(node,
                                       tBegin,
                                       tEnd,
                                       visitedNodes,
                                       distance)
    {
        contacts <- outgoing[[as.character(node)]]

        if(is.null(contacts)) {return(NULL)}

        visitedNodes <- c(visitedNodes, node)

        # We are only interested in contacts within the specified time period
        # and not going in loops or backwards in the search path.
        contacts <- contacts[contacts[,3] <= tEnd &
                             contacts[,3] >= tBegin &
                             !(contacts[,2] %in% visitedNodes),,drop=FALSE]

        # Did the search result in any ingoing contacts
        if(length(contacts) > 0L) {
            # Add a column for distance
            contacts <- cbind(contacts, distance, deparse.level=0)

            # Continue to search for ingoing contacts to source
            result <- tapply(seq_len(nrow(contacts)), contacts[,2], function(i) {
                # Extract information
                m1 <- contacts[i,,drop=FALSE]

                # Continue to search for more contacts
                m2 <- searchOutgoingContacts(node=m1[1,2],
                                             tBegin=min(m1[,3]),
                                             tEnd=tEnd,
                                             visitedNodes=visitedNodes,
                                             distance=distance + 1L)

                # Combine the matrices
                return(rbind(m1, m2))
            })

            result <- do.call(rbind, result)
            dimnames(result) <- NULL

            return(result)
        }

        return(NULL)
    }

    searchOutgoingContacts <- compiler::cmpfun(searchOutgoingContacts)

    Ingoing <- function(root, tBegin, tEnd)
    {
        contacts <- matrix(integer(0), ncol=6, nrow=0)
        distance <- integer(0)
        index <- integer(0)

        node <- which(root == levels(nodes))

        # Check if root has any ingoing contacts
        if(length(node)) {
            result <- searchIngoingContacts(node = node,
                                            tBegin = tBegin,
                                            tEnd = tEnd,
                                            visitedNodes = integer(0),
                                            distance = 1L)

            if(!is.null(result)) {
                # Extract last column with distance from root
                distance <- result[, 7]
                result <- result[, 1:6, drop=FALSE]

                # Since the algorithm might visit the same node more than once
                # make sure we have unique contacts
                contacts <- unique(result)

                # Create an index to contacts, so that the result matrix can be reconstructed
                # from the contacts, combined with index and distance
                # result <- cbind(contacts[index,], distance)
                index <- match(apply(result, 1, function(x) paste(x, collapse="\r")),
                               apply(contacts, 1, function(x) paste(x, collapse="\r")))
            }
        }

        return(new('Contacts',
                   root = root,
                   tBegin = as.Date(tBegin, origin = as.Date('1970-01-01')),
                   tEnd = as.Date(tEnd, origin = as.Date('1970-01-01')),
                   source = levels(nodes)[contacts[,1]],
                   destination = levels(nodes)[contacts[,2]],
                   t = as.Date(contacts[,3], origin = as.Date('1970-01-01')),
                   id = idLevels[contacts[,4]],
                   n = as.numeric(nLevels[contacts[,5]]),
                   category = categoryLevels[contacts[,6]],
                   index = index,
                   distance = distance,
                   direction = 'in'))
    }

    Outgoing <- function(root, tBegin, tEnd)
    {
        contacts <- matrix(integer(0), ncol=6, nrow=0)
        distance <- integer(0)
        index <- integer(0)

        node <- which(root == levels(nodes))

        # Check if root has any outgoing contacts
        if(length(node)) {
            result <- searchOutgoingContacts(node = node,
                                             tBegin = tBegin,
                                             tEnd = tEnd,
                                             visitedNodes = integer(0),
                                             distance = 1L)

            if(!is.null(result)) {
                # Extract last column with distance from root
                distance <- result[, 7]
                result <- result[, 1:6, drop=FALSE]

                # Since the algorithm might visit the same node more than once
                # make sure we have unique contacts
                contacts <- unique(result)

                # Create an index to the contacts, so that the result matrix can be reconstructed
                # from the contacts, combined with index and distance
                # result <- cbind(contacts[index,], distance)
                index <- match(apply(result, 1, function(x) paste(x, collapse="\r")),
                               apply(contacts, 1, function(x) paste(x, collapse="\r")))
            }
        }

        return(new('Contacts',
                   root = root,
                   tBegin = as.Date(tBegin, origin = as.Date('1970-01-01')),
                   tEnd = as.Date(tEnd, origin = as.Date('1970-01-01')),
                   source = levels(nodes)[contacts[,1]],
                   destination = levels(nodes)[contacts[,2]],
                   t = as.Date(contacts[,3], origin = as.Date('1970-01-01')),
                   id = idLevels[contacts[,4]],
                   n = as.numeric(nLevels[contacts[,5]]),
                   category = categoryLevels[contacts[,6]],
                   index = index,
                   distance = distance,
                   direction = 'out'))
    }

    return(list(Ingoing = Ingoing, Outgoing=Outgoing))
}
