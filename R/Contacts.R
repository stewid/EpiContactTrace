## Copyright 2013-2020 Stefan Widgren and Maria Noremark,
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

##' Class \code{"Contacts"}
##'
##' Class to handle contacts.
##'
##'
##' The \code{Contacts} class keeps track of all ingoing or outgoing
##' livstock transfers in the contact chain for a specific root within
##' the time window used for contact tracing.  The slots;
##' \code{source}, \code{destination}, \code{t}, \code{id}, \code{n}
##' and \code{category} contains contact information extracted from
##' the movement dataset during contact tracing. The \code{index} slot
##' is an index to the extracted contacts within the class that
##' together with the \code{distance} slot can be used to rebuild the
##' exact contacts that were extracted from each search step during
##' the contact tracing.
##' @slot root A \code{character} vector of length one with the
##'     identifier of the root.
##' @slot tBegin A \code{Date} vector of length one with the start
##'     date of the time window used for contact tracing.
##' @slot tEnd A \code{Date} vector of length one with the end date of
##'     the time window used for contact tracing.
##' @slot source A \code{character} vector with the identifiers of the
##'     source holdings of the livestock transfer.
##' @slot destination A \code{character} vector with the identifier of
##'     the destination holdings of the livestock transfer.
##' @slot t A \code{Date} vector of the livestock transfer.
##' @slot id A \code{character} vector with the identifiers of the
##'     animals.
##' @slot n A \code{numeric} vector with the number of animals
##'     transfered.
##' @slot category A \code{character} vector with the category of
##'     animals e.g. cattle.
##' @slot index A \code{integer} index vector.
##' @slot distance A \code{integer} vector with the distance from root
##'     for the contact[index]
##' @slot direction A \code{character} vector of length one equal to
##'     the direction "in" or "out"
##' @section Objects from the Class: Objects can be created by calls
##'     of the form \code{new("Contacts", root, startDate, days,
##'     source, destination, t, id, n, category, level, direction,
##'     ...)}.
##' @export
##' @examples
##'
##' ## Load data
##' data(transfers)
##'
##' ## Perform contact tracing
##' contactTrace <- Trace(movements = transfers,
##'                       root = 2645,
##'                       tEnd = "2005-10-31",
##'                       days = 90)
##'
##' ## Show structure of ingoing contacts
##' str(contactTrace@@ingoingContacts)
##'
##' ## Show structure of ougoing contacts
##' str(contactTrace@@outgoingContacts)
##'
setClass(
    "Contacts",
    slots = c(root        = "character",
              tBegin      = "Date",
              tEnd        = "Date",
              source      = "character",
              destination = "character",
              t           = "Date",
              id          = "character",
              n           = "numeric",
              category    = "character",
              index       = "integer",
              distance    = "integer",
              direction   = "character"),
    validity = function(object) {
        l <- unique(c(length(object@source),
                      length(object@destination),
                      length(object@t),
                      length(object@id),
                      length(object@n),
                      length(object@category)))
        if (!identical(length(l), 1L)) {
            return(paste0("Lengths of source, destination, t, id, ",
                          "n and category should have equal length"))
        }

        TRUE
    }
)

setAs(
    from = "Contacts",
    to   = "data.frame",
    def  = function(from) {
        if (length(from@source) > 0L) {
            df1 <- data.frame(source           = from@source,
                              destination      = from@destination,
                              t                = from@t,
                              id               = from@id,
                              n                = from@n,
                              category         = from@category,
                              stringsAsFactors = FALSE)

            if (identical(from@direction, "in")) {
                df2 <- data.frame(root             = from@root,
                                  inBegin          = from@tBegin,
                                  inEnd            = from@tEnd,
                                  outBegin         = as.Date(as.character(NA)),
                                  outEnd           = as.Date(as.character(NA)),
                                  direction        = "in",
                                  stringsAsFactors = FALSE)
            } else {
              df2 <- data.frame(root             = from@root,
                                inBegin          = as.Date(as.character(NA)),
                                inEnd            = as.Date(as.character(NA)),
                                outBegin         = from@tBegin,
                                outEnd           = from@tEnd,
                                direction        = "out",
                                stringsAsFactors = FALSE)
            }

            return(cbind(df2, df1))
        }

        ## No contacts, return a zero row data.frame
        data.frame(root             = character(0),
                   inBegin          = as.Date(character(0)),
                   inEnd            = as.Date(character(0)),
                   outBegin         = as.Date(character(0)),
                   outEnd           = as.Date(character(0)),
                   direction        = character(0),
                   source           = character(0),
                   destination      = character(0),
                   t                = as.Date(character(0)),
                   id               = character(0),
                   n                = numeric(0),
                   category         = character(0),
                   stringsAsFactors = FALSE)
    }
)
