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

library(EpiContactTrace)
data(transfers)

##
## Check shortest paths
##

##
## Case 1
##
sp_1 <- ShortestPaths(transfers, root = 2645, tEnd = "2005-10-31", days = 90)
sp_2 <- ShortestPaths(Trace(movements = transfers,
                            root = 2645,
                            tEnd = "2005-10-31",
                            days = 90))
in_1 <- sp_1[sp_1$direction == "in", c("source", "distance")]
in_2 <- sp_2[sp_2$direction == "in", c("source", "distance")]
in_1 <- in_1[order(in_1$source, in_1$distance), ]
in_2 <- in_2[order(in_2$source, in_2$distance), ]
rownames(in_1) <- NULL
rownames(in_2) <- NULL
stopifnot(identical(in_1, in_2))
out_1 <- sp_1[sp_1$direction == "out", c("destination", "distance")]
out_2 <- sp_2[sp_2$direction == "out", c("destination", "distance")]
out_1 <- out_1[order(out_1$destination, out_1$distance), ]
out_2 <- out_2[order(out_2$destination, out_2$distance), ]
rownames(out_1) <- NULL
rownames(out_2) <- NULL
stopifnot(identical(out_1, out_2))

##
## Case 2
##
sp_in_exp <- data.frame(
    root = c("100", "100", "100", "100", "100"),
    source = c("54", "262", "356", "357", "358"),
    distance = c(1L, 1L, 1L, 1L, 1L),
    stringsAsFactors = FALSE)

sp_in <- ShortestPaths(Trace(movements = transfers,
                             root = 100,
                             tEnd = "2005-10-31",
                             days = 90))
sp_in <- sp_in[sp_in$direction == "in", c("root", "source", "distance")]
sp_in <- sp_in[order(as.numeric(sp_in$source)), ]
rownames(sp_in) <- NULL
stopifnot(identical(sp_in, sp_in_exp))

sp_out_exp <- data.frame(
    root = c("100", "100", "100", "100", "100", "100",
        "100", "100", "100", "100", "100", "100", "100", "100"),
    destination = c("101", "357", "358", "2508", "8239", "8243",
        "8327", "8356", "8388", "8420", "8991", "9003", "9087", "9110"),
    distance = c(1L, 1L, 1L, 2L, 1L, 2L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L),
    stringsAsFactors = FALSE)

sp_out <- ShortestPaths(Trace(movements = transfers,
                              root = 100,
                              tEnd = "2005-10-31",
                              days = 90))
sp_out <- sp_out[sp_out$direction == "out",
                 c("root", "destination", "distance")]
sp_out <- sp_out[order(as.numeric(sp_out$destination)), ]
rownames(sp_out) <- NULL
stopifnot(identical(sp_out, sp_out_exp))
