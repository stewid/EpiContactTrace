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

library(EpiContactTrace)
data(transfers)

##
## Check shortest paths
##

##
## Case 1
##
sp.1 <- ShortestPaths(transfers, root=2645, tEnd='2005-10-31', days=90)
sp.2 <- ShortestPaths(Trace(movements=transfers, root=2645, tEnd='2005-10-31', days=90))
in.1 <- sp.1[sp.1$direction == 'in', c('source', 'distance')]
in.2 <- sp.2[sp.2$direction == 'in', c('source', 'distance')]
in.1 <- in.1[order(in.1$source, in.1$distance),]
in.2 <- in.2[order(in.2$source, in.2$distance),]
stopifnot(identical(in.1, in.2))
out.1 <- sp.1[sp.1$direction == 'out', c('destination', 'distance')]
out.2 <- sp.2[sp.2$direction == 'out', c('destination', 'distance')]
out.1 <- out.1[order(out.1$destination, out.1$distance),]
out.2 <- out.2[order(out.2$destination, out.2$distance),]
stopifnot(identical(out.1, out.2))
