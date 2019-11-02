## Copyright 2013-2019 Stefan Widgren and Maria Noremark,
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

## Load data
data(transfers)

## Perform contact tracing
contactTrace <- Trace(movements = transfers,
                      root = 2645,
                      tEnd = "2005-10-31",
                      days = 90)

## Generate an html report showing details of the contact tracing for
## root 2646.
## Creates the file 2645.html in the temporary directory.
Report(contactTrace, dir = tempdir())

## Drop line with version and time-stamp
lines <- readLines(file.path(tempdir(), "2645.html"))
lines[c(-9, -1948)]

## It's possible to generate reports for a list of ContactTrace
## objects.  Perform contact tracing for two of the included herds
root <- 1:2

## Perform contact tracing
contactTrace <- Trace(movements = transfers,
                      root = root,
                      tEnd = "2005-10-31",
                      days = 90)

## Generate reports Creates the files 1.html, 2645.html in the
## temporary directory
Report(contactTrace, dir = tempdir())

lines <- readLines(file.path(tempdir(), "1.html"))
lines[c(-9, -97)]
