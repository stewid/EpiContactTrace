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

##
## Some misc checking
##

##
## Loops: Case 1
##
movements <- data.frame(source = c(2L, 2L),
                        destination = c(1L, 2L),
                        t = as.Date(c("2010-10-03", "2010-10-02")))

ct <- Trace(movements,
            root = 1L,
            inBegin = as.Date("2010-09-30"),
            inEnd = as.Date("2010-10-05"),
            outBegin = as.Date("2010-10-05"),
            outEnd = as.Date("2010-10-10"))

stopifnot(identical(ct@ingoingContacts@source, "2"))
stopifnot(identical(ct@ingoingContacts@destination, "1"))

##
## Loops: Case 2
##
movements <- data.frame(source = c(2L, 2L),
                        destination = c(1L, 2L),
                        t = as.Date(c("2010-10-03", "2010-10-02")))

ct <- Trace(movements,
            root = 2L,
            inBegin = as.Date("2010-09-30"),
            inEnd = as.Date("2010-10-05"),
            outBegin = as.Date("2010-09-30"),
            outEnd = as.Date("2010-10-10"))

stopifnot(identical(ct@outgoingContacts@source, "2"))
stopifnot(identical(ct@outgoingContacts@destination, "1"))

##
## Direction: Case 1
##
movements <- data.frame(
    source = 1:7,
    destination = c(4L, 5L, 5L, 6L, 8L, 8L, 8L),
    t = structure(c(14849, 14846, 14847, 14850, 14848, 14851, 14852),
                  class = "Date"))

ct <- Trace(movements,
            root = 4L,
            inBegin = as.Date("2010-08-02"),
            inEnd = as.Date("2010-09-01"),
            outBegin =  as.Date("2010-08-01"),
            outEnd = as.Date("2010-08-31"))

stopifnot(identical(ct@ingoingContacts@direction, "in"))
stopifnot(identical(ct@outgoingContacts@direction, "out"))

##
## Direction: Case 2
##
movements <- data.frame(
    source = c(1L, 2L, 3L, 3L),
    destination = c(3L, 3L, 4L, 4L),
    t = structure(c(14834, 14838, 14836, 14841), class = "Date"),
    individual = c(NA_character_, NA_character_, NA_character_,  NA_character_),
    n = c(NA_integer_, NA_integer_, NA_integer_, NA_integer_))

ct <- Trace(movements,
            root = 4L,
            inBegin = as.Date("2010-08-02"),
            inEnd = as.Date("2010-09-01"),
            outBegin =  as.Date("2010-08-01"),
            outEnd = as.Date("2010-08-31"))

stopifnot(identical(ct@ingoingContacts@direction, "in"))
stopifnot(identical(ct@outgoingContacts@direction, "out"))

##
## Root not in movements: Case 1
##
movements <- data.frame(
    source = c("1", "1", "1"),
    destination = c("2", "3", "4"),
    t = structure(c(14834, 14838, 14836), class = "Date"),
    individual = c(NA_character_, NA_character_, NA_character_),
    n = c(NA_integer_, NA_integer_, NA_integer_))

ct <- Trace(movements,
            root = 15L,
            inBegin = as.Date("2010-08-02"),
            inEnd = as.Date("2010-09-01"),
            outBegin = as.Date("2010-08-01"),
            outEnd = as.Date("2010-08-31"))

stopifnot(identical(InDegree(ct)$inDegree, 0L))
stopifnot(identical(OutDegree(ct)$outDegree, 0L))

##
## Root not in movements: Case 2
##
movements <- data.frame(
    source = c("1", "1", "1"),
    destination = c("2", "3", "4"),
    t = structure(c(14834, 14838, 14836), class = "Date"),
    individual = c(NA_character_, NA_character_, NA_character_),
    n = c(NA_integer_, NA_integer_, NA_integer_))

ns <- NetworkSummary(movements, root = 15, tEnd = "2010-09-01", days = 30)
stopifnot(identical(ns$inDegree, 0L))

##
## Root not in movements: Case 3
##
movements <- data.frame(
    source = c("1", "1", "1"),
    destination = c("2", "3", "4"),
    t = structure(c(14834, 14838, 14836), class = "Date"),
    individual = c(NA_character_, NA_character_, NA_character_),
    n = c(NA_integer_, NA_integer_, NA_integer_))

ns <- NetworkSummary(movements, root = 15, tEnd = "2010-08-31", days = 30)
stopifnot(identical(ns$outDegree, 0L))

##
## Duplicate movements: Case 1
##
movements <- data.frame(
    source = c("2019", "2019", "2019", "2019", "2019", "2019", "2019",
               "2019", "2019", "2019", "2036", "2036", "2036", "2036",
               "2036", "2036", "2036", "2036", "2036", "2036", "2036",
               "2036", "2036", "2036", "2036", "2357", "2357", "2846",
               "2846", "2846", "2846", "2847", "2852", "2825", "2823",
               "2839", "1375", "2357", "2357", "2357", "5615", "5615",
               "5615", "5615", "5615", "5615", "5615", "5615", "5615",
               "2890", "2645", "2645", "2645", "2645", "2645", "2645",
               "2645", "2645", "2645", "2645", "2645", "2645", "2821",
               "2821", "2645", "2825", "2825", "444", "4422", "4422",
               "4422", "4422", "4422", "4422", "4422", "4422", "4422",
               "4422", "1323", "1323", "1323", "1323", "1323", "1323",
               "1323", "1323", "1323", "1323", "1323", "1323", "1323",
               "1323", "1323", "1323", "1323", "1323", "1323", "1323",
               "1323", "1323", "1323", "1323", "1323", "1323", "1323",
               "1323", "1323", "1323", "1323", "1323", "1323", "1323",
               "1323", "1323", "1323", "1323", "1323", "1323", "1323",
               "4422", "4422", "4422", "4422", "4422", "2645", "2645",
               "2645", "2645", "2645", "2645", "2645", "585", "585",
               "585", "585", "585", "585", "585", "585", "585", "585",
               "585", "585", "585", "585", "264", "264", "264", "264",
               "264", "264", "264", "264", "264", "264", "264", "264",
               "264", "264", "264", "2645", "2645", "2645", "2645",
               "2645", "2645", "2645", "2645", "2645", "2645"),
    destination = c("2645", "2645", "2645", "2645", "2645", "2645",
                    "2645", "2645", "2645", "2645", "2645", "2645",
                    "2645", "2645", "2645", "2645", "2645", "2645",
                    "2645", "2645", "2645", "2645", "2645", "2645",
                    "2645", "2645", "2645", "2645", "2645", "2645",
                    "2645", "2645", "2645", "2852", "2825", "2825",
                    "2839", "2839", "2839", "2839", "2839", "2839",
                    "2839", "2839", "2839", "2839", "2839", "2839",
                    "2839", "2825", "10644", "10644", "10644",
                    "10644", "10644", "10697", "2821", "2821", "2821",
                    "2821", "2821", "2821", "2820", "2880", "2825",
                    "2823", "444", "4422", "10071", "10071", "10071",
                    "10072", "10072", "10072", "10072", "10072",
                    "1323", "1323", "10071", "10071", "10071",
                    "10071", "10071", "10195", "10195", "10195",
                    "10195", "10195", "10196", "3354", "3354", "3354",
                    "3354", "3354", "3354", "3354", "3354", "3354",
                    "3354", "3354", "3354", "8750", "8750", "8750",
                    "8750", "8750", "8750", "8750", "8750", "8750",
                    "8750", "8750", "8750", "8750", "8750", "8750",
                    "8750", "8750", "8750", "3362", "3362", "3362",
                    "3362", "3362", "2839", "585", "585", "585",
                    "585", "585", "585", "264", "264", "264", "264",
                    "264", "264", "264", "264", "264", "264", "264",
                    "264", "264", "264", "584", "584", "584", "584",
                    "584", "584", "584", "584", "584", "584", "584",
                    "584", "584", "584", "584", "9789", "9789",
                    "9789", "9789", "9789", "9789", "9789", "9789",
                    "9789", "9966"),
    t = structure(c(13071, 13071, 13071, 13071, 13071, 13071, 13071,
                    13071, 13071, 13071, 13080, 13080, 13080, 13080,
                    13080, 13080, 13080, 13080, 13080, 13080, 13080,
                    13080, 13080, 13080, 13080, 13054, 13078, 13045,
                    13045, 13045, 13045, 13078, 13078, 13078, 13078,
                    13078, 13078, 13070, 13070, 13070, 13070, 13070,
                    13070, 13070, 13070, 13070, 13070, 13070, 13070,
                    13078, 13069, 13069, 13069, 13069, 13069, 13078,
                    13011, 13011, 13011, 13011, 13011, 13011, 13034,
                    13034, 13078, 13078, 13078, 13078, 13081, 13081,
                    13081, 13085, 13085, 13085, 13085, 13085, 13080,
                    13080, 13083, 13083, 13083, 13083, 13083, 13081,
                    13081, 13081, 13083, 13081, 13082, 13081, 13081,
                    13081, 13081, 13081, 13081, 13081, 13081, 13081,
                    13081, 13081, 13081, 13087, 13087, 13087, 13087,
                    13087, 13087, 13087, 13087, 13087, 13087, 13087,
                    13087, 13087, 13087, 13087, 13087, 13087, 13087,
                    13085, 13085, 13085, 13085, 13085, 13078, 13083,
                    13083, 13083, 13083, 13083, 13083, 13087, 13087,
                    13087, 13087, 13087, 13087, 13087, 13087, 13087,
                    13087, 13087, 13087, 13087, 13087, 13087, 13087,
                    13087, 13087, 13087, 13087, 13087, 13087, 13087,
                    13087, 13087, 13087, 13087, 13087, 13087, 13049,
                    13049, 13049, 13049, 13049, 13049, 13049, 13049,
                    13049, 13043), class = "Date"),
    id = c("034A8", "034A9", "034AA", "034AB", "034AC", "034AD",
           "034AE", "034AF", "034B0", "034B1", "0355B", "04EBB", "05697",
           "061B0", "06518", "09193", "0A9B3", "0A9B4", "0A9B5", "0A9B6",
           "0A9B7", "0A9B8", "0A9B9", "0A9BA", "0A9BB", "05D5F", "0A9BF",
           "0944D", "0A9A1", "0A9A2", "0A9BC", "0A9BD", "0A9BE", "06551",
           "06414", "06552", "0A34A", "0A354", "0A355", "0A356", "0A34B",
           "0A34C", "0A34D", "0A34E", "0A34F", "0A350", "0A351", "0A352",
           "0A353", "06554", "0A5E0", "0A5E1", "0A5E2", "0A5E3", "0A5E4",
           "0A9A9", "044F8", "04A07", "04A09", "04A0A", "04A0B", "04A0C",
           "049E3", "04A4F", "06553", "06416", "06550", "075C9", "075BB",
           "075BF", "075C4", "075C1", "075C5", "075C6", "075C7", "075C8",
           "075B9", "075BE", "0A9E9", "0A9EC", "0A9ED", "0A9EE", "0A9F2",
           "07F27", "07F28", "07F29", "07F2A", "07F2B", "07F2D", "0554F",
           "05550", "0556D", "0556E", "0556F", "05570", "05571", "05572",
           "05573", "05574", "05575", "05576", "0210D", "0210E", "0210F",
           "02110", "02111", "02112", "02113", "02114", "02115", "02116",
           "02117", "02118", "02119", "0211A", "0211B", "0211C", "0211D",
           "0211E", "075BA", "075BC", "075BD", "075C0", "075C2", "0A349",
           "0A9A8", "0A9AA", "0A9AB", "0A9AC", "0A9AD", "0A9AE", "00EF8",
           "00EF9", "00EFA", "00EFB", "00EFC", "00EFD", "00EFE", "00EFF",
           "00F00", "00F01", "00F02", "00F03", "00F04", "00F05", "00EF2",
           "00EF3", "00EF5", "00EF6", "00EF7", "00F06", "00F07", "00F09",
           "00F0A", "00F0B", "00F0D", "00F0E", "00F10", "00F12", "00F13",
           "0A9A3", "0A9A4", "0A9A5", "0A9A6", "0A9A7", "0A9AF", "0A9B0",
           "0A9B1", "0A9B2", "06E4F"),
    n = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
          1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
          1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
          1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
          1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
          1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
          1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
          1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
          1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
    category = c("Cattle", "Cattle", "Cattle", "Cattle", "Cattle",
                 "Cattle", "Cattle", "Cattle", "Cattle", "Cattle",
                 "Cattle", "Cattle", "Cattle", "Cattle", "Cattle",
                 "Cattle", "Cattle", "Cattle", "Cattle", "Cattle",
                 "Cattle", "Cattle", "Cattle", "Cattle", "Cattle",
                 "Cattle", "Cattle", "Cattle", "Cattle", "Cattle",
                 "Cattle", "Cattle", "Cattle", "Cattle", "Cattle",
                 "Cattle", "Cattle", "Cattle", "Cattle", "Cattle",
                 "Cattle", "Cattle", "Cattle", "Cattle", "Cattle",
                 "Cattle", "Cattle", "Cattle", "Cattle", "Cattle",
                 "Cattle", "Cattle", "Cattle", "Cattle", "Cattle",
                 "Cattle", "Cattle", "Cattle", "Cattle", "Cattle",
                 "Cattle", "Cattle", "Cattle", "Cattle", "Cattle",
                 "Cattle", "Cattle", "Cattle", "Cattle", "Cattle",
                 "Cattle", "Cattle", "Cattle", "Cattle", "Cattle",
                 "Cattle", "Cattle", "Cattle", "Cattle", "Cattle",
                 "Cattle", "Cattle", "Cattle", "Cattle", "Cattle",
                 "Cattle", "Cattle", "Cattle", "Cattle", "Cattle",
                 "Cattle", "Cattle", "Cattle", "Cattle", "Cattle",
                 "Cattle", "Cattle", "Cattle", "Cattle", "Cattle",
                 "Cattle", "Cattle", "Cattle", "Cattle", "Cattle",
                 "Cattle", "Cattle", "Cattle", "Cattle", "Cattle",
                 "Cattle", "Cattle", "Cattle", "Cattle", "Cattle",
                 "Cattle", "Cattle", "Cattle", "Cattle", "Cattle",
                 "Cattle", "Cattle", "Cattle", "Cattle", "Cattle",
                 "Cattle", "Cattle", "Cattle", "Cattle", "Cattle",
                 "Cattle", "Cattle", "Cattle", "Cattle", "Cattle",
                 "Cattle", "Cattle", "Cattle", "Cattle", "Cattle",
                 "Cattle", "Cattle", "Cattle", "Cattle", "Cattle",
                 "Cattle", "Cattle", "Cattle", "Cattle", "Cattle",
                 "Cattle", "Cattle", "Cattle", "Cattle", "Cattle",
                 "Cattle", "Cattle", "Cattle", "Cattle", "Cattle",
                 "Cattle", "Cattle", "Cattle", "Cattle", "Cattle",
                 "Cattle", "Cattle", "Cattle", "Cattle", "Cattle"))

ct.1 <- Trace(movements, 2645, "2005-10-31", 90)
ct.1.df <- as(ct.1, "data.frame")

ct.2 <- Trace(ct.1.df, 2645, "2005-10-31", 90)
ct.2.df <- as(ct.2, "data.frame")

ct.1.df <- ct.1.df[, c("source",
                       "destination",
                       "t",
                       "id",
                       "n",
                       "category")]

ct.2.df <- ct.2.df[, c("source",
                       "destination",
                       "t",
                       "id",
                       "n",
                       "category")]

ct.1.df <- ct.1.df[order(ct.1.df$source,
                         ct.1.df$destination,
                         ct.1.df$t,
                         ct.1.df$id,
                         ct.1.df$n,
                         ct.1.df$category), ]

ct.2.df <- ct.2.df[order(ct.2.df$source,
                         ct.2.df$destination,
                         ct.2.df$t,
                         ct.2.df$id,
                         ct.2.df$n,
                         ct.2.df$category), ]

rownames(ct.1.df) <- NULL
rownames(ct.2.df) <- NULL

stopifnot(identical(ct.2.df, ct.1.df))
