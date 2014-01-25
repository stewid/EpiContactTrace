context('Contact Tracing')

test_that('Network summary', {
    load(file="data/transfers.rda")
    load(file="data/ns.rda")

    root <- sort(unique(c(transfers$source,
                          transfers$destination)))

    result <- NetworkSummary(transfers,
                             root=root,
                             tEnd='2005-10-31',
                             days=90)

    expect_that(NetworkSummary(transfers,
                               root=root,
                               tEnd='2005-10-31',
                               days=90),
                is_identical_to(ns))
})

test_that('Network summary using Trace', {
    load(file="data/transfers.rda")

    expect_that(NetworkSummary(Trace(transfers,
                                     root=584,
                                     tEnd='2005-10-31',
                                     days=91)),
                is_identical_to(NetworkSummary(transfers,
                                               root=584,
                                               tEnd='2005-10-31',
                                               days=91)))
})
