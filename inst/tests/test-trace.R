context('Contact Tracing')

test_that('Direction', {
  load(file="data/movements1.rda")

  ct <- Trace(movements1,
              root = 4L,
              inBegin = as.Date('2010-08-02'),
              inEnd = as.Date('2010-09-01'),
              outBegin =  as.Date('2010-08-01'),
              outEnd = as.Date('2010-08-31'))

  expect_that(ct@ingoingContacts@direction, is_identical_to('in'))
  expect_that(ct@outgoingContacts@direction, is_identical_to('out'))

  load(file="data/movements2.rda")

  ct <- Trace(movements2,
              root = 4L,
              inBegin = as.Date('2010-08-02'),
              inEnd = as.Date('2010-09-01'),
              outBegin =  as.Date('2010-08-01'),
              outEnd = as.Date('2010-08-31'))

  expect_that(ct@ingoingContacts@direction, is_identical_to('in'))
  expect_that(ct@outgoingContacts@direction, is_identical_to('out'))
})

test_that('Root not in movements', {
  load(file="data/movements3.rda")

  ct <- Trace(movements3,
              root = 15L,
              inBegin = as.Date('2010-08-02'),
              inEnd = as.Date('2010-09-01'),
              outBegin = as.Date('2010-08-01'),
              outEnd = as.Date('2010-08-31'))

  expect_that(InDegree(ct)$inDegree, is_identical_to(0L))
  expect_that(OutDegree(ct)$outDegree, is_identical_to(0L))

  ## Test with NetworkSummary
  expect_that(NetworkSummary(movements3,
                             root = 15,
                             tEnd = '2010-09-01',
                             days = 30)$inDegree,
              is_identical_to(0L))

  expect_that(NetworkSummary(movements3,
                             root = 15,
                             tEnd = '2010-08-31',
                             days = 30)$outDegree,
              is_identical_to(0L))
})

test_that('Loops', {
    movements <- data.frame(source = c(2L, 2L),
                            destination = c(1L, 2L),
                            t = as.Date(c('2010-10-03', '2010-10-02')))

    ct <- Trace(movements,
                root = 1L,
                inBegin = as.Date('2010-09-30'),
                inEnd = as.Date('2010-10-05'),
                outBegin = as.Date('2010-10-05'),
                outEnd = as.Date('2010-10-10'))

    expect_that(ct@ingoingContacts@source, is_identical_to('2'))
    expect_that(ct@ingoingContacts@destination, is_identical_to('1'))

    ct <- Trace(movements,
                root = 2L,
                inBegin = as.Date('2010-09-30'),
                inEnd = as.Date('2010-10-05'),
                outBegin = as.Date('2010-09-30'),
                outEnd = as.Date('2010-10-10'))

    expect_that(ct@outgoingContacts@source, is_identical_to('2'))
    expect_that(ct@outgoingContacts@destination, is_identical_to('1'))
})

test_that('Duplicate movements', {
    load(file="data/movements6.rda")

    ct.1 <- Trace(movements6, 2645, '2005-10-31', 90)
    ct.1.df <- as(ct.1, 'data.frame')

    ct.2 <- Trace(ct.1.df, 2645, '2005-10-31', 90)
    ct.2.df <- as(ct.2, 'data.frame')

    ct.1.df <- ct.1.df[, c('source',
                           'destination',
                           't',
                           'id',
                           'n',
                           'category')]

    ct.2.df <- ct.2.df[, c('source',
                           'destination',
                           't',
                           'id',
                           'n',
                           'category')]

    ct.1.df <- ct.1.df[order(ct.1.df$source,
                             ct.1.df$destination,
                             ct.1.df$t,
                             ct.1.df$id,
                             ct.1.df$n,
                             ct.1.df$category),]

    ct.2.df <- ct.2.df[order(ct.2.df$source,
                             ct.2.df$destination,
                             ct.2.df$t,
                             ct.2.df$id,
                             ct.2.df$n,
                             ct.2.df$category),]

    rownames(ct.1.df) <- NULL
    rownames(ct.2.df) <- NULL

    expect_that(ct.2.df, is_identical_to(ct.1.df))
})

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
