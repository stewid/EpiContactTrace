context('Contact Tracing')

test_that('Missing parameters', {
    expect_that(IngoingContactChain(data.frame(source=1L, destination=2L, t=as.Date('2011-08-10')),
                                    tEnd='2005-10-31',
                                    days=90),
                throws_error('Missing parameters in call to IngoingContactChain'))

    expect_that(IngoingContactChain(data.frame(source=1L, destination=2L, t=as.Date('2011-08-10')),
                                    root=1,
                                    days=90),
                throws_error('Use either tEnd and days or inBegin, inEnd, outBegin and outEnd in call to NetworkSummary'))

    expect_that(IngoingContactChain(data.frame(source=1L, destination=2L, t=as.Date('2011-08-10')),
                                    root=1,
                                    tEnd='2005-10-31'),
                throws_error('Use either tEnd and days or inBegin, inEnd, outBegin and outEnd in call to NetworkSummary'))

    expect_that(OutgoingContactChain(data.frame(source=1L, destination=2L, t=as.Date('2011-08-10')),
                                     tEnd='2005-10-31',
                                     days=90),
                throws_error('Missing parameters in call to OutgoingContactChain'))

    expect_that(OutgoingContactChain(data.frame(source=1L, destination=2L, t=as.Date('2011-08-10')),
                                     root=1,
                                     days=90),
                throws_error('Use either tEnd and days or inBegin, inEnd, outBegin and outEnd in call to NetworkSummary'))

    expect_that(OutgoingContactChain(data.frame(source=1L, destination=2L, t=as.Date('2011-08-10')),
                                     root=1,
                                     tEnd='2005-10-31'),
                throws_error('Use either tEnd and days or inBegin, inEnd, outBegin and outEnd in call to NetworkSummary'))

    expect_that(InDegree(data.frame(source=1L, destination=2L, t=as.Date('2011-08-10')),
                         tEnd='2005-10-31',
                         days=90),
                throws_error('Missing parameters in call to InDegree'))

    expect_that(InDegree(data.frame(source=1L, destination=2L, t=as.Date('2011-08-10')),
                         root=1,
                         days=90),
                throws_error('Use either tEnd and days or inBegin, inEnd, outBegin and outEnd in call to NetworkSummary'))

    expect_that(InDegree(data.frame(source=1L, destination=2L, t=as.Date('2011-08-10')),
                         root=1,
                         tEnd='2005-10-31'),
                throws_error('Use either tEnd and days or inBegin, inEnd, outBegin and outEnd in call to NetworkSummary'))

    expect_that(OutDegree(data.frame(source=1L, destination=2L, t=as.Date('2011-08-10')),
                          tEnd='2005-10-31',
                          days=90),
                throws_error('Missing parameters in call to OutDegree'))

    expect_that(OutDegree(data.frame(source=1L, destination=2L, t=as.Date('2011-08-10')),
                          root=1,
                          days=90),
                throws_error('Use either tEnd and days or inBegin, inEnd, outBegin and outEnd in call to NetworkSummary'))

    expect_that(OutDegree(data.frame(source=1L, destination=2L, t=as.Date('2011-08-10')),
                          root=1,
                          tEnd='2005-10-31'),
                throws_error('Use either tEnd and days or inBegin, inEnd, outBegin and outEnd in call to NetworkSummary'))
})

test_that('Trace parameter validation', {
    expect_that(Trace(), throws_error('Missing parameters in call to Trace'))

    expect_that(Trace(movement=data.frame(source=1L, destination=2L, t=as.Date('2011-08-10')),
                      tEnd='2005-10-31',
                      days=90),
                throws_error('Missing parameters in call to Trace'))

    expect_that(Trace(movement=data.frame(source=1L, destination=2L, t=as.Date('2011-08-10')),
                      root=1,
                      days=90),
                throws_error('Use either tEnd and days or inBegin, inEnd, outBegin and outEnd in call to Trace'))

    expect_that(Trace(movement=data.frame(source=1L, destination=2L, t=as.Date('2011-08-10')),
                      root=1,
                      tEnd='2005-10-31'),
                throws_error('Use either tEnd and days or inBegin, inEnd, outBegin and outEnd in call to Trace'))

    expect_that(Trace(movements=1:3,
                      root=1L,
                      inBegin=as.Date('2011-08-10'),
                      inEnd=as.Date('2011-08-10'),
                      outBegin=as.Date('2011-08-10'),
                      outEnd=as.Date('2011-08-10')),
                throws_error('movements must be a data.frame'))

    expect_that(Trace(movements=data.frame(destination=1, t=1),
                      root=1L,
                      inBegin=as.Date('2011-08-10'),
                      inEnd=as.Date('2011-08-10'),
                      outBegin=as.Date('2011-08-10'),
                      outEnd=as.Date('2011-08-10')),
                throws_error('movements must contain the columns source, destination and t.'))

    expect_that(Trace(movements=data.frame(source=1, t=1),
                      root=1L,
                      inBegin=as.Date('2011-08-10'),
                      inEnd=as.Date('2011-08-10'),
                      outBegin=as.Date('2011-08-10'),
                      outEnd=as.Date('2011-08-10')),
                throws_error('movements must contain the columns source, destination and t.'))

    expect_that(Trace(movements=data.frame(source=1, destination=1),
                      root=1L,
                      inBegin=as.Date('2011-08-10'),
                      inEnd=as.Date('2011-08-10'),
                      outBegin=as.Date('2011-08-10'),
                      outEnd=as.Date('2011-08-10')),
                throws_error('movements must contain the columns source, destination and t.'))

    expect_that(Trace(movements=data.frame(source=1L, destination=2L, t=1),
                      root=1L,
                      inBegin=as.Date('2011-08-10'),
                      inEnd=as.Date('2011-08-10'),
                      outBegin=as.Date('2011-08-10'),
                      outEnd=as.Date('2011-08-10')),
                throws_error('invalid class of column t in movements'))

    expect_that(Trace(movements=data.frame(source=c(1L,NA), destination=c(2L,3L), t=c('2011-08-10', '2011-08-10')),
                      root=1L,
                      inBegin=as.Date('2011-08-10'),
                      inEnd=as.Date('2011-08-10'),
                      outBegin=as.Date('2011-08-10'),
                      outEnd=as.Date('2011-08-10')),
                throws_error('source in movements contains NA'))

    expect_that(Trace(movements=data.frame(source=c(1L,2L), destination=c(2L,NA), t=c('2011-08-10', '2011-08-10')),
                      root=1L,
                      inBegin=as.Date('2011-08-10'),
                      inEnd=as.Date('2011-08-10'),
                      outBegin=as.Date('2011-08-10'),
                      outEnd=as.Date('2011-08-10')),
                throws_error('destination in movements contains NA'))

    expect_that(Trace(movements=data.frame(source=c(1L,2L), destination=c(2L,3L), t=c('2011-08-10', NA)),
                      root=1L,
                      inBegin=as.Date('2011-08-10'),
                      inEnd=as.Date('2011-08-10'),
                      outBegin=as.Date('2011-08-10'),
                      outEnd=as.Date('2011-08-10')),
                throws_error('t in movements contains NA'))

    expect_that(Trace(movements=data.frame(source=c(1L,2L), destination=c(2L,3L), t=c('2011-08-10', NA)),
                      root=1L,
                      inBegin=as.Date('2011-08-10'),
                      inEnd=as.Date('2011-08-10'),
                      outBegin=as.Date('2011-08-10'),
                      outEnd=as.Date('2011-08-10')),
                throws_error('t in movements contains NA'))

    expect_that(Trace(movements=data.frame(source=1L, destination=2L, t=as.Date('2011-08-10'), n='3'),
                      root=1L,
                      inBegin=as.Date('2011-08-10'),
                      inEnd=as.Date('2011-08-10'),
                      outBegin=as.Date('2011-08-10'),
                      outEnd=as.Date('2011-08-10')),
                throws_error('invalid class of column n in movements'))

    expect_that(Trace(movements=data.frame(source=1L, destination=2L, t=as.Date('2011-08-10'), n=3L, id=4),
                      root=1L,
                      inBegin=as.Date('2011-08-10'),
                      inEnd=as.Date('2011-08-10'),
                      outBegin=as.Date('2011-08-10'),
                      outEnd=as.Date('2011-08-10')),
                throws_error('invalid class of column id in movements'))

    expect_that(Trace(movements=data.frame(source=1L, destination=2L, t=as.Date('2011-08-10'), n=3L, id=4L, category=3),
                      root=1L,
                      inBegin=as.Date('2011-08-10'),
                      inEnd=as.Date('2011-08-10'),
                      outBegin=as.Date('2011-08-10'),
                      outEnd=as.Date('2011-08-10')),
                throws_error('invalid class of column category in movements'))

    expect_that(Trace(movements=data.frame(source=1L, destination=2L, t=as.Date('2011-08-10')),
                      root=1.1,
                      inBegin=as.Date('2011-08-10'),
                      inEnd=as.Date('2011-08-10'),
                      outBegin=as.Date('2011-08-10'),
                      outEnd=as.Date('2011-08-10')),
                throws_error("'root' must be an integer or character"))

    expect_that(Trace(movements=data.frame(source=1L, destination=2L, t=as.Date('2011-08-10')),
                      root=1L,
                      inBegin=2011,
                      inEnd=as.Date('2011-08-10'),
                      outBegin=as.Date('2011-08-10'),
                      outEnd=as.Date('2011-08-10')),
                throws_error("'inBegin' must be a Date vector"))

    expect_that(Trace(movements=data.frame(source=1L, destination=2L, t=as.Date('2011-08-10')),
                      root=1L,
                      inBegin=as.Date('2011-08-10'),
                      inEnd=2011,
                      outBegin=as.Date('2011-08-10'),
                      outEnd=as.Date('2011-08-10')),
                throws_error("'inEnd' must be a Date vector"))

    expect_that(Trace(movements=data.frame(source=1L, destination=2L, t=as.Date('2011-08-10')),
                      root=1L,
                      inBegin=as.Date('2011-08-10'),
                      inEnd=as.Date('2011-08-10'),
                      outBegin=2011,
                      outEnd=as.Date('2011-08-10')),
                throws_error("'outBegin' must be a Date vector"))

     expect_that(Trace(movements=data.frame(source=1L, destination=2L, t=as.Date('2011-08-10')),
                       root=1L,
                       inBegin=as.Date('2011-08-10'),
                       inEnd=as.Date('2011-08-10'),
                       outBegin=as.Date('2011-08-10'),
                       outEnd=2011),
                throws_error("'outEnd' must be a Date vector"))

     expect_that(Trace(movements=data.frame(source=1L, destination=2L, t=as.Date('2011-08-10')),
                       root=1L,
                       inBegin=as.Date('2011-08-10'),
                       inEnd=as.Date('2011-07-10'),
                       outBegin=as.Date('2011-08-10'),
                       outEnd=as.Date('2011-08-10')),
                throws_error('inEnd < inBegin'))

     expect_that(Trace(movements=data.frame(source=1L, destination=2L, t=as.Date('2011-08-10')),
                       root=1L,
                       inBegin=as.Date('2011-08-10'),
                       inEnd=as.Date('2011-08-10'),
                       outBegin=as.Date('2011-08-10'),
                       outEnd=as.Date('2011-07-10')),
                throws_error('outEnd < outBegin'))
})

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

test_that('IngoingContactChain', {
  load(file="data/movements1.rda")

  ct <- Trace(movements1,
              root = 8L,
              inBegin = as.Date('2010-08-22'),
              inEnd = as.Date('2010-10-01'),
              outBegin = as.Date('2010-08-01'),
              outEnd = as.Date('2010-08-31'))

  expect_that(IngoingContactChain(ct)$ingoingContactChain, is_identical_to(7L))
  expect_that(OutgoingContactChain(ct)$outgoingContactChain, is_identical_to(0L))

  load(file="data/movements2.rda")

  ct <- Trace(movements2,
              root = 4L,
              inBegin = as.Date('2010-07-22'),
              inEnd = as.Date('2010-08-21'),
              outBegin = as.Date('2010-08-01'),
              outEnd = as.Date('2010-08-31'))

  expect_that(IngoingContactChain(ct)$ingoingContactChain, is_identical_to(3L))
  expect_that(OutgoingContactChain(ct)$outgoingContactChain, is_identical_to(0L))

  load(file="data/movements5.rda")

  ct <- Trace(movements5,
              root = 1L,
              inBegin = as.Date('2010-08-02'),
              inEnd = as.Date('2010-09-01'),
              outBegin = as.Date('2010-09-01'),
              outEnd = as.Date('2010-10-01'))

  expect_that(IngoingContactChain(ct)$ingoingContactChain, is_identical_to(1L))
  expect_that(OutgoingContactChain(ct)$outgoingContactChain, is_identical_to(0L))
})

test_that('OutgoingContactChain', {
  load(file="data/movements4.rda")

  ct <- Trace(movements4,
              root = 1L,
              inBegin = as.Date('2010-08-02'),
              inEnd = as.Date('2010-09-01'),
              outBegin = as.Date('2010-08-01'),
              outEnd = as.Date('2010-11-09'))

  expect_that(IngoingContactChain(ct)$ingoingContactChain, is_identical_to(0L))
  expect_that(OutgoingContactChain(ct)$outgoingContactChain, is_identical_to(7L))

  load(file="data/movements5.rda")

  ct <- Trace(movements5,
              root = 1L,
              inBegin = as.Date('2010-07-02'),
              inEnd = as.Date('2010-08-01'),
              outBegin = as.Date('2010-08-01'),
              outEnd = as.Date('2010-08-31'))

  expect_that(IngoingContactChain(ct)$ingoingContactChain, is_identical_to(0L))
  expect_that(OutgoingContactChain(ct)$outgoingContactChain, is_identical_to(1L))

  load(file="data/movements7.rda")

  ct <- Trace(movements7,
              root = 1L,
              inBegin = as.Date('2010-10-10'),
              inEnd = as.Date('2010-10-20'),
              outBegin = as.Date('2010-10-10'),
              outEnd = as.Date('2010-10-20'))

  expect_that(IngoingContactChain(ct)$ingoingContactChain, is_identical_to(0L))
  expect_that(OutgoingContactChain(ct)$outgoingContactChain, is_identical_to(2L))

  expect_that(NetworkSummary(movements7,
                             root=1,
                             tEnd='2010-10-20',
                             days=10),
              is_identical_to(data.frame(root="1",
                                         inBegin=as.Date("2010-10-10"),
                                         inEnd=as.Date("2010-10-20"),
                                         inDays=10L,
                                         outBegin=as.Date("2010-10-10"),
                                         outEnd=as.Date("2010-10-20"),
                                         outDays=10L,
                                         inDegree=0L,
                                         outDegree=1L,
                                         ingoingContactChain=0L,
                                         outgoingContactChain=2L)))
})

test_that('InDegree', {
  load(file="data/movements2.rda")

  ct <- Trace(movements2,
              root = 4L,
              inBegin = as.Date('2010-08-02'),
              inEnd = as.Date('2010-09-01'),
              outBegin = as.Date('2010-08-01'),
              outEnd = as.Date('2010-08-31'))

  expect_that(InDegree(ct)$inDegree, is_identical_to(1L))
  expect_that(OutDegree(ct)$outDegree, is_identical_to(0L))

  ## Test with NetworkSummary
  expect_that(NetworkSummary(movements2,
                             root = 4,
                             tEnd = '2010-09-01',
                             days = 30)$inDegree,
              is_identical_to(1L))

  expect_that(NetworkSummary(movements2,
                             root = 4,
                             tEnd = '2010-08-31',
                             days = 30)$outDegree,
              is_identical_to(0L))

  ct <- Trace(movements2,
              root = 4L,
              inBegin = as.Date('2010-08-27'),
              inEnd = as.Date('2010-09-01'),
              outBegin = as.Date('2010-08-01'),
              outEnd = as.Date('2010-08-31'))

  expect_that(InDegree(ct)$inDegree, is_identical_to(0L))
  expect_that(OutDegree(ct)$outDegree, is_identical_to(0L))

  ## Test with NetworkSummary
  expect_that(NetworkSummary(movements2,
                             root = 4,
                             tEnd = '2010-09-01',
                             days = 5)$inDegree,
              is_identical_to(0L))

  expect_that(NetworkSummary(movements2,
                             root = 4,
                             tEnd = '2010-08-31',
                             days = 30)$outDegree,
              is_identical_to(0L))
})

test_that('OutDegree', {
  load(file="data/movements3.rda")

  ct <- Trace(movements3,
              root = 1L,
              inBegin = as.Date('2010-08-02'),
              inEnd = as.Date('2010-09-01'),
              outBegin = as.Date('2010-08-01'),
              outEnd = as.Date('2010-08-31'))

  expect_that(InDegree(ct)$inDegree, is_identical_to(0L))
  expect_that(OutDegree(ct)$outDegree, is_identical_to(3L))

  ## Test with NetworkSummary
  expect_that(NetworkSummary(movements3,
                             root = 1,
                             tEnd = '2010-09-01',
                             days = 30)$inDegree,
              is_identical_to(0L))

  expect_that(NetworkSummary(movements3,
                             root = 1,
                             tEnd = '2010-08-31',
                             days = 30)$outDegree,
              is_identical_to(3L))

  ct <- Trace(movements3,
              root = 1L,
              inBegin = as.Date('2010-08-02'),
              inEnd = as.Date('2010-09-01'),
              outBegin = as.Date('2010-08-01'),
              outEnd = as.Date('2010-08-16'))

  expect_that(InDegree(ct)$inDegree, is_identical_to(0L))
  expect_that(OutDegree(ct)$outDegree, is_identical_to(2L))

  ## Test with NetworkSummary
  expect_that(NetworkSummary(movements3,
                             root = 1,
                             tEnd = '2010-09-01',
                             days = 30)$inDegree,
              is_identical_to(0L))

  expect_that(NetworkSummary(movements3,
                             root = 1,
                             tEnd = '2010-08-16',
                             days = 15)$outDegree,
              is_identical_to(2L))
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

    ct.1.df <- ct.1.df[order(1:6),]
    ct.2.df <- ct.2.df[order(1:6),]

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

