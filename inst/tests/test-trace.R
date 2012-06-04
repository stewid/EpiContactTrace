context('Contact Tracing')

test_that('TraceDateInterval parameter validation', {
    expect_that(TraceDateInterval(), throws_error('Missing parameters in call to TraceDateInterval'))

    expect_that(TraceDateInterval(movements=1:3,
                                  root=1L,
                                  inBegin=as.Date('2011-08-10'),
                                  inEnd=as.Date('2011-08-10'),
                                  outBegin=as.Date('2011-08-10'),
                                  outEnd=as.Date('2011-08-10')),
                throws_error('movements must be a data.frame'))

    expect_that(TraceDateInterval(movements=data.frame(destination=1, t=1),
                                  root=1L,
                                  inBegin=as.Date('2011-08-10'),
                                  inEnd=as.Date('2011-08-10'),
                                  outBegin=as.Date('2011-08-10'),
                                  outEnd=as.Date('2011-08-10')),
                throws_error('movements must contain the columns source, destination and t.'))

    expect_that(TraceDateInterval(movements=data.frame(source=1, t=1),
                                  root=1L,
                                  inBegin=as.Date('2011-08-10'),
                                  inEnd=as.Date('2011-08-10'),
                                  outBegin=as.Date('2011-08-10'),
                                  outEnd=as.Date('2011-08-10')),
                throws_error('movements must contain the columns source, destination and t.'))

    expect_that(TraceDateInterval(movements=data.frame(source=1, destination=1),
                                  root=1L,
                                  inBegin=as.Date('2011-08-10'),
                                  inEnd=as.Date('2011-08-10'),
                                  outBegin=as.Date('2011-08-10'),
                                  outEnd=as.Date('2011-08-10')),
                throws_error('movements must contain the columns source, destination and t.'))

    expect_that(TraceDateInterval(movements=data.frame(source=1L, destination=2L, t='2011-08-10'),
                                  root=1L,
                                  inBegin=as.Date('2011-08-10'),
                                  inEnd=as.Date('2011-08-10'),
                                  outBegin=as.Date('2011-08-10'),
                                  outEnd=as.Date('2011-08-10')),
                throws_error('invalid class of column t in movements'))

    expect_that(TraceDateInterval(movements=data.frame(source=1L, destination=2L, t=as.Date('2011-08-10'), n='3'),
                                  root=1L,
                                  inBegin=as.Date('2011-08-10'),
                                  inEnd=as.Date('2011-08-10'),
                                  outBegin=as.Date('2011-08-10'),
                                  outEnd=as.Date('2011-08-10')),
                throws_error('invalid class of column n in movements'))

    expect_that(TraceDateInterval(movements=data.frame(source=1L, destination=2L, t=as.Date('2011-08-10'), n=3L, id=4),
                                  root=1L,
                                  inBegin=as.Date('2011-08-10'),
                                  inEnd=as.Date('2011-08-10'),
                                  outBegin=as.Date('2011-08-10'),
                                  outEnd=as.Date('2011-08-10')),
                throws_error('invalid class of column id in movements'))

    expect_that(TraceDateInterval(movements=data.frame(source=1L, destination=2L, t=as.Date('2011-08-10'), n=3L, id=4L, category=3),
                                  root=1L,
                                  inBegin=as.Date('2011-08-10'),
                                  inEnd=as.Date('2011-08-10'),
                                  outBegin=as.Date('2011-08-10'),
                                  outEnd=as.Date('2011-08-10')),
                throws_error('invalid class of column category in movements'))

    expect_that(TraceDateInterval(movements=data.frame(source=1L, destination=2L, t=as.Date('2011-08-10')),
                                  root=1.1,
                                  inBegin=as.Date('2011-08-10'),
                                  inEnd=as.Date('2011-08-10'),
                                  outBegin=as.Date('2011-08-10'),
                                  outEnd=as.Date('2011-08-10')),
                throws_error("'root' must be an integer or character"))

    expect_that(TraceDateInterval(movements=data.frame(source=1L, destination=2L, t=as.Date('2011-08-10')),
                                  root=1L,
                                  inBegin=2011,
                                  inEnd=as.Date('2011-08-10'),
                                  outBegin=as.Date('2011-08-10'),
                                  outEnd=as.Date('2011-08-10')),
                throws_error('inBegin must be a Date vector'))

    expect_that(TraceDateInterval(movements=data.frame(source=1L, destination=2L, t=as.Date('2011-08-10')),
                                  root=1L,
                                  inBegin=as.Date('2011-08-10'),
                                  inEnd=2011,
                                  outBegin=as.Date('2011-08-10'),
                                  outEnd=as.Date('2011-08-10')),
                throws_error('inEnd must be a Date vector'))

    expect_that(TraceDateInterval(movements=data.frame(source=1L, destination=2L, t=as.Date('2011-08-10')),
                                  root=1L,
                                  inBegin=as.Date('2011-08-10'),
                                  inEnd=as.Date('2011-08-10'),
                                  outBegin=2011,
                                  outEnd=as.Date('2011-08-10')),
                throws_error('outBegin must be a Date vector'))

     expect_that(TraceDateInterval(movements=data.frame(source=1L, destination=2L, t=as.Date('2011-08-10')),
                                  root=1L,
                                  inBegin=as.Date('2011-08-10'),
                                  inEnd=as.Date('2011-08-10'),
                                  outBegin=as.Date('2011-08-10'),
                                  outEnd=2011),
                throws_error('outEnd must be a Date vector'))

     expect_that(TraceDateInterval(movements=data.frame(source=1L, destination=2L, t=as.Date('2011-08-10')),
                                  root=1L,
                                  inBegin=as.Date('2011-08-10'),
                                  inEnd=as.Date('2011-07-10'),
                                  outBegin=as.Date('2011-08-10'),
                                  outEnd=as.Date('2011-08-10')),
                throws_error('inEnd < inBegin'))

     expect_that(TraceDateInterval(movements=data.frame(source=1L, destination=2L, t=as.Date('2011-08-10')),
                                  root=1L,
                                  inBegin=as.Date('2011-08-10'),
                                  inEnd=as.Date('2011-08-10'),
                                  outBegin=as.Date('2011-08-10'),
                                  outEnd=as.Date('2011-07-10')),
                throws_error('outEnd < outBegin'))
})

test_that('Direction', {
  load(file="data/movements1.rda")

  ct <- TraceDateInterval(movements1,
                          root = 4L,
                          inBegin = as.Date('2010-08-02'),
                          inEnd = as.Date('2010-09-01'),
                          outBegin =  as.Date('2010-08-01'),
                          outEnd = as.Date('2010-08-31'))

  expect_that(ct@ingoingContacts@direction, is_identical_to('in'))
  expect_that(ct@outgoingContacts@direction, is_identical_to('out'))

  load(file="data/movements2.rda")

  ct <- TraceDateInterval(movements2,
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

  ct <- TraceDateInterval(movements1,
                          root = 8L,
                          inBegin = as.Date('2010-08-22'),
                          inEnd = as.Date('2010-10-01'),
                          outBegin = as.Date('2010-08-01'),
                          outEnd = as.Date('2010-08-31'))

  expect_that(IngoingContactChain(ct), is_identical_to(7L))
  expect_that(OutgoingContactChain(ct), is_identical_to(0L))

  load(file="data/movements2.rda")

  ct <- TraceDateInterval(movements2,
                          root = 4L,
                          inBegin = as.Date('2010-07-22'),
                          inEnd = as.Date('2010-08-21'),
                          outBegin = as.Date('2010-08-01'),
                          outEnd = as.Date('2010-08-31'))

  expect_that(IngoingContactChain(ct), is_identical_to(3L))
  expect_that(OutgoingContactChain(ct), is_identical_to(0L))

  load(file="data/movements5.rda")

  ct <- TraceDateInterval(movements5,
                          root = 1L,
                          inBegin = as.Date('2010-08-02'),
                          inEnd = as.Date('2010-09-01'),
                          outBegin = as.Date('2010-09-01'),
                          outEnd = as.Date('2010-10-01'))

  expect_that(IngoingContactChain(ct), is_identical_to(1L))
  expect_that(OutgoingContactChain(ct), is_identical_to(0L))
})

test_that('OutgoingContactChain', {
  load(file="data/movements4.rda")

  ct <- TraceDateInterval(movements4,
                          root = 1L,
                          inBegin = as.Date('2010-08-02'),
                          inEnd = as.Date('2010-09-01'),
                          outBegin = as.Date('2010-08-01'),
                          outEnd = as.Date('2010-11-09'))

  expect_that(IngoingContactChain(ct), is_identical_to(0L))
  expect_that(OutgoingContactChain(ct), is_identical_to(7L))

  load(file="data/movements5.rda")

  ct <- TraceDateInterval(movements5,
                          root = 1L,
                          inBegin = as.Date('2010-07-02'),
                          inEnd = as.Date('2010-08-01'),
                          outBegin = as.Date('2010-08-01'),
                          outEnd = as.Date('2010-08-31'))

  expect_that(IngoingContactChain(ct), is_identical_to(0L))
  expect_that(OutgoingContactChain(ct), is_identical_to(1L))
})

test_that('InDegree', {
  load(file="data/movements2.rda")

  ct <- TraceDateInterval(movements2,
                          root = 4L,
                          inBegin = as.Date('2010-08-02'),
                          inEnd = as.Date('2010-09-01'),
                          outBegin = as.Date('2010-08-01'),
                          outEnd = as.Date('2010-08-31'))

  expect_that(InDegree(ct), is_identical_to(1L))
  expect_that(OutDegree(ct), is_identical_to(0L))

  ct <- TraceDateInterval(movements2,
                          root = 4L,
                          inBegin = as.Date('2010-08-27'),
                          inEnd = as.Date('2010-09-01'),
                          outBegin = as.Date('2010-08-01'),
                          outEnd = as.Date('2010-08-31'))

  expect_that(InDegree(ct), is_identical_to(0L))
  expect_that(OutDegree(ct), is_identical_to(0L))
})

test_that('OutDegree', {
  load(file="data/movements3.rda")

  ct <- TraceDateInterval(movements3,
                          root = 1L,
                          inBegin = as.Date('2010-08-02'),
                          inEnd = as.Date('2010-09-01'),
                          outBegin = as.Date('2010-08-01'),
                          outEnd = as.Date('2010-08-31'))

  expect_that(InDegree(ct), is_identical_to(0L))
  expect_that(OutDegree(ct), is_identical_to(3L))

  ct <- TraceDateInterval(movements3,
                          root = 1L,
                          inBegin = as.Date('2010-08-02'),
                          inEnd = as.Date('2010-09-01'),
                          outBegin = as.Date('2010-08-01'),
                          outEnd = as.Date('2010-08-16'))

  expect_that(InDegree(ct), is_identical_to(0L))
  expect_that(OutDegree(ct), is_identical_to(2L))
})

test_that('Root not in movements', {
  load(file="data/movements3.rda")

  ct <- TraceDateInterval(movements3,
                          root = 15L,
                          inBegin = as.Date('2010-08-02'),
                          inEnd = as.Date('2010-09-01'),
                          outBegin = as.Date('2010-08-01'),
                          outEnd = as.Date('2010-08-31'))

  expect_that(InDegree(ct), is_identical_to(0L))
  expect_that(OutDegree(ct), is_identical_to(0L))
})

test_that('Loops', {
    movements <- data.frame(source = c(2L, 2L),
                            destination = c(1L, 2L),
                            t = as.Date(c('2010-10-03', '2010-10-02')))

    ct <- TraceDateInterval(movements,
                            root = 1L,
                            inBegin = as.Date('2010-09-30'),
                            inEnd = as.Date('2010-10-05'),
                            outBegin = as.Date('2010-10-05'),
                            outEnd = as.Date('2010-10-10'))

    expect_that(ct@ingoingContacts@source, is_identical_to('2'))
    expect_that(ct@ingoingContacts@destination, is_identical_to('1'))

    ct <- TraceDateInterval(movements,
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
    ct.2 <- Trace(as(ct.1, 'data.frame'), 2645, '2005-10-31', 90)

    expect_that(ct.2, is_identical_to(ct.1))
})

