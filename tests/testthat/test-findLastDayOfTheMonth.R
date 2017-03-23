context('Finding last day of the month')

myDate  <-  as.Date('1997-01-03')

test_that('Simple corner cases', {

    # returns correct structure
    expect_is(findLastDayOfTheMonth(myDate), 'character')
    expect_identical(findLastDayOfTheMonth(myDate), '1997-01-31')
    expect_identical(findLastDayOfTheMonth(as.Date('2004-02-15')), '2004-02-29')
    expect_identical(findLastDayOfTheMonth(as.Date('2002-02-15')), '2002-02-28')

    # NA or NULL as input
    expect_true(is.na(findLastDayOfTheMonth(NA)))
    expect_true(length(findLastDayOfTheMonth(NULL)) == 0)

    # input is number
    expect_error(findLastDayOfTheMonth(1), 'character string is not in a standard unambiguous format')

})
