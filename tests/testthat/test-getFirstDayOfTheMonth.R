context('Creating vectors of class Date')

yearNum   <-  1997
yearChar  <-  as.character(yearNum)
mnthNum   <-  1
mnthChar  <-  as.character(mnthNum)

test_that('Simple corner cases', {
    # returns correct structure
    expect_is(getFirstDayOfTheMonth(mnthNum, yearNum), 'Date')
    expect_is(getFirstDayOfTheMonth(mnthNum, yearChar), 'Date')
    expect_is(getFirstDayOfTheMonth(mnthChar, yearNum), 'Date')
    expect_is(getFirstDayOfTheMonth(mnthChar, yearChar), 'Date')

    # month and year are put in wrong order
    expect_error(getFirstDayOfTheMonth(yearChar, mnthChar), 'character string is not in a standard unambiguous format')

    # NA or NULL as input
    expect_error(getFirstDayOfTheMonth(mnthChar, NA), 'character string is not in a standard unambiguous format')
    expect_error(getFirstDayOfTheMonth(NA, yearChar), 'character string is not in a standard unambiguous format')
    expect_error(getFirstDayOfTheMonth(NA, NA), 'character string is not in a standard unambiguous format')
    expect_error(getFirstDayOfTheMonth(mnthChar, NULL), 'character string is not in a standard unambiguous format')
    expect_error(getFirstDayOfTheMonth(NULL, yearChar), 'character string is not in a standard unambiguous format')
    expect_error(getFirstDayOfTheMonth(NULL, NULL), 'character string is not in a standard unambiguous format')

    # months that go below or beyond 1:12
    expect_error(getFirstDayOfTheMonth(0, 2010), 'character string is not in a standard unambiguous format')
    expect_error(getFirstDayOfTheMonth(-1.5, 2010), 'character string is not in a standard unambiguous format')
    expect_error(getFirstDayOfTheMonth(13, 2010), 'character string is not in a standard unambiguous format')

    # months and/or year are decimals
    expect_error(getFirstDayOfTheMonth(10.5, 2010), 'character string is not in a standard unambiguous format')
    expect_error(getFirstDayOfTheMonth(10, 2010.5), 'character string is not in a standard unambiguous format')
    expect_error(getFirstDayOfTheMonth('10.5', '2010.5'), 'character string is not in a standard unambiguous format')

    # year with two digits only
    expect_is(getFirstDayOfTheMonth(mnthChar, mnthChar), 'Date')
    expect_identical(getFirstDayOfTheMonth(mnthChar, mnthChar), as.Date('0001-01-01'))

    # either month or year have multiple elements
    expect_is(getFirstDayOfTheMonth(rep(mnthChar, 2), yearChar), 'Date')
    expect_true(length(getFirstDayOfTheMonth(rep(mnthChar, 2), yearChar)) == 2L)
    expect_is(getFirstDayOfTheMonth(rep(mnthChar, 2), rep(yearChar, 3)), 'Date')
    expect_true(length(getFirstDayOfTheMonth(rep(mnthChar, 2), rep(yearChar, 3))) == 3L)
})
