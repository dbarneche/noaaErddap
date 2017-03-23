context('Checking input date limits')

type1      <-  'productivity'
type2      <-  'chlorophyll'
type3      <-  'sst'
typo       <-  'temperature'
firstDay1  <-  as.Date('1997-10-03')
firstDay2  <-  as.Date('1960-01-03')
firstDay3  <-  as.Date('2020-01-03')

test_that('Simple corner cases', {
    
    # returns correct structure
    expect_is(checkDateLimits(type1, firstDay1), 'Date')
    expect_is(checkDateLimits(type2, firstDay1), 'Date')
    expect_is(checkDateLimits(type3, firstDay1), 'Date')

    # day falls outside allowed dates    
    expect_error(checkDateLimits(type1, firstDay2))
    expect_error(checkDateLimits(type2, firstDay2))
    expect_error(checkDateLimits(type3, firstDay2))
    expect_error(checkDateLimits(type1, firstDay3))
    expect_error(checkDateLimits(type2, firstDay3))
    expect_error(checkDateLimits(type3, firstDay3))
    expect_error(checkDateLimits(type1, as.Date('01-01-01')))

    # type and firstDay are input in wrong order
    expect_error(checkDateLimits(firstDay1, type1), 'character string is not in a standard unambiguous format')

    # NA or NULL as input
    expect_error(checkDateLimits(type1, NA), 'missing value where TRUE/FALSE needed')
    expect_error(checkDateLimits(NA, firstDay1), 'missing value where TRUE/FALSE needed')
    expect_error(checkDateLimits(NA, NA), 'missing value where TRUE/FALSE needed')
    expect_error(checkDateLimits(type1, NULL), 'subscript out of bounds')
    expect_error(checkDateLimits(NULL, firstDay1), 'subscript out of bounds')
    expect_error(checkDateLimits(NULL, NULL), 'subscript out of bounds')

    # firstDay is not Date
    expect_error(checkDateLimits(type1, 2010))
    expect_error(checkDateLimits(type1, '2010'))
    expect_error(checkDateLimits(type1, data.frame('2010')))

    # type does not match allowed options
    expect_error(checkDateLimits(typo, firstDay1), 'subscript out of bounds')

    # either month or year have multiple elements
    expect_identical(checkDateLimits(type1, firstDay1), firstDay1)
    expect_identical(checkDateLimits(type2, firstDay1), firstDay1)
    expect_identical(checkDateLimits(type3, firstDay1), firstDay1)

})
