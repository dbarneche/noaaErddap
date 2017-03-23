context('Cache file link')

path      <-  file.path('test', 'productivity')
firstDay  <-  as.Date('1997-01-03')

test_that('Simple corner cases', {

    # returns correct structure
    expect_is(erddapLocal(path, firstDay), 'character')
    expect_identical(erddapLocal(path, firstDay), 'test/productivity/January-1997.nc')

    # path and firstDay are put in wrong order
    expect_error(erddapLocal(firstDay, path), 'Input of wrong class')

    # NA or NULL as input
    expect_error(erddapLocal(NA, firstDay), 'Input of wrong class')
    expect_error(erddapLocal(path, NA), 'Input of wrong class')
    expect_error(erddapLocal(NA, NA), 'Input of wrong class')
    expect_error(erddapLocal(NULL, firstDay), 'Input of wrong class')
    expect_error(erddapLocal(path, NULL), 'Input of wrong class')
    expect_error(erddapLocal(NULL, NULL), 'Input of wrong class')

    # inputs are numbers
    expect_error(erddapLocal(1, firstDay), 'Input of wrong class')
    expect_error(erddapLocal(path, 1), 'Input of wrong class')

})
