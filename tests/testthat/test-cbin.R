test_that("cbin works", {

    x   <- c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE)
    ans <- c(  1L,   1L,    1L,    2L,   3L,   3L,   3L)
    expect_equal(cbin(x), ans)

    ## value of last entry does not matter:
    x[length(x)] <- !x[length(x)]
    expect_equal(cbin(x), ans)
    x[length(x)] <- NA
    expect_equal(cbin(x), ans)

    ## No missing (exept last entry)
    expect_error(cbin(NA,NA))
    expect_equal(cbin(NA), 1L)

})
