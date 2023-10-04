test_that("event1trunc works", {

    d <- data.frame(id = rep(1:4, c(5, 4, 1, 1)),
                    tstart = c(0, 5, 6, 8, 9,
                               0, 6, 10, 11,
                               0,
                               0),
                    tstop =  c(5, 6, 8, 9, 12,
                               6, 10, 11, 13,
                               4,
                               7),
                    blobb =  c(0, 0, 1, 0, 1,
                               0, 0, 0, 1,
                               0,
                               1))
    ans <- d[c(1:3, 6:11), ]
    rownames(ans) <- NULL
    expect_equal(event1trunc(d, event = "blobb"), ans)


})
