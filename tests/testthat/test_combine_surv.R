test_that("combine_surv works", {

    ## library(survivalist)
    ## library(testthat)

    d <- data.frame(
        the_id = 1:5,
        X_t = c(10,10,10,10,10),
        X_e = c( 0, 1, 0, 1, 0),
        Y_t = c(10,10, 9, 8, 7),
        Y_e = c( 0, 1, 0, 0, 1)
    )
    sl <- data.frame(label = c("S1", "S2"),
                     time = c("X_t", "Y_t"),
                     event = c("X_e", "Y_e"))
    cs <- combine_surv(surv = sl,
                       data = d,
                       id = "the_id",
                       nm = c("A", "B"))
    r <- data.frame(
        the_id = 1:5,
        A = c(10,10,9,8,7),
        B = as.integer(c(0,1,0,0,1))
    )
    expect_equal(cs, r)

    d <- data.frame(
        the_id = 1:5,
        X_t = c(10,10,10,10,10),
        X_e = c( 0, 1, 0, 1, 0),
        Y_t = c(10,10, 9, 8, 7),
        Y_e = c( 0, 1, 0, 0, 1),
        Z_t = c(10,10, 9, 9, 8),
        Z_e = c( 0, 1, 1, 0, 1)
    )
    sl <- data.frame(label = c("S1", "S2", "S3"),
                     time = c("X_t", "Y_t", "Z_t"),
                     event = c("X_e", "Y_e", "Z_e"))
    cs <- combine_surv(surv = sl, data = d, id = "the_id",
                       nm = c("A", "B"))
    r <- data.frame(
        the_id = 1:5,
        A = c(10,10,9,8,7),
        B = as.integer(c(0,1,1,0,1))
    )
    expect_equal(cs, r)

})
