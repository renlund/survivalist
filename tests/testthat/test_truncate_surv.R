test_that("truncate_surv works", {

    d <- data.frame(
        the_id = 1:7,
        X_t = c( 5, 5, 7, 7, 8, 8, 8.1),
        X_e = c( 0, 1, 0, 1, 0, 1, 1),
        Y_t = c(10,10, 9, 8, 8, 7, 7),
        Y_e = c( 0, 1, 0, 0, 1, 0, 1)
    )
    sl <- list(c("X_t", "X_e"), c("Y_t", "Y_e"))
    ts <- truncate_surv(surv = sl,
                        data = d,
                        trunc = 8,
                        id = "the_id")
    r <- data.frame(
        the_id = 1:7,
        X_t = c( 5, 5, 7, 7, 8, 8, 8),
        X_e = c( 0, 1, 0, 1, 0, 1, 0),
        Y_t = c( 8, 8, 8, 8, 8, 7, 7),
        Y_e = c( 0, 0, 0, 0, 1, 0, 1)
        )
    expect_equal(ts, r)


})
