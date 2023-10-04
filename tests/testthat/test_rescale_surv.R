test_that("rescale_surv works", {

    r <- data.frame(
        the_id = 1:5,
        X_t = c(10,10,10,10,10),
        X_e = c( 0, 1, 0, 1, 0),
        Y_t = c(10,10, 9, 8, 7),
        Y_e = c( 0, 1, 0, 0, 1)
    )
    d <- r
    d$X_t <- 10 * d$X_t
    d$Y_t <- 10 * d$Y_t
    sl <- list(c("X_t", "X_e"), c("Y_t", "Y_e"))
    rs <- rescale_surv(surv = sl,
                       data = d,
                       FUN = function(x) x/10,
                       id = "the_id")

    expect_equal(rs, r)


})
