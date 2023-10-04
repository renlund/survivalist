test_that("cumsum_bounded works", {

    expect_equal(cumsum_bounded(c(-100, 50, 60, -200),
                                low = 0, high = 100),
                 c(0,50,100,0))

    expect_equal(
        cumsum_bounded(c(25, -20, 55),
                       low = c(0, -5, 0),
                       high = c(10, 10, 100)),
        c(10, -5, 50)
    )

    for(i in 1:100){
        tmp <- rnorm(100)
        expect_equal(cumsum_bounded(tmp, low= -Inf, high=Inf),
                     cumsum(tmp))
    }


})
