test_that("fixed_treatment works", {

    d <- data.table::data.table(id = 1, t = 0, state = "a", run = 1)
    ans <- data.table::data.table(id = 1, t = c(0,1), state = c("a", ""))
    expect_equal(fixed_switch(d), ans)

    d <- data.table::data.table(id = 1, t = 0, state = "a", run = 10)
    ans <- data.table::data.table(id = 1, t = c(0,10), state = c("a", ""))
    expect_equal(fixed_switch(d), ans)

    d <- data.table::data.table(id = 1, t = c(0,5,10), state = "a", run = 10)
    ans <- data.table::data.table(id = 1, t = c(0,20), state = c("a", ""))
    expect_equal(fixed_switch(d), ans)

    ans <- data.table::data.table(id = 1, t = c(0,5,10,20),
                                  state = rep(c("a", ""), c(3,1)))
    expect_equal(fixed_switch(d, simplify = FALSE), ans)

    d <- data.table::data.table(id = 1, t = c(0,5), state = c("a", "b"), run = 10)
    ans <- data.table::data.table(id = 1, t = c(0,5,15), state = c("a", "b", ""))
    expect_equal(fixed_switch(d), ans)

    d <- data.table::data.table(id = 1, t = c(0,5,10,100),
                                state = c("a", "b")[c(1,2,2,2)], run = 10)
    ans <- data.table::data.table(id = 1, t = c(0,5,20,100,110),
                                  state = c("a", "b", "")[c(1,2,3,2,3)])
    expect_equal(fixed_switch(d), ans)

    ## make sure duplicated t's are ignored:
    expect_equal(fixed_switch(d[c(1,1,2,2,3,4)]), ans)

    ## make sure that reordering within fixed_treatment works:
    for(i in 1:10) expect_equal(fixed_treatment(d[sample(1:nrow(d))]), ans)

})

test_that("fixed_treatment handles names correctly", {

    d <- data.frame(id = 1, t = 0, state = "a", run = 10)
    ans <- data.frame(id = 1, t = c(0,10), state = c("a", ""))
    expect_equal(fixed_treatment(d), ans)

    d2 <- d; ans2 <- ans
    names(d2) <- c("lopnr", "tid", "trt", "dur")
    names(ans2) <- c("lopnr", "tid", "trt")
    expect_equal(
        fixed_treatment(d2, id = "lopnr", t = "tid",
                        state = "trt", run = "dur"),
        ans2
    )

    d2 <- d; ans2 <- ans
    names(d2) <- c("run", "state", "foo", "id")
    names(ans2) <- c("run", "state", "foo")
    expect_equal(
        fixed_treatment(d2, id = "run", t = "state",
                        state = "foo", run = "id"),
        ans2
    )


})
