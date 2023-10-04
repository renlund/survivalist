test_that("pill_switch works", {

    x <- data.table::data.table(
                         id = 1,
                         t =     c(  0, 10, 30, 35, 40),
                         state = c("A","B","B","C","C"),
                         pills = c(  4, 10, 10, 20, 20),
                         usage = c(  3,  1,  1,  2,  1),
                         capacity = Inf
                     )
    ans <- data.table::data.table(
                           id = 1,
                           t = c(0,2,10,20,30,35,70),
                           state = c("A", "", "B", "", "B", "C", "")
                       )
    expect_equal(pill_switch(x), ans)


    x <- data.table::data.table(
                         id = 1L, t = c(0,5), state = "A",
                         pills = c(5,5), usage = 1, capacity = Inf
        )
    ans = data.table::data.table(id = 1L, t = c(0,10), state = c("A", ""))
    expect_equal(pill_switch(x), ans)

    d <- data.table::data.table(id = 1, t = 0, state = "a", pills = 1,
                                usage = 1, capacity = 1)
    ans <- data.table::data.table(id = 1, t = c(0,1), state = c("a", ""))
    expect_equal(pill_switch(d), ans)

    d <- data.table::data.table(id = 1, t = 0, state = "a", pills = 20,
                                usage = 2, capacity = Inf)
    ans <- data.table::data.table(id = 1, t = c(0,10), state = c("a", ""))
    expect_equal(pill_switch(d), ans)

    d <- data.table::data.table(id = 1, t = c(0,5,10), state = "a",
                                pills = 10, usage = 1, capacity = Inf)
    ans <- data.table::data.table(id = 1, t = c(0,30), state = c("a", ""))
    expect_equal(pill_switch(d), ans)

    d <- data.table::data.table(id = 1, t = c(0,5,10), state = "a",
                                pills = c(20,25,0), usage = c(3,5,1),
                                capacity = Inf)
    ans <- data.table::data.table(id = 1, t = c(0,15), state = c("a", ""))
    expect_equal(pill_switch(d), ans)

    d <- data.table::data.table(id = 1, t = c(0,5), state = c("a", "b"),
                                pills = 10, usage = 1, capacity = Inf)
    ans <- data.table::data.table(id = 1, t = c(0,5,15), state = c("a", "b", ""))
    expect_equal(pill_switch(d), ans)

    d <- data.table::data.table(id = 1, t = c(0,5,10,100),
                                state = c("a", "b")[c(1,2,2,2)],
                                pills = 10, usage = 1, capacity = Inf)
    ans <- data.table::data.table(id = 1, t = c(0,5,25,100,110),
                                  state = c("a", "b", "")[c(1,2,3,2,3)])
    expect_equal(pill_switch(d), ans)

    ## make sure duplicated t's are ignored:
    expect_equal(pill_switch(d[c(1,1,2,2,3,4)]), ans)

    ## make sure that reordering within pill_treatment works:
    for(i in 1:10) expect_equal(pill_treatment(d[sample(1:nrow(d))]), ans)

    ## varying capacity
    d <- data.table::data.table(id = 1, t = c(0,10,20), state = "a",
                                pills = c(10, 100, 10),
                                usage = c(1, 2, 1),
                                capacity = c(1,40, 20))
    ans <- data.table::data.table(id = 1, t = c(0,2,10,41),
                                  state = c("a", "")[c(1,2,1,2)])
    expect_equal(pill_switch(d), ans)


})


test_that("pill_treatment handles names correctly", {

    d <- data.frame(id = 1, t = 0, state = "a", pills = 1,
                    usage = 1, capacity = 1000)
    ans <- data.frame(id = 1, t = c(0,1), state = c("a", ""))
    expect_equal(pill_treatment(d), ans)

    d2 <- d; ans2 <- ans
    names(d2) <- c("lopnr", "tid", "trt", "storlek", "anv", "grans")
    names(ans2) <- c("lopnr", "tid", "trt")
    expect_equal(
        pill_treatment(d2, id = "lopnr", t = "tid",
                       state = "trt", pills = "storlek",
                       usage = "anv", capacity = "grans"),
        ans2
    )

    d2 <- d; ans2 <- ans
    names(d2) <- c("usage", "id", "t", "state", "capacity", "pills")
    names(ans2) <- c("usage", "id", "t")
    expect_equal(
        pill_treatment(d2, id = "usage", t = "id",
                       state = "t", pills = "state",
                       usage = "capacity", capacity = "pills"),
        ans2
    )

    d <- data.frame(id = 1, t = 0, state = "a", pills = 1)
    ans <- data.frame(id = 1, t = c(0,1), state = c("a", ""))
    expect_warning(expect_warning(pill_treatment(d)))

})
