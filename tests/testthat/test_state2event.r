test_that("'state2event' works", {

    x = factor(LETTERS[c(1,1)])
    expect_equal(state2event(x),
                 data.frame(A=c(0L, 0L)))

    x = factor(LETTERS[c(1,1,2)])
    expect_equal(state2event(x),
                 data.frame(A = c(0L,0L,0L),
                            B = c(0L,1L,0L)))

    x = factor(LETTERS[c(1,1,2)], levels = LETTERS[3:1])
    expect_equal(state2event(x),
                 data.frame(C = c(0L,0L,0L),
                            B = c(0L,1L,0L),
                            A = c(0L,0L,0L)))

    x = factor(LETTERS[c(1,1,2,1,1,1,2,2)])
    expect_equal(state2event(x),
                 data.frame(A = c(0L,0L,1L,0L,0L,0L,0L,0L),
                            B = c(0L,1L,0L,0L,0L,1L,0L,0L)))

    expect_error(state2event(factor(c())))

    expect_error(state2event(factor(NA)))

    x <- factor(c(NA, NA), levels = LETTERS[1:2])
    expect_error(state2event(x))

    x <- factor(c(NA, "A", "B", NA), levels = LETTERS[1:2])
    expect_error(state2event(x))

})
