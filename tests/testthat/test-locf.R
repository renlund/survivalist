test_that("locf works", {

    expect_equal(
        locf(c(1L, NA_integer_, 2L, NA_integer_, NA_integer_)),
        c(1L, 1L, 2L, 2L, 2L)
    )

    expect_equal(
        locf(c("Foo", NA_character_, "Baz", NA_character_, NA_character_)),
        c("Foo", "Foo", "Baz", "Baz", "Baz")
    )

    expect_equal(
        locf(c(NA_integer_, 2L, NA_integer_, NA_integer_)),
        c(NA_integer_, 2L, 2L, 2L)
    )

})
