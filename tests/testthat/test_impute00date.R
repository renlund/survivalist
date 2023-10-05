test_that("impute00date and related functions work", {

    expect_equal(survivalist:::prefixNchar1With0(7), "07")
    expect_equal(survivalist:::prefixNchar1With0(17), 17)

    expect_equal(survivalist:::create_date(2021,6,2), as.Date("2021-06-02"))
    expect_equal(survivalist:::create_date(2024,2,29), as.Date("2024-02-29"))
    expect_error(survivalist:::create_date(2025,2,29))

    expect_equal(survivalist:::days_in_month(2023, 1), 31)
    expect_equal(survivalist:::days_in_month(2023, 2), 28)
    expect_equal(survivalist:::days_in_month(2020, 2), 29)
    expect_error(survivalist:::days_in_month(2020, 13))

    expect_equal(survivalist:::days_in_year(2023), 365)
    expect_equal(survivalist:::days_in_year(2020), 366)
    expect_equal(survivalist:::days_in_year(2100), 365)
    expect_equal(survivalist:::days_in_year(2000), 366)

    expect_equal(survivalist:::what_day_in_year(1979,4,24), 31+28+31 + 24)
    expect_equal(survivalist:::what_day_in_year(1980,7,13), 31+29+31+30+31+30 + 13)

    expect_equal(survivalist:::what_date_in_year(1979, 31+28+31 + 24), as.Date("1979-04-24"))
    expect_equal(survivalist:::what_date_in_year(1980, 31+29+31+30+31+30 + 13), as.Date("1980-07-13"))
    expect_error(survivalist:::what_date_in_year(1979, -1))

    expect_equal(survivalist:::date_in_what_remains_of_the_month(2020, 1, 1),
                 as.Date("2020-01-16"))
    expect_equal(survivalist:::date_in_what_remains_of_the_month(2020, 2, 12),
                 as.Date("2020-02-20"))

    expect_equal(survivalist:::date_in_what_remains_of_the_year(2020, 1, 1),
                 as.Date("2020-07-01"))
    expect_equal(survivalist:::date_in_what_remains_of_the_year(2020, 7, 1),
                 as.Date("2020-09-30"))
    expect_equal(survivalist:::date_in_what_remains_of_the_year(2020, 12, 29),
                 as.Date("2020-12-30"))

    expect_equal(impute_a_00_date("20000100"), as.Date("2000-01-16"))
    expect_equal(impute_a_00_date("20000100", as.Date("1999-12-31")),
                 as.Date("2000-01-16"))
    expect_equal(impute_a_00_date("20000100", as.Date("2000-01-10")),
                 as.Date("2000-01-20"))
    expect_error(impute_a_00_date("20000100", as.Date("2000-02-01")))

    expect_equal(impute_a_00_date("20000000"), as.Date("2000-07-01"))
    expect_equal(impute_a_00_date("20000000", as.Date("1999-12-31")),
                 as.Date("2000-07-01"))
    expect_equal(impute_a_00_date("20000000", as.Date("2000-07-01")),
                 as.Date("2000-09-30"))
    expect_error(impute_a_00_date("20000000", as.Date("2001-01-01")))
    expect_error(impute_a_00_date("20000101"))

    expect_equal(impute00date(c("20200513", "20200500", "20200000")),
                 as.Date(c("2020-05-13", "2020-05-16", "2020-07-01")))
    expect_equal(impute00date(c("20200513", "20200500", "20200000"),
                              as.Date(c("2020-05-13", "2020-05-16", "2020-07-01"))),
                 as.Date(c("2020-05-13", "2020-05-23", "2020-09-30")))

    expect_warning(r <- impute00date("20200132"))
    expect_setequal(r, as.Date(NA_character_))
    expect_equal(attr(r, "bad_input_format"), 1)

    expect_warning(r <- impute00date("20200100", as.Date("2020-02-15")))
    expect_setequal(r, as.Date("2020-01-16"))
    expect_equal(attr(r, "input_below_bound"), 1)

})
