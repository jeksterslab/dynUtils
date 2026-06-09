## ---- test-dynUtils-dyn-utils-lapply-dot
lapply(
  X = 1,
  FUN = function(i,
                 text) {
    message(text)

    testthat::test_that(
      paste(
        text,
        "dynUtils:::.DynUtilsLapply matches lapply when ncores is NULL"
      ),
      {
        out <- dynUtils:::.DynUtilsLapply(
          X = 1:4,
          FUN = function(x, add) {
            x + add
          },
          ncores = NULL,
          add = 10
        )

        expected <- lapply(
          X = 1:4,
          FUN = function(x, add) {
            x + add
          },
          add = 10
        )

        testthat::expect_equal(out, expected)
      }
    )

    testthat::test_that(
      paste(
        text,
        "dynUtils:::.DynUtilsLapply stays serial when ncores = 1"
      ),
      {
        out <- dynUtils:::.DynUtilsLapply(
          X = 1:3,
          FUN = sqrt,
          ncores = 1L
        )

        testthat::expect_equal(out, lapply(1:3, sqrt))
      }
    )

    testthat::test_that(
      paste(
        text,
        "dynUtils:::.DynUtilsLapply can use forked parallelism when available"
      ),
      {
        testthat::skip_on_cran()
        testthat::skip_if(
          .Platform$OS.type == "windows",
          message = "Avoid socket-cluster tests in this lightweight test file."
        )

        out <- dynUtils:::.DynUtilsLapply(
          X = 1:4,
          FUN = sqrt,
          ncores = 2L
        )

        testthat::expect_equal(out, lapply(1:4, sqrt))
      }
    )
  },
  text = "test-dynUtils-dyn-utils-lapply-dot"
)
