## ---- test-dyn-utils-delete-initial-na-keep-index
lapply(
  X = 1,
  FUN = function(i,
                 text) {
    message(text)

    testthat::test_that(
      paste(
        text,
        "dynUtils:::.DeleteInitialNAKeepIndex keeps from first complete row onward"
      ),
      {
        start <- c(1L, 5L, 8L)
        end <- c(4L, 7L, 10L)
        ok <- c(
          FALSE, FALSE, TRUE, FALSE,
          FALSE, FALSE, FALSE,
          TRUE, TRUE, TRUE
        )

        testthat::expect_identical(
          dynUtils:::.DeleteInitialNAKeepIndex(
            j = 1L,
            start = start,
            end = end,
            ok = ok
          ),
          3:4
        )

        testthat::expect_identical(
          dynUtils:::.DeleteInitialNAKeepIndex(
            j = 2L,
            start = start,
            end = end,
            ok = ok
          ),
          integer(0)
        )

        testthat::expect_identical(
          dynUtils:::.DeleteInitialNAKeepIndex(
            j = 3L,
            start = start,
            end = end,
            ok = ok
          ),
          8:10
        )
      }
    )
  },
  text = "test-dyn-utils-delete-initial-na-keep-index"
)
