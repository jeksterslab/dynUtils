## ---- test-dynUtils-dyn-utils-use-parallel-dot
lapply(
  X = 1,
  FUN = function(i,
                 text) {
    message(text)

    testthat::test_that(
      paste(
        text,
        "dynUtils:::.DynUtilsUseParallel returns TRUE only when parallelism is useful"
      ),
      {
        testthat::expect_false(
          dynUtils:::.DynUtilsUseParallel(ncores = NULL, n_tasks = 2L)
        )
        testthat::expect_false(
          dynUtils:::.DynUtilsUseParallel(ncores = 1L, n_tasks = 2L)
        )
        testthat::expect_false(
          dynUtils:::.DynUtilsUseParallel(ncores = 2L, n_tasks = 1L)
        )
        testthat::expect_false(
          dynUtils:::.DynUtilsUseParallel(ncores = NA_integer_, n_tasks = 2L)
        )
        testthat::expect_true(
          dynUtils:::.DynUtilsUseParallel(ncores = 2L, n_tasks = 2L)
        )
        testthat::expect_true(
          dynUtils:::.DynUtilsUseParallel(ncores = c(2L, 1L), n_tasks = 3L)
        )
      }
    )
  },
  text = "test-dynUtils-dyn-utils-use-parallel-dot"
)
