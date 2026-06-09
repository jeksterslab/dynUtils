#' Insert NAs for Missing Observations
#'
#' The function creates a sequence of time values.
#' It starts with the smallest time value as the starting point
#' and the largest time value as the endpoint.
#' The sequence is incremented by `delta_t`.
#' This new sequence is combined with the existing empirical time values.
#' For any specific time value where there are no observations,
#' NAs are inserted.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inheritParams SubsetByID
#'
#' @return Returns a data frame.
#'
#' @examples
#' # prepare parameters
#' set.seed(42)
#' ## number of individuals
#' n <- 5
#' ## time points
#' time <- 5
#' ## dynamic structure
#' p <- 3
#' mu0 <- rep(x = 0, times = p)
#' sigma0 <- 0.001 * diag(p)
#' sigma0_l <- t(chol(sigma0))
#' alpha <- rep(x = 0, times = p)
#' beta <- 0.50 * diag(p)
#' psi <- 0.001 * diag(p)
#' psi_l <- t(chol(psi))
#'
#' library(simStateSpace)
#' ssm <- SimSSMVARFixed(
#'   n = n,
#'   time = time,
#'   mu0 = mu0,
#'   sigma0_l = sigma0_l,
#'   alpha = alpha,
#'   beta = beta,
#'   psi_l = psi_l,
#'   type = 0
#' )
#' data <- as.data.frame(ssm)
#' InsertNA(
#'   data = data,
#'   id = "id",
#'   time = "time",
#'   observed = paste0("y", 1:p),
#'   delta_t = 0.10
#' )
#'
#' @family Dynamic Modeling Utility Functions
#' @keywords dynUtils data
#' @export
InitialNA <- function(data,
                      id,
                      time,
                      observed,
                      covariates = NULL,
                      ncores = NULL) {
  data <- .DynUtilsSelectSort(
    data = data,
    id = id,
    time = time,
    observed = observed,
    covariates = covariates
  )

  if (nrow(data) == 0L) {
    data[[id]][0]
  } else {
    run <- rle(data[[id]])
    start <- cumsum(run$lengths) - run$lengths + 1L

    first_rows <- data[
      start, ,
      drop = FALSE
    ]

    first_rows[
      !stats::complete.cases(first_rows),
      id
    ]
  }
}
