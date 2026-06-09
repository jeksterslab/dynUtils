#' Delete for NAs in Initial Row By ID
#'
#' The function removes the initial row by ID if it contains missing values.
#' This process is repeated recursively
#' until the first row per ID no longer has missing observations.
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
#' # Replace first row with NA
#' data[1, paste0("y", 1:p)] <- NA
#' DeleteInitialNA(
#'   data = data,
#'   id = "id",
#'   time = "time",
#'   observed = paste0("y", 1:p),
#' )
#'
#' @family Dynamic Modeling Utility Functions
#' @keywords dynUtils data
#' @export
DeleteInitialNA <- function(data,
                            id,
                            time,
                            observed,
                            covariates = NULL,
                            ncores = NULL) {
  object <- SubsetByID(
    data = data,
    id = id,
    time = time,
    observed = observed,
    covariates = covariates,
    ncores = ncores
  )
  par <- FALSE
  if (!is.null(ncores)) {
    ncores <- as.integer(ncores)
    if (ncores > 1) {
      par <- TRUE
    }
  }
  if (par) {
    cl <- parallel::makeCluster(ncores)
    on.exit(
      parallel::stopCluster(cl = cl)
    )
    output <- parallel::parLapply(
      cl = cl,
      X = object,
      fun = function(i) {
        missing <- any(
          is.na(
            i[1, ]
          )
        )
        while (missing) {
          i <- i[-1, , drop = FALSE]
          if (dim(i)[1] == 0) {
            return(NULL)
          }
          missing <- any(
            is.na(
              i[1, ]
            )
          )
        }
        return(i)
      }
    )
  } else {
    output <- lapply(
      X = object,
      FUN = function(i) {
        missing <- any(
          is.na(
            i[1, ]
          )
        )
        while (missing) {
          i <- i[-1, , drop = FALSE]
          if (dim(i)[1] == 0) {
            return(NULL)
          }
          missing <- any(
            is.na(
              i[1, ]
            )
          )
        }
        return(i)
      }
    )
  }
  output <- do.call(
    what = "rbind",
    args = output
  )
  rownames(output) <- NULL
  return(output)
}
