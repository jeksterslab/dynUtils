#' Check for NAs in Initial Row By ID
#'
#' The function checks if there are missing values
#' for the initial row by ID.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inheritParams SubsetByID
#'
#' @return Returns a vector of ID numbers
#'   where the initial row has any missing value.
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
#' InitialNA(
#'   data = data,
#'   id = "id",
#'   time = "time",
#'   observed = paste0("y", 1:p),
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
        if (missing) {
          return(
            i[1, attributes(object)$args$id]
          )
        } else {
          return(NA)
        }
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
        if (missing) {
          return(
            i[1, attributes(object)$args$id]
          )
        } else {
          return(NA)
        }
      }
    )
  }
  output <- do.call(
    what = "rbind",
    args = output
  )
  dim(output)
  output <- output[stats::complete.cases(output)]
  return(output)
}
