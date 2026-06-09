#' Scale by ID
#'
#' The function scales the data by ID.
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inheritParams SubsetByID
#' @param scale Logical.
#'   If `scale = TRUE`, standardize by `id`.
#'   If `scale = FALSE`, mean center by `id`.
#' @param obs_skip Character vector.
#'   A vector of character strings
#'   of the names of the observed variables to skip centering/scaling.
#' @param cov_skip Character vector.
#'   A vector of character strings
#'   of the names of the covariates to skip centering/scaling.
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
#' ScaleByID(
#'   data = data,
#'   id = "id",
#'   time = "time",
#'   observed = paste0("y", 1:p)
#' )
#'
#' @family Dynamic Modeling Utility Functions
#' @keywords dynUtils data
#' @export
ScaleByID <- function(data,
                      id,
                      time,
                      observed,
                      covariates = NULL,
                      scale = TRUE,
                      obs_skip = NULL,
                      cov_skip = NULL,
                      ncores = NULL) {
  object <- SubsetByID(
    data = data,
    id = id,
    time = time,
    observed = observed,
    covariates = covariates,
    ncores = ncores
  )
  if (is.null(covariates)) {
    cov_skip <- NULL
  }
  foo <- function(i,
                  id,
                  time,
                  observed,
                  covariates,
                  scale,
                  obs_skip,
                  cov_skip) {
    if (is.null(obs_skip)) {
      obs <- observed
    } else {
      obs <- observed[!observed %in% obs_skip]
      obs <- obs[obs %in% colnames(i)]
    }
    if (is.null(covariates)) {
      covs <- NULL
    } else {
      if (is.null(cov_skip)) {
        covs <- covariates
      } else {
        covs <- covariates[!covariates %in% cov_skip]
        covs <- covs[covs %in% colnames(i)]
      }
    }
    varnames <- obs
    if (!is.null(covs)) {
      varnames <- c(
        varnames,
        covs
      )
    }
    if (scale) {
      # prevent NaN
      j <- as.data.frame(
        lapply(
          X = i[, varnames, drop = FALSE],
          FUN = function(x) {
            (
              x - mean(x, na.rm = TRUE)
            ) / stats::sd(
              x,
              na.rm = TRUE
            )^as.logical(
              stats::sd(
                x,
                na.rm = TRUE
              )
            )
          }
        )
      )
    } else {
      j <- as.data.frame(
        lapply(
          X = i[, varnames, drop = FALSE],
          FUN = function(x) {
            x - mean(x, na.rm = TRUE)
          }
        )
      )
    }
    for (k in varnames) {
      i[, k] <- j[, k]
    }
    return(i)
  }
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
      fun = foo,
      id = id,
      time = time,
      observed = observed,
      covariates = covariates,
      scale = scale,
      obs_skip = obs_skip,
      cov_skip = cov_skip
    )
  } else {
    output <- lapply(
      X = object,
      FUN = foo,
      id = id,
      time = time,
      observed = observed,
      covariates = covariates,
      scale = scale,
      obs_skip = obs_skip,
      cov_skip = cov_skip
    )
  }
  output <- do.call(
    what = "rbind",
    args = output
  )
  rownames(output) <- NULL
  return(output)
}
