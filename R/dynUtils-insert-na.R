#' Insert NAs for Missing Observations
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param delta_t Positive number.
#'   Time interval.
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
InsertNA <- function(data,
                     id,
                     time,
                     observed,
                     covariates = NULL,
                     delta_t,
                     ncores = NULL) {
  stopifnot(delta_t > 0)
  object <- SubsetByID(
    data = data,
    id = id,
    time = time,
    observed = observed,
    covariates = covariates,
    ncores = ncores
  )
  # create new time vector with delta_t
  times <- attributes(object)$idx$time
  new_times <- sort(
    unique(
      c(
        seq(
          from = min(times),
          to = max(times),
          by = delta_t
        ),
        times
      )
    )
  )
  # create new time vector if min(diff(new_times)) is smaller than delta_t
  if (delta_t < min(diff(new_times))) {
    new_times <- sort(
      unique(
        c(
          seq(
            from = min(times),
            to = max(times),
            by = min(diff(new_times))
          ),
          new_times
        )
      )
    )
  }
  p <- ncol(object[[1]]) - 2
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
      fun = function(i,
                     new_times,
                     j,
                     p) {
        id <- i[1, attributes(object)$args$id]
        out <- lapply(
          X = new_times,
          FUN = function(t,
                         p,
                         id) {
            if (t %in% i[, "time"]) {
              return(
                i[which(i[, "time"] == t), ]
              )
            } else {
              return(
                c(
                  id,
                  t,
                  rep(x = NA, times = p)
                )
              )
            }
          },
          p = p,
          id = id
        )
        out <- do.call(
          what = "rbind",
          args = out
        )
        return(out)
      },
      new_times = new_times,
      p = p
    )
  } else {
    output <- lapply(
      X = object,
      FUN = function(i,
                     new_times,
                     j,
                     p) {
        id <- i[1, attributes(object)$args$id]
        out <- lapply(
          X = new_times,
          FUN = function(t,
                         p,
                         id) {
            if (t %in% i[, "time"]) {
              return(
                i[which(i[, "time"] == t), ]
              )
            } else {
              return(
                c(
                  id,
                  t,
                  rep(x = NA, times = p)
                )
              )
            }
          },
          p = p,
          id = id
        )
        out <- do.call(
          what = "rbind",
          args = out
        )
        return(out)
      },
      new_times = new_times,
      p = p
    )
  }
  output <- do.call(
    what = "rbind",
    args = output
  )
  rownames(output) <- NULL
  return(output)
}
