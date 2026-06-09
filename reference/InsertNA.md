# Check for NAs in Initial Row By ID

The function checks if there are missing values for the initial row by
ID.

## Usage

``` r
InsertNA(data, id, time, observed, covariates = NULL, delta_t, ncores = NULL)
```

## Arguments

- data:

  Data frame. A data frame object of data for potentially multiple
  subjects that contain a column of subject ID numbers (i.e., an ID
  variable), a column indicating subject-specific measurement occasions
  (i.e., a TIME variable), at least one column of observed values.

- id:

  Character string. A character string of the name of the ID variable in
  the data.

- time:

  Character string. A character string of the name of the TIME variable
  in the data.

- observed:

  Character vector. A vector of character strings of the names of the
  observed variables in the data.

- covariates:

  Character vector. A vector of character strings of the names of the
  covariates in the data.

- delta_t:

  Positive number. Time interval.

- ncores:

  Positive integer. Number of cores to use. If `ncores = NULL`, use a
  single core. Consider using multiple cores when number of individuals
  is large.

## Value

Returns a vector of ID numbers where the initial row has any missing
value.

## See also

Other Dynamic Modeling Utility Functions:
[`DeleteInitialNA()`](https://github.com/jeksterslab/dynUtils/reference/DeleteInitialNA.md),
[`InitialNA()`](https://github.com/jeksterslab/dynUtils/reference/InitialNA.md),
[`ScaleByID()`](https://github.com/jeksterslab/dynUtils/reference/ScaleByID.md),
[`SubsetByID()`](https://github.com/jeksterslab/dynUtils/reference/SubsetByID.md)

## Author

Ivan Jacob Agaloos Pesigan

## Examples

``` r
# prepare parameters
set.seed(42)
## number of individuals
n <- 5
## time points
time <- 5
## dynamic structure
p <- 3
mu0 <- rep(x = 0, times = p)
sigma0 <- 0.001 * diag(p)
sigma0_l <- t(chol(sigma0))
alpha <- rep(x = 0, times = p)
beta <- 0.50 * diag(p)
psi <- 0.001 * diag(p)
psi_l <- t(chol(psi))

library(simStateSpace)
ssm <- SimSSMVARFixed(
  n = n,
  time = time,
  mu0 = mu0,
  sigma0_l = sigma0_l,
  alpha = alpha,
  beta = beta,
  psi_l = psi_l,
  type = 0
)
data <- as.data.frame(ssm)
# Replace first row with NA
data[1, paste0("y", 1:p)] <- NA
InitialNA(
  data = data,
  id = "id",
  time = "time",
  observed = paste0("y", 1:p),
)
#> [1] 1
```
