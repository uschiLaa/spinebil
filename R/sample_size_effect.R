#' Simulate Effect of Sample Size on Projection Pursuit Index Under Gaussian Noise
#'
#' For a given index function, simulates how the index behaves across a range
#' of sample sizes when applied to pairs of standard normal noise.
#'
#' @param index_fun A function taking two numeric vectors (`x`, `y`) and returning a scalar index.
#' @param n_sim Integer. Number of simulations per sample size. Default is 100.
#'
#' @return A tibble with:
#' - `sample_size`: sample size used for each simulation block
#' - `percentile95`: 95th percentile of the index values over simulations
#'
#' @examples
#' ppi_samplesize_effect(scag_index("stringy"), n_sim = 10)
#'
#' @export
ppi_samplesize_effect <- function(index_fun,
                                  n_sim = 100) {
  
  # Define sample sizes
  low_sample_sizes <- seq(30, 200, by = 5)
  middle_sample_sizes <- seq(200, 500, by = 10)
  big_sample_sizes <- seq(500, 2000, by = 25)
  sample_sizes <- unique(c(low_sample_sizes, middle_sample_sizes, big_sample_sizes))
  
  # Set up parallel processing
  future::plan(future::multisession, workers = max(1, parallel::detectCores() - 1))
  
  results <- furrr::future_map_dfr(sample_sizes, function(sample_size) {
    
    index_values <- numeric(n_sim)
    
    for (i in seq_len(n_sim)) {
      x <- stats::rnorm(sample_size)
      y <- stats::rnorm(sample_size)
      mat <- cbind(x,y)
      
      index_values[i] <- tryCatch(
        index_fun(mat),
        error = function(e) {
          warning(sprintf("Simulation %d failed at sample size %d: %s", i, sample_size, e$message))
          NA_real_
        }
      )
    }
    
    tibble::tibble(
      sample_size = sample_size,
      percentile95 = stats::quantile(index_values, 0.95, na.rm = TRUE)
    )
    
  }, .options = furrr::furrr_options(seed = TRUE), .progress = TRUE)
  
  future::plan(future::sequential)
  return(results)
}
