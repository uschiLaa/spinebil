#' Simulate Effect of Sample Size on Projection Pursuit Index Under Gaussian Noise
#'
#' For a given index function, simulates how the index behaves across a range
#' of sample sizes when applied to pairs of standard normal noise.
#' 
#' @param index_fun A function taking a 2-column numeric matrix (x, y) and returning a scalar index.
#' @param sample_sizes Numeric vector (e.g., 100, c(50,100,200), seq(30,200,5)),
#'   or a function with no arguments returning a numeric vector.
#'   If NULL, uses the original default grid.
#' @param n_sim Integer. Number of simulations per sample size.
#' @param csv_file Optional file path. If provided, saves the summary tibble to CSV.
#'
#' @return A tibble with:
#' - `sample_size`: sample size used for each simulation block
#' - `percentile95`: 95th percentile of the index values over simulations
#' 
#' @examples
#' \dontrun{
#' ppi_samplesize_effect(scag_index("stringy"), sample_sizes = c(100,300), n_sim = 3)
#'}
#' @export
ppi_samplesize_effect <- function(index_fun,
                                  sample_sizes = NULL,
                                  n_sim = 100,
                                  csv_file = NULL) {
  stopifnot(is.function(index_fun))
  
  # Default sample sizes
  default_sample_sizes <- function() {
    low_sample_sizes <- seq(30, 200, by = 5)
    middle_sample_sizes <- seq(200, 500, by = 10)
    big_sample_sizes <- seq(500, 2000, by = 25)
    unique(c(low_sample_sizes, middle_sample_sizes, big_sample_sizes))
  }
  
  # Resolve sample sizes
  if (is.null(sample_sizes)) {
    sample_sizes <- default_sample_sizes()
  } else if (is.function(sample_sizes)) {
    sample_sizes <- sample_sizes()
  }
  
  # Set up parallel processing
  with(future::plan(future::multisession), local = TRUE)
  
  results <- furrr::future_map_dfr(
    sample_sizes,
    function(sample_size) {
      index_values <- numeric(n_sim)
      
      for (i in seq_len(n_sim)) {
        mat <- cbind(stats::rnorm(sample_size), stats::rnorm(sample_size))
        
        index_values[i] <- tryCatch(
          index_fun(mat),
          error = function(e) {
            warning(sprintf(
              "Simulation %d failed at sample size %d: %s",
              i, sample_size, e$message
            ))
            NA_real_
          }
        )
      }
      
      tibble::tibble(
        sample_size  = sample_size,
        percentile95 = stats::quantile(index_values, 0.95, na.rm = TRUE)
      )
    },
    .options = furrr::furrr_options(seed = TRUE),
    .progress = TRUE
  )
  
  if (!is.null(csv_file)) {
    dir.create(dirname(csv_file), recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(results, csv_file, row.names = FALSE)
  }
  
  results
}
