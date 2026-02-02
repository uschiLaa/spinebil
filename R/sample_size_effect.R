#' Simulate Effect of Sample Size on Projection Pursuit Index Under Gaussian Noise
#'
#' For a given index function, simulates how the index behaves across a range
#' of sample sizes when applied to pairs of standard normal noise.
#' 
#' @param index_fun A function taking a 2-column numeric matrix (x, y) and returning a scalar.
#' @param sample_sizes Numeric vector (e.g., 100, c(50,100,200), seq(30,200,5)). Default is 100.
#' @param n_sim Integer. Number of simulations per sample size.
#' @param csv_file Optional file path to save results as CSV.
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
                                  sample_sizes = 100,
                                  n_sim = 100,
                                  csv_file = NULL) {
  stopifnot(is.function(index_fun))
  
  total <- length(sample_sizes) * n_sim
  pb <- utils::txtProgressBar(min = 0, max = total, style = 3)
  on.exit(close(pb), add = TRUE)
  
  done <- 0L
  
  results <- dplyr::bind_rows(lapply(sample_sizes, function(n) {
    index_values <- numeric(n_sim)
    
    for (i in seq_len(n_sim)) {
      mat <- cbind(stats::rnorm(n), stats::rnorm(n))
      index_values[i] <- index_fun(mat)
      
      done <<- done + 1L
      utils::setTxtProgressBar(pb, done)
    }
    
    tibble::tibble(
      sample_size  = n,
      percentile95 = stats::quantile(index_values, 0.95, na.rm = TRUE)
    )
  }))
  
  if (!is.null(csv_file)) {
    dir.create(dirname(csv_file), recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(results, csv_file, row.names = FALSE)
  }
  
  results
}
