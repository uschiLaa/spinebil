#' Simulate and Compare Index Scale on Structured vs Noisy Data
#'
#' Performs simulations to compute a projection pursuit index on structured
#' (sampled) data and on random noise, allowing a comparison of index scale across contexts.
#'
#' 
#' @param data A data frame or tibble with at least two numeric columns.
#' @param index_fun A function that takes two numeric vectors (`x`, `y`) and returns a numeric scalar index.
#' @param n_sim Integer. Number of simulations. Default is 100.
#' @param n_obs Integer. Number of observations per simulation. Default is 500.
#' @param seed Optional integer seed for reproducibility.
#'
#' @return A tibble with columns:
#' - `simulation`: simulation number
#' - `var_i`, `var_j`: variable names
#' - `var_pair`: pair name as a string
#' - `sigma`: 0 for structured data, 1 for noisy data
#' - `index`: index value returned by `index_fun`
#'
#' @examples
#' ppi_scale(data_gen("polynomial", degree = 3), scag_index("stringy"), n_sim = 10)
#' 
#' @export
ppi_scale <- function(data,
                       index_fun,
                       n_sim = 100,
                       n_obs = 500,
                       seed = NULL) {
  
  if (!is.data.frame(data)) data <- as.data.frame(data)
  stopifnot(ncol(data) >= 2, nrow(data) >= 2)
  if (!is.null(seed)) set.seed(seed)
  
  col_pairs <- utils::combn(seq_along(data), 2, simplify = FALSE)
  future::plan(future::multisession, workers = max(1, parallel::detectCores() - 1))
  
  results <- furrr::future_map_dfr(seq_len(n_sim), function(sim) {
    
    purrr::map_dfr(col_pairs, function(pair) {
      i <- pair[1]
      j <- pair[2]
      x_struct <- data[[i]]
      y_struct <- data[[j]]
      mat1 <- cbind(x_struct, y_struct)
      
      index_0 <- tryCatch(index_fun(mat1), error = function(e) NA_real_)
          
      x_noise <- scale(stats::rnorm(n_obs))[, 1]
      y_noise <- scale(stats::rnorm(n_obs))[, 1]
      mat2 <- cbind(x_noise, y_noise)
        
      index_1 <- tryCatch(index_fun(mat2), error = function(e) NA_real_)
      
      var_names <- names(data)[pair]
      
      tibble::tibble(
        simulation = sim,
        var_i = var_names[1],
        var_j = var_names[2],
        var_pair = paste0(var_names[1], "-", var_names[2]),
        sigma = c(0, 1),
        index = c(index_0, index_1)
      )
    })
  }, .options = furrr::furrr_options(seed = TRUE), .progress = TRUE)
  
  future::plan(future::sequential)
  return(results)
}

