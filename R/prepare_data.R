#' Prepare plant growth data
#'
#' This function reads a CSV file containing plant growth data and splits the "file" column into multiple columns
#' based on underscores. The user can specify the names of these new columns.
#'
#' @param filepath Character. The path to the CSV file.
#' @param new_colnames Character vector. Names for the new columns created from splitting the "file" column.
#' @return A data frame with the original data plus the new columns.
#' @export
#'
prepare_data <- function(filepath, new_colnames = c("genotype", "treatment", "condition", "replicate")) {
  # Read the CSV
  data <- read.csv(filepath, stringsAsFactors = FALSE)

  # Count how many splits are needed based on user input
  splits <- length(new_colnames)

  # Split the "file" column into multiple columns
  # If the filename structure might sometimes be shorter (3 instead of 4 parts), handle that gracefully
  split_cols <- tidyr::separate(data, col = "file", into = new_colnames[1:splits], sep = "_", remove = FALSE)

  return(split_cols)
}

