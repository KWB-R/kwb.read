# remove_empty_columns ---------------------------------------------------------
remove_empty_columns <- function(x)
{
  is_character <- sapply(x, is.character)
  
  x[is_character] <- lapply(x[is_character], kwb.utils::hsTrim)
  
  x[x == ""] <- NA
  
  kwb.utils::removeEmptyColumns(x)
}

# set_data_frame_column_names --------------------------------------------------
set_data_frame_column_names <- function(x, column_names = NULL)
{
  stopifnot(is.data.frame(x))
  
  # Return x if no column_names are given
  if (is.null(column_names)) {
    
    return(x)
  }
  
  # Number of columns in x
  n_required <- ncol(x)
  
  # Number of column names given in column_names
  n_given <- length(column_names)
  
  # Stop if there are not enough column names
  if (n_given < n_required) {
    
    stop_(
      "Not enough column names given to set_data_frame_column_names():\n",
      sprintf("Required: %d, given: %d\n", n_required, n_given)
    )
    
  } else {
    
    # Set column names with as many elements from column_names as required
    stats::setNames(x, column_names[seq_len(n_required)])
  }
}

# stop_ ------------------------------------------------------------------------
stop_ <- function(...)
{
  stop(..., call. = FALSE)
}

# stop_moved -------------------------------------------------------------------
stop_moved <- function(name, package = "kwb.prep")
{
  stop(call. = FALSE, sprintf(
    "The function '%s' has been moved to package '%s'", name, package
  ))
}
