# read_cctv_from_xml_be --------------------------------------------------------

#' Read CCTV Data from XML File(s) in Belgian Standard Format
#' 
#' @param files (vector of) path(s) to XML file(s)
#' @param as_is if \code{TRUE} the column names in the table of observations are 
#'   more or less kept as they are in the xml files, otherwise, they are renamed
#'   to the names that are defined in the EN 13508.2 standard. These names are
#'   e.g. required by the RERAU evaluation functions in \code{kwb.rerau}. The
#'   default is \code{FALSE}.
#' @param dbg if \code{TRUE} (default), debug messages are shown
#' @importFrom janitor make_clean_names
#' @importFrom kwb.utils catIf fullySorted moveColumnsToFront noFactorDataFrame
#' @export
#' 
read_cctv_from_xml_be <- function(files, as_is = FALSE, dbg = TRUE)
{
  #kwb.utils::assignPackageObjects("kwb.read")
  #kwb.utils::assignArgumentDefaults(kwb.read::read_cctv_from_xml_be)
  
  contents <- lapply(seq_along(files), function(i) {
    
    file <- files[i]
    
    kwb.utils::catIf(dbg, sprintf(
      "Reading %3d/%3d: '%s' ...\n", i, length(files), basename(file)
    ))
    
    try({
      
      path_value <- read_xml_as_path_value(file, dbg = dbg)
      
      path_value$path <- simplify_paths(x = path_value$path)
      
      path_value
    })
  })

  names(contents) <- janitor::make_clean_names(basename(files))
  
  # Which files could not be read correctly?
  failed <- sapply(contents, inherits, "try-error")
  
  # Remove contents that could not be read correctly
  contents <- contents[! failed]

  #columns_obs_onyx <- kwb.read::get_overview_of_columns(lapply(contents, obs))
  #columns_ins_onyx <- kwb.read::get_overview_of_columns(lapply(contents, ins))

  # Merge contents adding a column "File"
  full_content <- merge_contents_with_file_info(contents)
  
  # Split path-value data into inspection and observation related data
  path_value_list <- split_path_value_data(full_content)

  # Convert path and value information to wide data
  inspections <- to_table_of_inspections(path_value_list$inspection)
  observations <- to_table_of_observations(path_value_list$observation)

  get_integer_suffix <- function(x) as.integer(sapply(strsplit(x, "_"), "[", 2))
  
  inspections$InspectionNo <- get_integer_suffix(inspections$InspectionNo)
  
  observations$InspectionNo <- get_integer_suffix(observations$InspectionNo)
  observations$ObservationNo <- get_integer_suffix(observations$ObservationNo)
  observations$ContDefect <- get_continuous_defect(observations)

  observations <- merge_remarks(observations)

  result <- list(
    inspections = kwb.utils::fullySorted(remove_empty_columns(inspections)), 
    observations = kwb.utils::fullySorted(remove_empty_columns(observations))
  )

  if (! as_is) {
    
    result$observations <- rename_for_scoring(result$observations)
    result$observations <- kwb.utils::moveColumnsToFront(
      result$observations, c("File", "InspectionNo", "ObservationNo")
    )
    
    # Convert types
    result$inspections <- convert_types(result$inspections)
    result$observations <- convert_types(result$observations)
  }

  structure(
    result, 
    file_info = kwb.utils::noFactorDataFrame(
      file_id = names(contents), 
      file_path = files
    ), 
    which_failed = which(failed)
  )
}

# simplify_paths ---------------------------------------------------------------
#' @importFrom kwb.utils multiSubstitute
simplify_paths <- function(x)
{
  substitutions <- list(
    "^/INSPECTION/PIPELINE/" = "",
    
    # Inspections and observations may be indexed with [i] ...
    "DIRECT_OPTICAL_PIPELINE_INSPECTION\\[(\\d+)\\]" = "inspection_\\1",
    "DIRECT_OPTICAL_PIPELINE_OBSERVATION\\[(\\d+)\\]" = "observation_\\1",
    
    # ... but there may be also only one inspection
    "DIRECT_OPTICAL_PIPELINE_INSPECTION/" = "inspection_1/"
    
    , "/CONTINUOUS_DEFECT/TYPE$" = "/ContDefect_Type"
    , "/CONTINUOUS_DEFECT/CODE$" = "/ContDefect_Code"
    , "(_C[12])_[123]$" = "\\1"
    , "_C[12]_2/COMMENT$" = "_C1_comment"
    , "_C[12]_2/CODE_WITH_COMMENT$" = "_C1"
    , "_C1_2/CODE_WITH_PREFIX$" = "_C1_prefix"
    , "_C1_2/SUBCODE_X$" = "_C1_x"
    , "(_CODE1_[23])/CODE_WITH_GENERAL_REMARK$" = "\\1"
    , "_CODE1_[23]/[A-Z]{3}$" = "_C1_comment"
    , "_C2_1$" = "_C2"
    , "_Q1_1$" = "_Q1"
    , "(_Q[12])_[123]/VALUE_(LESS_THEN_1000_MM|NUMBER|PERCENTAGE|DEGREES|MM(_DECIMAL)?)$" = "\\1"
    , "/([A-Z]{3})/COORDINATES/(X|Y)_POSITION$" = "/\\1_\\2"
    , "/([A-Z]{3})/\\1_(C1(_comment|_prefix|_x)?|C2|Q[12])$" = "/\\1_\\2"
    , "/([A-Z]{3})/(CL[12])$" = "/\\1_\\2"
    , "/([A-Z]{3})/\\1_CODE1_[123]$" = "/\\1"
    , "/([A-Z]{3})/Remarks$" = "/\\1_Remarks"
    , "/ABH/NAME_INSPECTOR$" = "/ABH_inspector"
    , "/ABH/NAME_INSPECTORS_COMPANY$" = "/ABH_company"
    , "/ADB/VALUE_TEMPERATURE_CELCIUS$" = "/ADB_temperature"
    , "/FULL_POSITIONING_FINDING/([A-Z]{3})/\\1_(X|Y|Z)$" = "/\\1_\\2"
    , "/FULL_POSITIONING_FINDING/([A-Z]{3})$" = "/\\1"
    , "/FULL_POSITIONING_FINDING/([A-Z]{3})/(X|Y)_POSITION$" = "/\\1_\\2"
    , "/(ACN)/YEAR_RANGE_C_I_O$" = "/\\1"
    , "/BBE/BBD_Q1$" = "/BBD_Q1"
  )

  kwb.utils::multiSubstitute(x, substitutions)
}

# merge_contents_with_file_info ------------------------------------------------
#' @importFrom kwb.utils setColumns
merge_contents_with_file_info <- function(contents)
{
  do.call(rbind, lapply(names(contents), function(name) {
    
    x <- contents[[name]]
    
    kwb.utils::setColumns(x, path = paste0(name, "/", x$path), dbg = FALSE)
  }))
}

# split_path_value_data --------------------------------------------------------
#' @importFrom kwb.utils selectColumns
split_path_value_data <- function(path_value)
{
  # Select the paths only
  paths <- kwb.utils::selectColumns(path_value, "path")
  
  # Do the paths correspond to observations?
  is_observation <- is_observation_path(paths)

  # List of two components: inspection-related and observation-related data
  list(
    inspection = path_value[! is_observation, ],
    observation = path_value[is_observation, ]
  )
}

# is_observation_path ----------------------------------------------------------
is_observation_path <- function(xml_path)
{
  grepl("/observation_\\d+/", xml_path)
}

# to_table_of_inspections ------------------------------------------------------
to_table_of_inspections <- function(path_value_inspection)
{
  #kwb.utils::assignPackageObjects("kwb.read")
  
  # Define (key) column names
  columns <- c("File", "InspectionNo", "Variable")

  # Split path column into subdirectory columns
  subdirs_value <- split_path_to_subdirs(path_value_inspection, columns)
  
  # Convert long format to wide format
  reshape_to_wide(x = subdirs_value)
}

# split_path_to_subdirs --------------------------------------------------------
#' @importFrom kwb.utils selectColumns setColumns
split_path_to_subdirs <- function(path_value, key_columns)
{
  paths <- kwb.utils::selectColumns(path_value, "path")
  values <- kwb.utils::selectColumns(path_value, "value")
  
  stop_on_too_deep_paths(paths, max_depth = length(key_columns))
  
  subdirs <- to_subdir_data(paths, column_names = key_columns)
  
  kwb.utils::setColumns(subdirs, Value = values, dbg = FALSE)
}

# stop_on_too_deep_paths -------------------------------------------------------
stop_on_too_deep_paths <- function(paths, max_depth)
{
  deep_paths <- get_too_deep(paths, max_depth)
  
  if (length(deep_paths)) {
    
    stop(
      "The following paths are too deep and need to be treated in ",
      "to_insp_table():\n", kwb.utils::collapsed(deep_paths, "\n"), 
      call. = FALSE
    )
  }
}

# get_too_deep -----------------------------------------------------------------
get_too_deep <- function(x, max_depth)
{
  parts <- strsplit(x, "/")
  
  lengths <- sapply(parts, length)
  
  x[lengths > max_depth]
}

# to_subdir_data ---------------------------------------------------------------
#' @importFrom kwb.utils asNoFactorDataFrame
#' @importFrom kwb.file to_subdir_matrix 
to_subdir_data <- function(paths, column_names = NULL, dbg = FALSE)
{
  subdirs <- kwb.utils::asNoFactorDataFrame(kwb.file::to_subdir_matrix(paths))
  
  set_data_frame_column_names(subdirs, column_names)
}

# reshape_to_wide --------------------------------------------------------------
#' @importFrom kwb.utils resetRowNames
#' @importFrom stats reshape
reshape_to_wide <- function(x, timevar = "Variable")
{
  idvar <- setdiff(names(x), c(timevar, "Value"))
  
  result <- stats::reshape(
    x, direction = "wide", timevar = timevar, idvar = idvar
  )

  names(result) <- gsub("^Value\\.", "", names(result))
  
  # Reset row names and remove attribute "reshapeWide"
  result <- structure(kwb.utils::resetRowNames(result), reshapeWide = NULL)
  
  rearrange_columns(result, idvar)
}

# rearrange_columns ------------------------------------------------------------
#' @importFrom kwb.utils moveToFront
rearrange_columns <- function(data_frame, keys = NULL)
{
  keys <- intersect(names(data_frame), keys)
  
  # Order columns by name but put the first two of column_names first
  data_frame[, kwb.utils::moveToFront(sort(names(data_frame)), keys)]
}

# to_table_of_observations -----------------------------------------------------
#' @importFrom kwb.utils catAndRun isNaOrEmpty selectColumns
to_table_of_observations <- function(path_value_observation, dbg = TRUE)
{
  #kwb.utils::assignPackageObjects("kwb.read")
  
  # Select the values
  values <- kwb.utils::selectColumns(path_value_observation, "value")
  
  # Which values are empty?
  is_empty <- kwb.utils::isNaOrEmpty(values)
  
  kwb.utils::catAndRun(
    dbg = dbg, 
    sprintf("Removing %d empty observations", sum(is_empty)),
    path_value_observation <- path_value_observation[! is_empty, ]
  )
  
  # Select the paths
  paths <- kwb.utils::selectColumns(path_value_observation, "path")
  
  # Does the path represent a three letter code?
  is_code <- is_three_letter_code_path(paths)

  # Extract general information on the observations
  path_value_general <- path_value_observation[! is_code, ]

  # Extract code related information on the observations
  path_value_codes <- path_value_observation[is_code, ]

  # Define (key) column names
  columns <- c("File", "InspectionNo", "ObservationNo", "Variable")
  
  # Split path column into subdirectory columns for general observations
  subdirs_value_general <- split_path_to_subdirs(path_value_general, columns)
  
  # Split path column into subdirectory columns for code observations
  subdirs_value_codes <- split_path_to_subdirs(path_value_codes, columns)

  # Split Variable column into columns Code and Suffix
  subdirs_value_code_suffix <- split_code_variable(subdirs_value_codes)

  # Convert to general observation data frame
  obs_general <- reshape_to_wide(subdirs_value_general)

  # Convert to code observation data frame
  obs_codes <- reshape_to_wide(subdirs_value_code_suffix)
  
  merge(obs_general, obs_codes)
}

# is_three_letter_code_path ----------------------------------------------------
is_three_letter_code_path <- function(xml_path)
{
  grepl("observation_\\d+/[A-Z]{3}(_|$)", xml_path)
}

# split_code_variable ----------------------------------------------------------
#' @importFrom kwb.utils selectColumns setColumns
split_code_variable <- function(subdirs_value)
{
  # Get values from column "Variable"
  variables <- kwb.utils::selectColumns(subdirs_value, "Variable")
  
  # Split the variable names at underscore
  variable_parts <- strsplit(variables, "_")
  
  # The first parts are the main EU codes
  codes <- sapply(variable_parts, "[", 1)
  
  # Stop if there are unknown codes
  stop_on_invalid_codes(codes)
  
  # Paste the remaining parts to suffixex
  suffixes <- sapply(lapply(variable_parts, "[", -1), paste, collapse = "_")
  
  # Set default suffix for empty suffixes
  suffixes[suffixes == ""] <- "C1_comment" # "General"
  
  # Overwrite the Variable column with the suffixes
  subdirs_value$Variable <- suffixes
  
  # Store the EU main codes in a new column "Code"
  kwb.utils::setColumns(subdirs_value, Code = codes)
}

# stop_on_invalid_codes --------------------------------------------------------
#' @importFrom kwb.en13508.2 getCodes
stop_on_invalid_codes <- function(codes)
{
  valid_codes <- sort(unique(kwb.en13508.2::getCodes()$Code))
  
  stopifnot(all(sort(unique(codes)) %in% valid_codes))
}

# get_continuous_defect --------------------------------------------------------
#' @importFrom kwb.utils pasteColumns0
get_continuous_defect <- function(observations)
{
  columns <- paste0("ContDefect_", c("Type", "Code"))
  
  if (!all(columns %in% names(observations))) {
    return(NULL)
  }
  
  result <- rep("", nrow(observations))
  
  defect_codes <- kwb.utils::pasteColumns0(observations, columns)
  
  is_available <- ! is.na(observations[[columns[1]]])
  
  result[is_available] <- defect_codes[is_available]
  
  result
}

# convert_types ----------------------------------------------------------------
#' @importFrom kwb.utils catAndRun stringList
convert_types <- function(df)
{
  # Names of columns to be converted to numeric
  numeric_columns <- c("I", "ABQ", "ACB", "ACC", "ACG", "ACH", "ACI")
  numeric_columns <- intersect(numeric_columns, names(df))

  # Add columns that contain coordinates
  numeric_columns <- c(numeric_columns, grep("_[XY]$", names(df), value = TRUE))
  
  # Names of columns to be converted to integer
  integer_columns <- c("G", "H")
  integer_columns <- intersect(integer_columns, names(df))

  # Define helper function to generate message
  msg <- function(x, kind) {
    sprintf("Converting %s to %s", kwb.utils::stringList(x), kind)
  }
  
  # Apply as.numeric to the numeric and as.integer to the integer columns
  if (length(numeric_columns)) {

    kwb.utils::catAndRun(
      msg(numeric_columns, "numeric"),
      df[numeric_columns] <- lapply(df[numeric_columns], as.numeric)
    )
  }
  
  if (length(integer_columns)) {

    kwb.utils::catAndRun(
      msg(integer_columns, "integer"),
      df[integer_columns] <- lapply(df[integer_columns], as.integer)
    )
  }
  
  df
}

# rename_for_scoring -----------------------------------------------------------
#' @importFrom kwb.utils moveColumnsToFront renameColumns
rename_for_scoring <- function(observations)
{
  renamings <- list(
    Code = "A", 
    C1 = "B", 
    C2 = "C", 
    Q1 = "D", 
    Q2 = "E", 
    REMARKS = "F",
    CL1 = "G", 
    CL2 = "H",
    LONGITUDINAL_LOCATION = "I", 
    ContDefect = "J",
    JOINT = "K",
    PHOTO_REF = "M",
    VIDEO_REF = "N"
  )

  # Rename and select relevant columns of the table of observations (Onyx)
  result <- kwb.utils::renameColumns(observations, renamings = renamings)
  
  columns <- sort(unlist(unname(renamings)))
  
  kwb.utils::moveColumnsToFront(result, intersect(columns, names(result)))
}

# merge_remarks ----------------------------------------------------------------
#' @importFrom kwb.utils parallelNonNA removeColumns renameColumns
merge_remarks <- function(observations)
{
  if (all(c("REMARKS", "Remarks") %in% names(observations))) {
    
    remarks <- kwb.utils::parallelNonNA(
      observations$REMARKS, observations$Remarks
    )
    
    observations$REMARKS <- remarks
    
    observations <- kwb.utils::removeColumns(observations, "Remarks")
  }

  kwb.utils::renameColumns(observations, list(Remarks = "REMARKS"))
}

# get_overview_of_columns ------------------------------------------------------

#' Get Overview of Columns in Data Frames
#' 
#' @param x list of data frames
#' 
#' @return data frame with each row representing one data frame in \code{x} and
#'   with as many columns as there are distinct columns in all of the data 
#'   frames in \code{x}. In a row \emph{i} the data frame contains "x" in those 
#'   columns that are contained in the \emph{i}-th data frame in list \code{x}
#'   and an empty string otherwise.
#' @importFrom kwb.utils asNoFactorDataFrame
#' @export
#' 
#' @examples 
#' get_overview_of_columns(list(
#'   df1 = data.frame(a = 1, b = 2), 
#'   df2 = data.frame(b = 3, c = 4),
#'   df3 = data.frame(a = 5, c = 6)
#' ))
#' 
get_overview_of_columns <- function(x)
{
  stopifnot(is.list(x))
  stopifnot(all(sapply(x, is.data.frame)))
  
  column_list <- lapply(x, names)
  
  all_columns <- sort(unique(unlist(column_list)))
  
  has_column <- do.call(rbind, lapply(
    column_list, function(x) all_columns %in% x
  ))
  
  colnames(has_column) <- all_columns
  
  mode(has_column) <- "character"
  
  has_column[has_column == "TRUE"] <- "x"
  has_column[has_column == "FALSE"] <- ""
  
  kwb.utils::asNoFactorDataFrame(has_column)
}
