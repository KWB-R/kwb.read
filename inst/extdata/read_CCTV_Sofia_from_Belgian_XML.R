library("kwb.utils")
library("kwb.sema")

runStartScript(sourceLibFiles = FALSE)

sema_set(paths = c(ALLPATHS$BASE, ALLPATHS$SOF))

PATHS <- resolve(c(ALLPATHS$BASE, ALLPATHS$SOF))

# M A I N ----------------------------------------------------------------------
if (FALSE)
{
  Sys.setlocale(locale = "Bulgarian_Bulgaria")
  
  input_dir_canasoft <- safe_sema_path("dir.raw.reli", "CCTV_7_xml")
  
  input_dir_onyx <- safePath(
    kwb.file::get_download_dir(), "GoogleDrive/XMLs_for_Reliable_Sewer"
  )
  
  files_canasoft <- kwb.file::dir_full_recursive_xml(input_dir_canasoft)
  files_onyx <- kwb.file::dir_full_recursive_xml(input_dir_onyx)
  
  contents_canasoft <- read_xml_as_path_value(files_canasoft)
  contents_onyx <- read_xml_as_path_value(files_onyx)
  
  full_canasoft <- merge_contents_with_file_info(contents_canasoft)
  full_onyx <- merge_contents_with_file_info(contents_onyx)
  
  full_content <- full_canasoft
  #full_content <- full_onyx
  
  path_value_list <- kwb.read:::split_path_value_data(full_content)
  
  path_value_inspection <- path_value_list$inspection
  path_value_observation <- path_value_list$observation

  inspections <- kwb.read:::to_table_of_inspections(path_value_inspection)
  inspections <- kwb.read:::remove_empty_columns(inspections)
  
  observations <- kwb.read:::to_table_of_observations(path_value_observation)
  observations <- kwb.read:::remove_empty_columns(observations)

  ##############################################################################
  
  contents_onyx <- kwb.read::read_cctv_from_xml_be(xmls = files_onyx)
  
  path_value <- rbindAll(contents_canasoft)
  
  CCTV_canasoft <- merge_contents(contents_canasoft)
  CCTV_onyx <- merge_contents(contents_onyx)
}

# merge_contents ---------------------------------------------------------------
merge_contents <- function(contents)
{
  list(
    inspections = safeRowBindAll(lapply(contents, kwb.sema::ins)),
    observations = safeRowBindAll(lapply(contents, kwb.sema::obs))
  )
}

# Debugging area ---------------------------------------------------------------
if (FALSE)
{
  x <- observations[[1]]
  
  which(sapply(observations, function(x) "V4.BAC" %in% names(x)))
  
  names(read_cctv_from_xml_be_(xml = files_onyx[3])$observations)
}
