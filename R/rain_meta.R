# distanceToNeighbour ----------------------------------------------------------

#' Distance Matrix to Matrix of Neighbours
#' 
#' Convert distance matrix to matrix of neighbours
#' 
#' @param mdist distance matrix as returned by \code{\link{getGaugeDistances}}
#'   
#' @return matrix of neighbour names with row names = gauge names and column
#'   names = \code{n1, n2, n<n.gauges - 1>}
#'
#' @export
#' 
distanceToNeighbour <- function(mdist)
{
  ## number of gauges
  n <- nrow(mdist)
  
  # Prepare matrix of neighbours (n gauges have n-1 neighbours each)
  # column names: n1, n2, ... n<n.gauges - 1>
  neighb <- matrix(nrow = n, ncol = n - 1, dimnames = list(
    rownames(mdist), paste0("n", seq_len(n - 1))
  ))
  
  ## Fill neighbour matrix
  for (i in seq_len(n)) {
    
    neighb[i, ] <- colnames(mdist)[-i][order(mdist[i, -i])]
  }
  
  neighb
}

# getGaugeDistances ------------------------------------------------------------

#' Returns Distances Between Gauges
#' 
#' Returns matrix containing relative distances between gauges
#' 
#' @param gaugeInfo data frame as returned by \code{getGaugeInfo}
#' @param gauges vector of names of gauges for which distances are to be
#'   calculated
#' @param dbg if \code{TRUE} (default), debug messages are shown
#' 
#' @export
#' @importFrom kwb.utils catIf
getGaugeDistances <- function(
  gaugeInfo = getGaugeInfo(),
  gauges = c(
    "NknI", "NknII", "ChbI", "BlnX", "BlnIX", "Wil", "Wila", "BlnV", "BlnXI", 
    "Lbg", "Hlg", "ZhlIe", "KoepIf", "Kar", "SpaII", "BlnIV"
  ),
  dbg = FALSE
) 
{
  ## inner function calculating the distance of two points (x1, y1) and (x2, y2)
  getDistance <- function(x1, y1, x2, y2) {
    
    sqrt((x2 - x1)^2 + (y2 - y1)^2)
  }
  
  ## Are there gauge names in our rain data that we do  not find in the
  ## distance table?
  gauges.missing <- setdiff(gauges, kwb.utils::selectColumns(gaugeInfo, "pw"))
  
  if (length(gauges.missing)) {
    
    warning(
      "\n*** Cannot find coordinates of the following gauges: ", 
      kwb.utils::stringList(gauges.missing), 
      ". They will not appear in the output matrix."
    )
    
    gauges <- setdiff(gauges, gauges.missing)
  }
  
  ## number of gauges
  n <- length(gauges)
  
  ## Init distance matrix with the gauge names as row and column names
  mdist <- matrix(nrow = n, ncol = n, dimnames = list(gauges, gauges))
  
  ## Loop through "start" rain gauges
  for (g1 in gauges) {
    
    kwb.utils::catIf(dbg, "Gauge", g1, ":\n")
    
    g1.info <- gaugeInfo[gaugeInfo$pw == g1, ]
    
    ## Loop through "stop" rain gauges
    for (g2 in gauges) {
      
      g2.info <- gaugeInfo[gaugeInfo$pw == g2, ] 
      
      distance <- getDistance(
        g1.info$xpos, g1.info$ypos, g2.info$xpos, g2.info$ypos
      )
      
      kwb.utils::catIf(dbg, sprintf("  dist to %10s: %6.0f m\n", g2, distance))
      
      ## Insert rounded distance between "start" and "stop" gauge into matrix
      mdist[g1, g2] <- round(distance)
    }
  }
  
  ## Return distance matrix
  mdist
}

# getGaugeInfo -----------------------------------------------------------------

#' Get Rain Gauge Coordinates from Rain Meta Database
#' 
#' @param mdb mdb containing table "tblPwInfo"
#' 
#' @return data frame with columns \code{pw, xpos, ypos}
#' 
#' @export
#' @importFrom kwb.db sqlForSelect hsSqlQuery
getGaugeInfo <- function(mdb = mdb_rain_meta())
{
  ## Get coordinates of gauges
  fields <- "Kurzzeichen AS pw, X_Wert AS xpos, Y_Wert AS ypos"
  
  sql <- kwb.db::sqlForSelect("tblPwInfo", fields, sqlDialect = "msaccess")
  
  ## read gauge information into gaugeInfo
  gaugeInfo <- kwb.db::hsSqlQuery(mdb, sql)
  
  ## Rename gauges in column "pw"
  gaugeInfo$pw <- gaugeNamesShort(gaugeInfo$pw)
  
  gaugeInfo
}

# BWB_RAIN_GAUGES --------------------------------------------------------------

#' BWB_RAIN_GAUGES
#' 
#' @param mdb path to MS Access Database Containing Meta Data
#' @export
#' @importFrom kwb.db hsGetTable 
BWB_RAIN_GAUGES <- function(mdb = mdb_rain_meta())
{
  result <- kwb.db::hsGetTable( 
    mdb = mdb, tbl = "BWBRR_to_BwbName", cond = "TRUE ORDER BY Kurzzeichen",
    fields = "STATION AS FUB_STATION, Kurzzeichen AS BWB_SHORT", 
    stringsAsFactors = FALSE, dbg = FALSE
  )
  
  result$FUB_SHORT = gsub("BWB", "b", result$FUB_STATION)
  
  result$BWB_CODE = gsub("\\s+", "", result$BWB_SHORT)
  
  result[, c("FUB_STATION", "FUB_SHORT", "BWB_SHORT", "BWB_CODE")]
}

# mdb_rain_meta ----------------------------------------------------------------

#' Path to Rain Meta Database (on KWB's server). Name of KWB server needs to 
#' be defined in environment variable \code{SERVERNAME}
#' @export
mdb_rain_meta <- function()
{
  sprintf("//%s/miacso$/Daten/ACCESS/Regen/0_META/BWB_Rain_Meta.mdb",
          Sys.getenv("SERVERNAME")
          )
}
