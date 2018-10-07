# Formatted kable tables for Word output ---------

#' Formatted kable tables for Word output.
#'
#' @param x A matrix or dataframe with defined column names.
#' @param digits_arg The number of digits after 0 to appear in table. Set at 2 by default.
#' @param align_args A scalar or vector of length = ncol(). Values are "r", "l", or "c". Set at "r" by default.
#' @param rownames_bool A logical. By default, FALSE, so that no rows are printed.
#' @return A nice table in the knitted Word document.
FMTD_KABLE <- function(x, digits_args, align_args, rownames_bool, big.mark) {
  if (missing(rownames_bool)) (rownames_bool <- FALSE)

  if (rownames_bool == TRUE & all(rownames(x) == 1:dim(x)[1])){
    rownames(x) <- paste(rownames(x), "")
  }

  if (missing(digits_args)) (digits_args <- 2)
  if (missing(align_args)) (align_args <- "r")
  if (missing(big.mark))(big.mark <- "")

  x <- as.data.frame(x)
  knitr::kable(x, row.names = rownames_bool, colnames = colnames(x),
               digits = digits_args, align = align_args,
               format.args = list(nsmall = digits_args, big.mark  = big.mark))
}

# Write scalar or vector in text of knitted output -----------------

#' Write scalar or vector in text of knitted output.
#'
#' @param x A vector of length larger than 1.
#' @return A list in text separated with "," and "and".
PASTE_INTEXT <- function(x) {
  aa <- length(x)
  if (length(x)>2){
    bb <- paste(x[-aa], collapse = ", ")
    cc <- paste(bb, x[aa], sep=" and ")
  }
  else if (length(x)==2) {
    cc <- paste(x[1], x[2], sep=" and ")
  }
  return(cc)
}

# Read .xlsx master file ----------------------------

#' Read .xlsx master file
#'
#' @param tab_name_as_string For example, "Sheet1".
#' @return The data table contained in the tab of the master file.
READ_EXCEL <- function(tab_name_as_string, input_path) {
  x<- try(as.data.frame(read_excel(paste(input_path,"/Master_table.xlsx", sep=""), sheet = tab_name_as_string, skip=3)), silent = TRUE)
  return(x)
}

# Create folder in a directory ---------------------------------------------------------------------------

#' Create folder in a directory.
#' @param absolute_path string of the directory where the folder should be created.
#' @param name_folder string of the folder name to be created.
#' @description A folder is created if it does not exist yet.
#' @return A string of the entire path of the created folder.
CREATE_AND_NAME_FOLDER <- function(absolute_path, name_folder){
  a <-dir.create(file.path(absolute_path, name_folder), showWarnings = FALSE)
  return (list(paste(absolute_path,name_folder, sep="/"), a))
}
# Create a matrix of NAs by defining row names and row columns --------------------------------------------

#' Create a matrix of NAs by defining row names and row columns.
#'
#' @param rowname_v A vector of desired names for rows.
#' @param colname_v A vector of desired names for columns.
#' @return A matrix.
NA_SUMMARY_M <- function (rowname_v,colname_v ) {
  x <- matrix(NA, nrow=length(rowname_v), ncol= length(colname_v))
  rownames(x) <- rowname_v
  colnames(x) <- colname_v
  return ( x )
}

# Transform coordinates in table to a point shapefile ----------------------
#' Transform coordinates in table to a point shapefile.
#'
#' @param points_shp A dataframe with columns of longitude and latitude, named "longitude" and "latitude".
#' @return A shapefile of points
TRANSFORM_TABLE_SHP <- function(points_shp) {
  coordinates(points_shp) = ~ longitude + latitude
  proj4string(points_shp) = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  return(points_shp)
}


# Rasters: Are CRS, resolution, extent, and cell number the same? -----------------------------------------
#' Rasters: Are CRS, resolution, extent, and cell number the same?
#'
#' @param incoming_r Raster object to be compared.
#' @param present_r Raster object that is the basis of the comparison.
#' @return nothing if match and a warning if mismatch.
FIX_PROJ_R <- function (incoming_r, present_r) {
  x <- incoming_r
  test_happy <- rep("ok",4)
  test_v <- test_happy

  if(proj4string(x) != proj4string(present_r)) {
    test_v[1] <- warning("CRS does not match, ")
  }

  if (any(round(res(x),7) != round(res(present_r),7))) {
    test_v[2] <- warning("resolution does not match, ")
  }

  if (any(round(extent(x)[],5) != round(extent(present_r)[],5))){
    test_v[3] <- warning("extent does not match,")
  }

  if(ncell(x) != ncell(present_r)) {
    test_v[4] <- warning("ncell does not match,")
  }

  if (any(test_v != test_happy)) (return(stop()))
}

# Is the projection of a .shp file the same as the one of a raster? --------------------------------------

#' Is the projection of a .shp file the same as the one of a raster?
#'
#' @param incoming_shp shp.file like object to be compared.
#' @param present_r Raster object that is the basis of the comparison.
#' @return nothing if match and a warning if mismatch.
FIX_PROJ_SHP = function (incoming_shp, present_r) {
  x <- incoming_shp
  test <- rep("ok",1)

  if(proj4string(x) != proj4string(present_r)){
    stop("CRS does not match, ")
    # you could use: x <- spTransform(x, proj4string(present_r))
  }
}

# (Save) and open rasters -------------------------------------------------------------------------------

#' Save and open rasters.
#'
#' @param raster_r The raster object to be saved and opened.
#' @param mainDir The path string of the folder than contains the .tiff file.
#' @param subDir_rasterName The name of the folder than contains the .tiff file. This name is also the name of the .tiff file.
#' @return The saved raster.
SAVEandOPEN_R <- function(raster_r, mainDir, subDir_rasterName) {
  a <-dir.create(file.path(mainDir, subDir_rasterName), showWarnings = FALSE)
  filename = paste(mainDir, subDir_rasterName, paste(subDir_rasterName, ".tif", sep=""), sep="/")
  try(writeRaster(raster_r, filename = filename, format = "GTiff", overwrite=TRUE), silent =TRUE) # note that the date of the tif file does not change even if the code runs ok.
  raster_r <- raster(filename )
  return(raster_r)
}

#' Open rasters.
#'
#' @param mainDir The path string of the folder than contains the .tiff file.
#' @param subDir_rasterName The name of the folder than contains the .tiff file. This name is also the name of the .tiff file.
#' @return The raster that is opened.
OPEN_R <- function(mainDir, subDir_rasterName) {
  filename = paste(mainDir, subDir_rasterName, paste(subDir_rasterName, ".tif", sep=""), sep="/")
  raster_r <- raster(   filename   )
  return(raster_r)
}


# (Save) and open .shp files ----------------------------------------------------------------------------

#' Save and open .shp files.
#'
#' @param shapefile_shp The .shp file like object to be saved and opened.
#' @param mainDir The path string of the folder than contains the .shp file.
#' @param subDir_shpName The name of the folder than contains the .shp file. This name is also the names of the .shp file and related files.
#' @return The saved .shp file.
SAVEandOPEN_SHP <- function(shapefile_shp, mainDir, subDir_shpName) {
  dir.create(file.path(mainDir, subDir_shpName), showWarnings = FALSE)
  path_name <- paste(mainDir, subDir_shpName, sep="/")
  try(writeOGR(shapefile_shp, path_name , subDir_shpName, "ESRI Shapefile", overwrite_layer = TRUE), silent = TRUE)
  shapefile_shp <- readOGR(path_name, subDir_shpName, stringsAsFactors = FALSE, verbose=FALSE)
  return(shapefile_shp)
}

#' Open .shp files.
#'
#' @param mainDir The path string of the folder than contains the .shp file.
#' @param subDir_shpName The name of the folder than contains the .shp file. This name is also the names of the .shp file and related files.
#' @return The .shp file that is opened.
OPEN_SHP <- function(mainDir, subDir_shpName) {
  path_name <- paste(mainDir, subDir_shpName, sep="/")
  shapefile_shp <- readOGR(path_name, subDir_shpName, stringsAsFactors = FALSE, verbose= FALSE)
  return(shapefile_shp)
}

# Calculate area size ---------------------------------------------

#' Calculate area size.
#'
#' @param x_shp A polygon in Geographic Coordinate System.
#' @param byid_area TRUE or FALSE. Is the area calculated for each row?
#' @param unit_area Accepts only one of these two values: "m2" or "km2".
#' @return Area size
CALC_AREA <- function(x_shp, byid_area, unit_area){
  aa <- c(1, 10^6)
  bb <- c("m2", "km2")
  denom <- aa[bb == unit_area]
  x_shp <-spTransform(x_shp, CRS("+init=epsg:32645"))
  return (gArea(x_shp, byid = byid_area) / denom)
}

# Kolmogorov-Smirnoff test statistic -----------------------------

#' Kolmogorov-Smirnoff test statistic.
#'
#' @param x A vector. Value to fit.
#' @param F An object of class function. A probability density function.
#' @param ... Further arguments to be passed to F.
#' @return A scalar. The value of the Kolmogorov-Smirnoff test statistic.
KS <- function(x, F, ...) {
  n <- length(x)
  x_sort <- sort(x)
  tmp <- c((1:n)/n - F(x_sort, ...),
           F(x_sort) - (0:(n-1))/n)
  max(tmp)
}

# Flood rasters ----------------------------------------------

# Matricise a raster

#' Matricise a raster.
#'
#' @param x A raster.
#' @return A matrix.
MATRICISE <- function (x) {matrix(x[], ncol = ncol(y), byrow = TRUE)}

# Uni-directional spread of river reaches

#' Uni-directional spread of river reaches.
#'
#' @param y A raster object. It shows river network according to the river reach ID. NA stands for no rivers.
#' @param max_extent A scalar. The number of pixels to go in each side of the rivers to mark the potential flooded areas.
#' @param fill_dir_v A vector of length = unique(y[]) accepting "H" or "V" values only. If a river reach flows in average following latitudes, the filling is done vertically ("V"). Otherwise, the direction of filling is "H" when the river reach flows in average along longitudes.
#' @return An object of type list of length = unique(y[]) and that contains rasters.
SPREAD_RIVER <- function(y, max_extent, fill_dir_v) {

  river_extent_l <- list()
  dd             <- ncol(y)
  oo             <- nrow(y)

  unique_id <- unique(c(y[]))
  unique_id <- unique_id[!is.na(unique_id)]
  unique_id <- unique_id[order(unique_id)]

  for (k in 1:length(unique_id)) {

    x                     <- y
    id_reach              <- unique_id[k]
    x[x != id_reach ]     <- NA
    cc                    <- which(x[] == id_reach)
    row_id                <- ceiling(cc / dd) # row
    col_id                <- cc - (row_id - 1) * dd # column

    if (fill_dir_v[k] == "H") {

      index_min <- (col_id - max_extent) > 0
      index_max <- (col_id + max_extent) <= dd

      nrow_left <- (col_id - max_extent)
      nrow_left[index_min == FALSE] <- 1

      nrow_right <- (col_id + max_extent)
      nrow_right[index_max == FALSE] <- dd

      for (i in 1:length(cc)) {
        x[row_id[i],nrow_left[i]:nrow_right[i]] <- id_reach
      }

    } else if (fill_dir_v[k] == "V") {

      index_min <- (row_id - max_extent) > 0
      index_max <- (row_id + max_extent) <= oo

      ncol_up <- (row_id - max_extent)
      ncol_up[index_min == FALSE] <- 1

      ncol_down <- (row_id + max_extent)
      ncol_down[index_max == FALSE] <- oo

      for (i in 1:length(cc)) {
        x[ncol_up[i]:ncol_down[i], col_id[i]] <- id_reach
      }

    } else {
      stop("Value of elements in fill_dir_v is either 'H' or 'V'.")
    }

    river_extent_l[[k]] <- x
  }

  river_extent_l

}

# Uni-directional pouring of water from river reaches to potential flooded area

#' Uni-directional pouring of water from river reaches to potential flooded area
#'
#' @param DEM A raster. Shows elevation. DEM and the rasters in argument ll must have the same projection and dimension.
#' @param y A raster object. It shows river network according to the river reach ID. NA stands for no rivers.
#' @param ll A list. It has a length equal to the number of river reaches and contains rasters. A raster marks the potentially flooded areas. NAs stand for non potentially flooded areas. The argument DEM and the rasters in ll must have the same projection and dimension.
#' @param fill_dir_v A vector of length = length(ll) and accepting "H" or "V" values only. If a river reach flows in average following latitudes, the filling is done vertically ("V"). Otherwise, the direction of filling is "H" when the river reach flows in average along longitudes.
#' @param stage A vector of length = length(ll). Numerical that represents the water stage for each river reach.
#' @return An array of dim = c(nrow(DEM), ncol(DEM) , length(ll)). Each matrix element is a boolean raster than indicates the pixels that are flooded.

POUR_WATER <- function(DEM, y, ll, fill_dir_v, stage) {

  res_list <- array(data = NA, dim = c(nrow(DEM), ncol(DEM) , length(ll)))
  dem_safe_m <- MATRICISE(DEM)
  y_safe_m <- MATRICISE(y)

  for (i in 1:length(ll)) {

    extent_m <- MATRICISE(ll[[i]])

    dem_m <- dem_safe_m
    dem_m[is.na(extent_m)] <- NA
    dem_m

    y_m <- y_safe_m
    y_m[ y_m != i]  <- NA
    mm <- which(y_m == i)
    y_m [mm] <- dem_safe_m[mm]
    y_m

    options(warn=-1)
    aa <- c(1, 2)
    bb <- c("H", "V")
    row_col <- aa[bb == fill_dir_v[i]]
    e_zero <- apply(y_m, row_col, min, na.rm = TRUE)
    e_zero [e_zero == Inf] <- NA
    e_zero
    options(warn=0)

    rep_count <-  prod(dim(dem_m)) / length(e_zero)
    byrow_bool <- length(e_zero) == dim(dem_m)[2]
    diff <- dem_m - matrix(rep(e_zero, rep_count), nrow = nrow(dem_m), byrow = byrow_bool)

    dem_m[diff > stage[i]] <- NA
    dem_m [!is.na(dem_m)] <- 1
    dem_m [is.na(dem_m)] <- 0
    res_list[,,i] <- dem_m

  }
  res_list
}

# Stack flood rasters per river reach

#' Stack flood rasters per river reach.
#' @param rr An array with nrow = nrow(DEM), ncol = ncol(DEM) and number of layers equal to the number of river reaches. Each matrix element is a boolean raster than indicates the pixels that are flooded.
#' @param DEM A raster. It serves as template to copy the stacked matrix in.
#' @return A raster of projection and dimension equal to the ones of the argument DEM.
## y <- matrix(c(NA, 1 , 1 , NA, 2 , NA,
##               1 , 1 , NA, 2 , NA, NA,
##               NA, NA, NA, 2 , NA, NA,
##               NA, NA, NA, NA, 2 , NA,
##               NA, NA, NA, NA, NA, 2 ),
##             ncol = 6, byrow =  TRUE)
## y <- raster(y)
## DEM <- matrix(c(100,  100,  130,  140, 150, 160,
##                 140,  150,  110,  150, 140, 160,
##                 90 ,  120,  110,  150, 110, 120,
##                 100,  100,  130,  140, 150, 160,
##                 90 ,  100,  110,  150, 110,  80),
##               ncol = 6, byrow =  TRUE)
## DEM <- raster(DEM)
## stage <- c(15, 25)
## max_extent <- 2
## fill_dir_v <- c("V", "H")
## ll <- SPREAD_RIVER(y, max_extent, fill_dir_v)
## rr <- POUR_WATER(DEM, y, ll, fill_dir_v, stage)
## tt <- STACK_FLOOD(rr, DEM)
STACK_FLOOD <- function(rr, DEM) {
  ss <- apply(rr, c(1,2), sum)
  ss[ss != 0] <- 1
  ss [ss == 0] <- NA
  tt <- DEM
  tt[] <- ss
  tt
}

# Rasters related to toes of possible landslides ---------------------------------------------------------

#' Rasters related to toes of possible landslides
#'
#' @param r_r A raster of river reach ID.
#' @param weights_for_angle Weights corresponding to river angle as second argument of car::recode().
#' @return An object of class list of two rasters.The first raster provides the orientation of the landslide at the toe, going from 1 to 8. The landslide placed at location 1 is at 0 degree. The following ones are placed anti-clockwise. The second raster provides the weight of landslide at the toe provided the curve of the adjacent river.
TOES_LIST <- function (r_r, weights_for_angle) {

  # Prepare output rasters
  toe_1to8_r <- r_r
  toe_1to8_r[] <- NA
  toe_weight_angle_r <-toe_1to8_r
  # Initiate loop
  nc <- r_r@ncols
  index_nonNA_r_v <- which(!is.na(r_r[])) # find the indexes in the river raster which are non NA
  length_ <- length(index_nonNA_r_v)
  unnice <- data.frame(aa = c(1,2,3,4,5,6,7,8), bb = c(5, 6, 7, 8, 1, 2, 3, 4))
  # Prepare empty vectors
  angle_v <- rep(0, length_)
  toe_loc_v <- rep(NA,length_)
  toe_1to8_value_v <- rep(NA,length_)
  toe_angle_value_v <- rep(NA, length_)

  for (i in 1:length_) {

    index_center <- index_nonNA_r_v[i] # pixel where water is present
    index_focal <- index_center + c(-nc - 1, -nc, -nc +1, -1,0, +1, +nc-1, +nc, +nc+1) # indexes of the pixels that represent the focal
    index_focal_toe <- c(index_focal[6], index_focal[3], index_focal[2], index_focal[1], index_focal[4], index_focal[7], index_focal[8], index_focal[9]) # the 1st element of this vector represents East, the 3rd element represents north and so on.
    r_focal_m <- matrix(r_r[index_focal], nrow = 3, byrow=TRUE)

    if (length(which(!is.na(r_focal_m))) == 3) {
      v_v <- c(r_focal_m[2,3], r_focal_m[1, 3], r_focal_m[1,2], r_focal_m[1,1], r_focal_m[2,1], r_focal_m[3,1], r_focal_m[3,2], r_focal_m[3,3]  )
      a <- which(!is.na(v_v))[1]
      vreordered_v <- v_v[c(a:length(v_v), 1:(a-1))][1:length(v_v)]
      angle_1to8 <- abs  ( diff  (  which(!is.na(vreordered_v))  )  )
      toe_1to8 <- a + ceiling((angle_1to8/2))
      angle_degree <- 45 * angle_1to8

      if (angle_degree %in% c(0,180) ){
        toe_loc_v[i] <- NA
        toe_1to8_value_v[i] <- NA
      } else if (angle_degree >180){
        toe_loc_v[i] <- index_focal_toe[toe_1to8]
        toe_1to8_value_v[i] <- toe_1to8
        toe_angle_value_v[i] <- angle_degree
      } else if (angle_degree < 180){
        toe_1to8_updated <- unnice$bb[unnice$aa==toe_1to8]
        toe_loc_v[i] <- index_focal_toe[toe_1to8_updated]
        toe_1to8_value_v[i] <- toe_1to8_updated
        toe_angle_value_v[i] <- angle_degree
      }
    }
    print(paste("river cell ", i))
  }

  index_v <- !is.na(toe_loc_v) # index of vectors with no NA
  toe_loc_select_v <- toe_loc_v[index_v] # indexes of pixels of toes where there is an adjacent river curve
  toe_1to8_r[toe_loc_select_v]  <- toe_1to8_value_v[index_v] # Updates the value of the toes pixel from 1 to 8

  toe_weight_angle_r[toe_loc_select_v] <- toe_angle_value_v[index_v] # Update the value of the toe pixels first with orientation angle ...
  toe_weight_angle_r[] <- car::recode(toe_weight_angle_r[], weights_for_angle ) # ... and second with corresponding weight

  return(list(toe_1to8_r,toe_weight_angle_r))
}

# Build landslides from toe -------------------------------------------------------------------------------

#' Build landslides from toe.
#' @param count_pixel The count of pixels covered by a landslide.
#' @param ncol_raster The column number of the raster than contains the toe orientation derived in function TOES_LIST.
#' @description  Landslides at 0, 90, 180 and 270 degrees are assumed to have odd counts of rows and columns. Landslides at 45, 135, 225 and 305 degree are assumed to have even and same counts of rows and columns.
#' @return An object of class list with 8 elements. Each element correspond to the index of the vectorised raster of toes of possible landslides.
IND_LDSD <-function(count_pixel, ncol_raster) {

  # Prepare the count of rows and columns.
  sqrt_ <- round(sqrt(count_pixel))
  sqrt_v <- c(sqrt_, sqrt_+1)
  even_ <- sqrt_v[which(sqrt_v %% 2 == 0)]
  odd_small <- even_ -1
  odd_large <- even_ + 1

  # index_ldsd_1
  nrow <- odd_small
  ncol <- odd_large
  devi_nrow <- (nrow-1)/2
  start_row_v <- ( (-devi_nrow):+devi_nrow  ) * ncol_raster
  go_row_v <- 0:(ncol-1)

  index_ldsd_1 <- c()
  for (i in 1:nrow) {
    index_ldsd_1 <- c(index_ldsd_1, start_row_v[i] + go_row_v)
  }

  # index_ldsd_5
  nrow <- odd_small
  ncol <- odd_large
  devi_nrow <- (nrow-1)/2
  start_row_v <- ( (-devi_nrow):+devi_nrow  ) * ncol_raster
  go_row_v <- -(ncol-1):0

  index_ldsd_5 <- c()
  for (i in 1:nrow) {
    index_ldsd_5 <- c(index_ldsd_5, start_row_v[i] + go_row_v)
  }

  # index_ldsd_3
  nrow <- odd_large
  ncol <- odd_small
  start_row_v <- (-nrow +1):0  * ncol_raster
  devi_ncol <- (ncol-1)/2
  go_row_v <- (-devi_ncol):+devi_ncol

  index_ldsd_3 <- c()
  for (i in 1:nrow) {
    index_ldsd_3 <- c(index_ldsd_3, start_row_v[i] + go_row_v)
  }

  # index_ldsd_7
  nrow <- odd_large
  ncol <- odd_small
  start_row_v <- 0:(nrow -1)  * ncol_raster
  devi_ncol <- (ncol-1)/2
  go_row_v <- (-devi_ncol):+devi_ncol

  index_ldsd_7 <- c()
  for (i in 1:nrow) {
    index_ldsd_7 <- c(index_ldsd_7, start_row_v[i] + go_row_v)
  }

  # index_ldsd_2
  nrow <- even_
  ncol <- even_
  start_row_v <- ((-nrow+1):0  ) * ncol_raster
  go_row_v <- 0:(ncol-1)

  index_ldsd_2 <- c()
  for (i in 1:nrow) {
    index_ldsd_2 <- c(index_ldsd_2, start_row_v[i] + go_row_v)
  }

  # index_ldsd_4
  nrow <- even_
  ncol <- even_
  start_row_v <- ((-nrow+1):0  ) * ncol_raster
  go_row_v <- (-ncol+1):0

  index_ldsd_4 <- c()
  for (i in 1:nrow) {
    index_ldsd_4 <- c(index_ldsd_4, start_row_v[i] + go_row_v)
  }

  # index_ldsd_6
  nrow <- even_
  ncol <- even_
  start_row_v <- (0:(nrow-1)) * ncol_raster
  go_row_v <- (-ncol+1):0

  index_ldsd_6 <- c()
  for (i in 1:nrow) {
    index_ldsd_6 <- c(index_ldsd_6, start_row_v[i] + go_row_v)
  }

  # index_ldsd_8
  nrow <- even_
  ncol <- even_
  start_row_v <- (0:(nrow-1)) * ncol_raster
  go_row_v <- 0:(ncol-1)

  index_ldsd_8 <- c()
  for (i in 1:nrow) {
    index_ldsd_8 <- c(index_ldsd_8, start_row_v[i] + go_row_v)
  }

  return (list(index_ldsd_1, index_ldsd_2, index_ldsd_3, index_ldsd_4, index_ldsd_5, index_ldsd_6, index_ldsd_7, index_ldsd_8))
}

# Accuracy assessment of LULC raster ------------------------

#' Overal accuracy
#'
#' @param x A matrix.
#' @return A scalar of the sum of the diagonal elements divided by the sum of all elements.
OVERALL_ACCURACY <- function(x) {sum(diag(x)) / sum (x)}

#' accuracy assessment and confusion matrix
#'
#'  @param aa A matrix.
#'  @return A dataframe with value of confusion matrix, producer's accuracy, user's accuracy, and overall accuracy.
NICE_DF <- function (aa) {
  Commis_error <- diag(aa) / apply(aa, 2 ,sum)
  Omis_error <- c(diag(aa) / apply(aa, 1 ,sum), OVERALL_ACCURACY(aa))
  aa <- rbind(aa, 100* Commis_error)
  aa <- cbind(aa, 100* Omis_error)
  class_v <- c("Bare land", "Farmland", "Forest", "Settlement", "Snow", "Water")
  rownames(aa) <- c(class_v, "User's accuracy [%]")
  colnames(aa) <- c(class_v, "Producer's accuracy [%]")
  aa <- round(aa, 0)
  return(as.data.frame(aa))
}

#' accuracy assessment (no confusion matrix)
#'
#'  @param aa A matrix.
#'  @return A dataframe with value of producer's accuracy, user's accuracy, and overall accuracy.
ASSMT_DF <- function (aa) {
  Omis_error <- diag(aa) / apply(aa, 1 ,sum)
  Commis_error <- diag(aa) / apply(aa, 2 ,sum)
  aa_df <- data.frame(Omis_error, Commis_error)
  rownames(aa_df) <- c("Bare land", "Farmland", "Forest", "Settlement", "Snow", "Water")
  colnames(aa_df) <- c("Prod", "User")
  return(round(aa_df * 100, 2))
}

#Truncated Poisson

#'Truncated poisson
#'
#'@param n the number of Poisson-realisation of lambda = absolute_lambda that have un upper limit of max_val
#'@param absolute_lambda Lambda value of the Poisson distribution
#'@param max_val the maximum number of landslides
#'@description we use this function instead of rpois because, with rpois, we could have the possibility that P(X = 124) = P(X = 123) (thank you Vik Gopal)

RPOIS_TRUNC <- function(n, absolute_lambda, max_val) {
  out <- base::sample(0:max_val, size=n, replace=TRUE,
                      prob=dpois(0:max_val, lambda = absolute_lambda))
  out
}

# Valuation ----------------------------------------------------------------------------------------------

#' Valuation HPP.
#'
#' @param asset_HPP_r A HPP asset raster.
#' @param hazard_r A hazard raster.
#' @param PAE A scalar for the probability of affected exposure.
#' @param beta_param A vector of length 2 for the parameters of the beta distribution.
#' @param BI_HPP_r A HPP revenue raster.
#' @param max_BI_duration A scalar. Maximum duration of business disruption in year.
#' @param cap_BI_HPP A scalar between 0 and 1. The minimum MDD for an HPP to incur indirect damages.
#' @return A vector that contains "Expsre_PixelCount [count pixel]", "Expsre_TotalValue [mio NPR]", "DD_PixelCount [count pixel]","DD_TotalValue [mio NPR]", "ID_InYear [year]", "ID_TotalValue [mio NPR]", and "IDandDD_TotalValue [mio NPR]".
VALUATION_HPP_R <- function(asset_HPP_r, hazard_r, PAE, beta_param, BI_HPP_r, max_BI_duration , cap_BI_HPP) {
  # Exposure value
  index_v <- which(!is.na(asset_HPP_r[]) & !is.na(hazard_r[]))
  EXPSR_CT <- length(index_v)
  EXPSR_VL <- sum(asset_HPP_r[index_v])
  #Direct damages
  size <- max(1,round(length(index_v) * PAE,0))
  index_damaged_v <- sample(index_v, size=size)
  MDD <- rbeta(length(index_damaged_v),beta_param[1],beta_param[2])
  aa <- asset_HPP_r[index_damaged_v]
  DD_CT <- size
  DD_VL <- sum(aa * MDD)
  #Indirect damages
  ID_CT <- 0
  ID_VL <- 0
  if (any(MDD>cap_BI_HPP)) {
    ID_CT <- max_BI_duration
    ID_VL <- sum(  (MDD>cap_BI_HPP) * BI_HPP_r[index_damaged_v] )
  }
  return (c(EXPSR_CT, EXPSR_VL, DD_CT, DD_VL, ID_CT, ID_VL))
}

#' Valuation road.
#'
#' @param asset_road_r A road asset raster.
#' @param hazard_r A hazard raster.
#' @param PAE A scalar for the probability of affected exposure.
#' @param beta_param A vector of length 2 for the parameters of the beta distribution.
#' @param max_BI_duration A scalar. Maximum duration of customs disrpution in year.
#' @param BI_pa_mioNPR_road Loss of income for the population in the study area per year in million NPR with the closure of the customs.
#' @return A vector that contains "Expsre_PixelCount [count pixel]", "Expsre_TotalValue [mio NPR]", "DD_PixelCount [count pixel]","DD_TotalValue [mio NPR]", "ID_InYear [year]", "ID_TotalValue [mio NPR]", and "IDandDD_TotalValue [mio NPR]".
VALUATION_ROAD_R <- function(asset_road_r, hazard_r, PAE, beta_param, max_BI_duration, BI_pa_mioNPR_road, cap_BI_HPP) {
  #Exposure
  index_v <- which(!is.na(asset_road_r[]) & !is.na(hazard_r[]))
  EXPSR_CT <- length(index_v)
  EXPSR_VL <- sum(asset_road_r[index_v])
  # Direct damages
  size <- max(1,round(length(index_v) * PAE,0))
  index_damaged_v <- sample(index_v, size=size)
  MDD <- rbeta(length(index_damaged_v),beta_param[1],beta_param[2])
  DD_CT <- size
  DD_VL <- sum(asset_road_r[index_damaged_v] * MDD)
  # Indirect damages
  #DD_MDDis1 <- sum(asset_road_r[index_damaged_v])
  #ID_CT <- max_BI_duration * DD_VL / DD_MDDis1
  #ID_VL <- BI_pa_mioNPR_road * ID_CT
  ID_CT <- 0
  ID_VL <- 0
  return (c(EXPSR_CT, EXPSR_VL, DD_CT, DD_VL, ID_CT, ID_VL))
}

#' Valuation settlement.
#' @param asset_settlement_r A settlement asset raster.
#' @param hazard_r A hazard raster.
#' @param PAE A scalar for the probability of affected exposure.
#' @param beta_param A vector of length 2 for the parameters of the beta distribution.
#' @return A vector that contains "Expsre_PixelCount [count pixel]", "Expsre_TotalValue [mio NPR]", "DD_PixelCount [count pixel]","DD_TotalValue [mio NPR]", "ID_InYear [year]", "ID_TotalValue [mio NPR]", and "IDandDD_TotalValue [mio NPR]".
VALUATION_SETTLEMENT_R <- function(asset_settlement_r, hazard_r, PAE, beta_param) {
  #Exposure
  index_v <- which(!is.na(asset_settlement_r[]) & !is.na(hazard_r[]))
  EXPSR_CT <- length(index_v)
  EXPSR_VL <- sum(asset_settlement_r[index_v])
  # Direct damages
  size <- max(1,round(length(index_v) * PAE,0))
  index_damaged_v <- sample(index_v, size=size)
  MDD <- rbeta(length(index_damaged_v),beta_param[1],beta_param[2])
  DD_CT <- size
  DD_VL <- sum(asset_settlement_r[index_damaged_v] * MDD)
  # Indirect damages
  ID_CT <- 0
  ID_VL <- 0
  return (c(EXPSR_CT, EXPSR_VL, DD_CT, DD_VL, ID_CT, ID_VL))
}

#' Valuation farmland.
#' @param asset_farmland_r A farmland asset raster.
#' @param hazard_r A hazard raster.
#' @param PAE A scalar for the probability of affected exposure.
#' @param beta_param A vector of length 2 for the parameters of the beta distribution.
#' @return A vector that contains "Expsre_PixelCount [count pixel]", "Expsre_TotalValue [mio NPR]", "DD_PixelCount [count pixel]","DD_TotalValue [mio NPR]", "ID_InYear [year]", "ID_TotalValue [mio NPR]", and "IDandDD_TotalValue [mio NPR]".
VALUATION_FARMLAND_R <- function(asset_farmland_r, hazard_r, PAE, beta_param, max_BI_duration) {
  #Exposure
  index_v <- which(!is.na(asset_farmland_r[]) & !is.na(hazard_r[]))
  EXPSR_CT <- length(index_v)
  EXPSR_VL <- sum(asset_farmland_r[index_v])
  # Direct damages
  index_v <- which(!is.na(asset_farmland_r[]) & !is.na(hazard_r[]))
  size <- max(1,round(length(index_v) * PAE,0))
  index_damaged_v <- sample(index_v, size=size)
  MDD <- rbeta(length(index_damaged_v),beta_param[1],beta_param[2])
  DD_CT <- size
  DD_VL <- sum(asset_farmland_r[index_damaged_v] * MDD)
  # Indirect damages
  ID_CT <- 0
  ID_VL <- DD_VL * max_BI_duration
  return (c(EXPSR_CT, EXPSR_VL, DD_CT, DD_VL, ID_CT, ID_VL))
}

# Rounding for tables in the damage paper ------------------------------

#' Rounding tables (non-percentage)
#' @param x_df A dataframe with elements of class numeric
#' @return A dataframe with elements of class character that are rounded up to the decimal defined in the function
SIMPLE_ROUND <- function(x_df){
  x_df <- round(x_df, 1)
  x_df[x_df < 0.1] <- "<0.1"
  return(x_df)
}

#' Rounding tables (percentage)
#' @param x_df A dataframe with elements of class numeric
#' @param dim_to_1 Value is 2 if the elements of the last column of the dataframe sum to 100 percent. Value is 1 if the elements of the last row of the dataframe sum to 100 percent.
#' @return A dataframe with elements of class character that are rounded up to the decimal defined in the function

SIMPLE_ROUND_PC <- function(x_df, dim_to_1){
  if (dim_to_1 == 1) {
    x_df <- t(x_df)
    x_df <- (round(x_df / x_df[, dim(x_df)[2]] * 100, 0))
    x_df <- t(x_df)
  } else if (dim_to_1 == 2) {
    x_df <- (round(x_df / x_df[, dim(x_df)[2]] * 100, 0))
  }else {
    stop("diM_to_1 can only take value {1, 2}")
  }
  x_df[x_df < 1] <- "<1"
  apply(x_df, 2, function(x_df) (paste(x_df, "%", sep = "")))
}


# Multiple plot function ----------------------------------

#' Multiple plot function
#'
#' @param  ... ggplot objects
#' @param plotlist ggplot objects as a list
#' @param cols number of colums the frame is split in
#' @param layout A matrix specifying the layout of the frame. If present, 'cols' is ignored.
#' @description The other of this function is winston@stdout.org. The function was retrieved online in Nov 2017.
#' @return A frame with the ggplot objects
MULTIPLOT <- function(..., plotlist=NULL, cols=1, layout=NULL) {

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


# Create matrix of coordinates ----------------------------------

#' Create matrix of coordinates of a rectangle
#'
#' @param v a vector of 4 elements containing first latitude and then longitude coordinates ranked as c("y_N_min", "y_N_max", "x_E_min", "x_E_max"))
#' @return A 4X2 matrix containing the coordinates of the edge of a rectangle. The first row corresponds to the Westsouthern edge, while the following rows progress anti-clockwise from there. Longitudes are in the fist column and latitudes in the second column.

XYM = function(v) {
  v <- as.numeric(v)
  xym <- matrix(c(v[3],v[1],   v[4],v[1],  v[4],v[2],   v[3],v[2]), ncol = 2, byrow= T)
  return(xym)
}
