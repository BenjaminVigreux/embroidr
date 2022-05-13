#' Scale an image raster array
#'
#' Scale the size of an image, in pixel. Process into a data frame.
#'
#' @param image A raster array from an image.
#' @param size_unit The unit by which the image will be scaled. Can be one of the following options: 'stitches', 'inches', or 'cm'
#' @param img_size Size of output image, in the unit determined by size_unit. Use a single value (e.g. \code{48}) for a square image.
#' Use a vector of two values for a rectangular image \code{c(width, height)}.
#' @param brightness A value >1 will increase the brightness of the image while a positive value <1 will decrease the brightness.
#' @param warhol Array of values \code{c(1, 2, 3)} associated with R, G, B colour channels. Swap values in array to swap colour channels for a fun visual effect.
#' @format NULL
#' @usage NULL
#' @return A list with element \code{Img_scaled} containing a data frame of the x- & y-coordinates, R, G, B channels, and hex colour of each stitch (pixel).
#' @export
image_to_scaled <- function(image, size_unit = "stitches", img_size = 48,
                            cloth_count = 16, brightness = 1, warhol = 1:3){

  #Adjust brightness. Max channel value is 1
  if(brightness < 0 ){stop("Brightness should be a positive value. Use 1 for no change, >1 for lighter, <1 for darker.")}
  image_b <- image*brightness
  image_b[image_b>1] <- 1

  if(is.null(size_unit)) {stop("Please use a valid size_unit. Options are 'stiches', 'inches', or 'cm'")}
  if(!size_unit %in% c('stitches', 'inches', 'cm')) {stop("please use a valid size_unit. Options are 'stiches', 'inches', or 'cm'")}

  if(is.null(cloth_count)) {stop("Please use a valid cloth_count. Typical sizes are: 11, 12, 14, 16, 18, 22, or 28")}
  if(!is.numeric(cloth_count)) {stop("cloth_count should be a numeric value")}

  # use option to set size using inches or cm, instead of default stitches
  if(size_unit == "inches") {
    # convert inches to stitches
    img_size <- img_size * cloth_count

  } else if(size_unit == "cm") {
    # convert cm to inches
    img_size <- img_size / 2.54

    # convert inches to stitches
    img_size <- img_size * cloth_count

  }

  #Only whole values for image size
  img_size <- round(img_size, 0)

  #RGB channel order as specified with the `warhol` input
  col_chan <- order(warhol[1:3])

  #Convert image to a data frame with RGB values
  img <- dplyr::bind_rows(
    list(
      (as.data.frame(image_b[, , col_chan[1]]) %>%
         dplyr::mutate(y=dplyr::row_number(), channel = "R")),
      (as.data.frame(image_b[, , col_chan[2]]) %>%
         dplyr::mutate(y=dplyr::row_number(), channel = "G")),
      (as.data.frame(image_b[, , col_chan[3]]) %>%
         dplyr::mutate(y=dplyr::row_number(), channel = "B"))
    )
  ) %>%
    tidyr::gather(x, value, -y, -channel) %>%
    dplyr::mutate(x = as.numeric(gsub("V", "", x))) %>%
    tidyr::spread(channel, value)

  # If png, drop the transparent bricks
  if(dim(image_b)[3] == 4){
    transparent <- as.data.frame(image_b[, , 4]) %>%
      dplyr::mutate(y=dplyr::row_number(), channel = "bg_transparent") %>%
      tidyr::gather(x, value, -y, -channel) %>%
      dplyr::mutate(x = as.numeric(gsub("V", "", x))) %>%
      tidyr::spread(channel, value) %>%
      dplyr::filter(bg_transparent < 1) %>%
      dplyr::mutate(bg_transparent = TRUE)

    img <- img %>%
      dplyr::left_join(transparent, by = c("y", "x")) %>%
      tidyr::replace_na(list(bg_transparent = FALSE))
  } else {
    img <- img %>%
      dplyr::mutate(bg_transparent = FALSE)
  }

  #Wide or tall image? Shortest side should be `img_size` pixels
  if(max(img$x) > max(img$y)){
    img_scale_x <-  max(img$x) / max(img$y)
    img_scale_y <- 1
  } else {
    img_scale_x <- 1
    img_scale_y <-  max(img$y) / max(img$x)
  }

  #If only 1 img_size value, create a square image
  if(length(img_size) == 1){
    img_size2 <- c(img_size, img_size)
  } else {
    img_size2 <- img_size[1:2]
    img_scale_x <- 1
    img_scale_y <- 1
  }

  #Rescale the image
  img2 <- img %>%
    dplyr::mutate(y_scaled = (y - min(y))/(max(y)-min(y))*img_size2[2]*img_scale_y + 1,
                  x_scaled = (x - min(x))/(max(x)-min(x))*img_size2[1]*img_scale_x + 1) %>%
    dplyr::select(-x, -y) %>%
    dplyr::group_by(y = ceiling(y_scaled), x = ceiling(x_scaled)) %>%
    #Get average R, G, B and convert it to hexcolour
    dplyr::summarize_at(dplyr::vars(R, G, B, bg_transparent), mean) %>%
    dplyr::mutate(bg_transparent = as.logical(round(bg_transparent))) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(colour = rgb(R, G, B)) %>%
    dplyr::ungroup() %>%
    #Center the image
    dplyr::filter(x <= stats::median(x) + img_size2[1]/2, x > stats::median(x) - img_size2[1]/2,
                  y <= stats::median(y) + img_size2[2]/2, y > stats::median(y) - img_size2[2]/2) %>%
    #Flip y
    dplyr::mutate(y = (max(y) - y) + 1)

  out_list <- list()
  out_list[["Img_scaled"]] <- img2
  out_list[["dims"]] <- img_size2
  out_list[["unit"]] <- size_unit
  out_list[["cloth_count"]] <- cloth_count

  return(out_list)

}
