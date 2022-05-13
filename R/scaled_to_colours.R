#' Convert image output from image_to_scaled() to cross-stitches
#'
#' Match raw colour channel values to a smaller subset of colours.
#'
#' @param image_list List output from image_to_scaled(). Contains an element  \code{Img_scaled}.
#' @param method Default 'cie94'. The method to use for colour comparison. Either 'euclidean', 'cie1976', 'cie94', 'cie2000', or 'cmc'.
#' See \code{farver::compare_colour}.
#' @param colour_table Defaults to \code{dmc_colours}. Data frame of  colours to map onto image. Must contain name and red, green, blue channels.
#' See attached data  \code{dmc_colours} as examples. Original source for \code{dmc_colours} is: https://github.com/sharlagelfand/dmc
#' @param colour_palette Used to supply a character vector of specific DMC colours to use. Can be particularly useful in instances where you already have
#' many colours available and maintain a list of their DMC codes. Defaults to NULL - in which case all colours are to be used.
#' Ignored if a \code{colour_table} is supplied.
#' @param trans_bg If \code{img} is a png has a transparent background, name of colour to replace the background.
#' @param dithering Improves colour of large, photo-realistic mosaics.
#' @param n_colours Optional: Integer representing the maximum number of colours to use. This option selects the \code{n_colours} that appear the most, and remaps all other colours to match these \code{n_colours}. Otherwise, defaults to NULL.
#' @format NULL
#' @usage NULL
#' @return A list with element \code{Img_lego} containing a data frame of the x- & y-coordinates, R, G, B channels, and mapped colour of each brick (pixel).
#' @export
scaled_to_colours <- function(image_list, method = "cie94",
                             colour_table = NULL,
                             colour_palette = NULL,
                             trans_bg = "B5200",
                             dithering = FALSE,
                             n_colours = NULL){
  in_list <- image_list

  # DMC thread colours to use ----
  if(is.null(colour_table)) {
    dmc_table <- floss
  } else {
    dmc_table <- colour_table
  }

  #Set up colour palette... used standard way or with Dithering
  if(!is.null(colour_palette)){
    dmc_table <- dmc_table %>%
      dplyr::filter(tolower(dmc) %in% tolower(colour_palette))
  }

  #Standard or dithering
  if(!dithering){
    img <- convert_colour_to_dmc_standard(in_list$Img_scaled, dmc_table, method)
  } else {
    img <- convert_colour_to_brick_dithering(in_list$Img_scaled, dmc_table, method)
  }

  #If option to reduce number of colours is selected, reduce the number of colours to n_colours
  if(!is.null(n_colours) & is.numeric(n_colours)) {
    # find top n_colours
    top_colours <- img %>%
      dplyr::group_by(dmc) %>%
      dplyr::tally() %>%
      dplyr::slice_max(n, n = n_colours) %>%
      dplyr::pull(dmc)

    # add background colour if not in top_colours
    if(!trans_bg %in% top_colours) {
      top_colours <- c(top_colours, trans_bg)
    }

    # filter dmc table
    filtered_dmc_table <- dmc_table %>%
      dplyr::filter(dmc %in% top_colours)

    # repeat colour conversion but only with filtered_dmc_table
    if(!dithering){
      img <- convert_colour_to_dmc_standard(in_list$Img_scaled, filtered_dmc_table, method)
    } else {
      img <- convert_colour_to_brick_dithering(in_list$Img_scaled, filtered_dmc_table, method)
    }


  }

  #Replace the transparent background with a DMC colour
  if(!(trans_bg %in% embroidr::floss$dmc)){
    stop("trans_bg must be a DMC colour.")
  }

  bg_colour <- embroidr::floss %>%
    dplyr::filter(dmc == trans_bg)

  img <- img %>%
    dplyr::mutate(colour = ifelse(bg_transparent, bg_colour$hex[1], colour),
                  dmc = ifelse(bg_transparent, bg_colour$dmc[1], dmc),
                  name = ifelse(bg_transparent, bg_colour$name[1], name),
                  hex = ifelse(bg_transparent, bg_colour$hex[1], hex)) %>%
    dplyr::select(-bg_transparent)


  #Return output....
  in_list[["Img_dmc"]] <- img

  in_list[["object"]] <- "mosaic"

  return(in_list)

}

convert_colour_to_dmc_standard <- function(img_object, colour_table, method){

    #Speed up calc by round pixel to nearest 1/20 & only calculating unique
    mosaic_base <- img_object %>%
      dplyr::mutate_at(dplyr::vars(R, G, B), list(~round(.*20)/20)) %>%
      dplyr::select(R, G, B) %>%
      dplyr::distinct()

    #Match colours with Farver ----

      mosaic_colours <- mosaic_base %>%
        dplyr::mutate(rgb = purrr::pmap(list(R, G, B), function(R, G, B){
          cc <- matrix(c(R, G, B), ncol = 3)*255
          dstncs <- farver::compare_colour(from=cc,
                                           to=colour_table[, c('red', 'green', 'blue')],
                                           from_space='rgb', to_space = 'rgb', method = method)

          sel_colour <- as.character(colour_table[which.min(dstncs), "name"])[1]

          colour_table %>%
            dplyr::filter(name == sel_colour) %>%
            dplyr::select(dmc, name, hex)
        })) %>%
        tidyr::unnest(rgb)


    img <- img_object %>%
      dplyr::mutate_at(dplyr::vars(R, G, B), list(~round(.*20)/20)) %>%
      dplyr::left_join(mosaic_colours, by = c("R", "G", "B"))

    return(img)

}

convert_colour_to_brick_dithering <- function(img_object, colour_table, method){


    #Speed up calc by round pixel to nearest 1/20 & only calculating unique
    mosaic_base <- img_object

    mosaic_base$dmc <- NA
    mosaic_base$name  <- NA
    mosaic_base$hex <- NA


    for(yy in unique(mosaic_base$y)){
      for(xx in unique(mosaic_base$x)){
        dstncs <- mosaic_base[mosaic_base$x == xx & mosaic_base$y == yy, c("R", "G", "B")] %>%
          as.matrix(ncol = 3) %>%
          farver::compare_colour(to=colour_table[, c('red', 'green', 'blue')]/255,
                                 from_space='rgb', to_space = 'rgb', method=method)

        #Assign DMC colour for this cell
        if(!is.character(colour_table$hex[which.min(dstncs)])){next}

        mosaic_base[mosaic_base$x == xx & mosaic_base$y == yy, c("dmc", "name", "hex")] <-
          as.list(as.character(colour_table[which.min(dstncs),  c("dmc", "name", "hex")][1,]))

        #Difference in colour
        dith_diff <- mosaic_base[mosaic_base$x == xx & mosaic_base$y == yy,  c("R", "G", "B")] -
          as.numeric(colour_table[which.min(dstncs),  c("red", "green", "blue")][1,])/255

        #Update colour of surrounding pixels.. if pixel exists
        if(xx < max(mosaic_base$x)){
          xs <- 1; ys <- 0;
          mosaic_base[(mosaic_base$x == (xx+xs)) & (mosaic_base$y == (yy+ys)),  c("R", "G", "B")] <-
            mosaic_base[(mosaic_base$x == (xx+xs)) & (mosaic_base$y == (yy+ys)),  c("R", "G", "B")] + (7/16)*dith_diff
        }

        if(yy > min(mosaic_base$y)){
          xs <- 0; ys <- -1;
          mosaic_base[(mosaic_base$x == (xx+xs)) & (mosaic_base$y == (yy+ys)),  c("R", "G", "B")] <-
            mosaic_base[(mosaic_base$x == (xx+xs)) & (mosaic_base$y == (yy+ys)),  c("R", "G", "B")] + (5/16)*dith_diff
        }

        if(xx > min(mosaic_base$x) & yy > min(mosaic_base$y)){
          xs <- -1; ys <- -1;
          mosaic_base[(mosaic_base$x == (xx+xs)) & (mosaic_base$y == (yy+ys)),  c("R", "G", "B")] <-
            mosaic_base[(mosaic_base$x == (xx+xs)) & (mosaic_base$y == (yy+ys)),  c("R", "G", "B")] + (3/16)*dith_diff
        }

        if(xx < max(mosaic_base$x) & yy > min(mosaic_base$y)){
          xs <- 1; ys <- -1;
          mosaic_base[(mosaic_base$x == (xx+xs)) & (mosaic_base$y == (yy+ys)),  c("R", "G", "B")] <-
            mosaic_base[(mosaic_base$x == (xx+xs)) & (mosaic_base$y == (yy+ys)),  c("R", "G", "B")] + (1/16)*dith_diff
        }

        #Ensure RGBs [0,1]
        mosaic_base[mosaic_base$R > 1, "R"] <- 1
        mosaic_base[mosaic_base$G > 1, "G"] <- 1
        mosaic_base[mosaic_base$B > 1, "B"] <- 1

        mosaic_base[mosaic_base$R < 0, "R"] <- 0
        mosaic_base[mosaic_base$G < 0, "G"] <- 0
        mosaic_base[mosaic_base$B < 0, "B"] <- 0
      }
    }

    return(mosaic_base)

}
