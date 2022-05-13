#' Transform an image raster array into a cross-stitch pattern
#'
#' @param image A raster array from an image.
#' @param size_unit The unit by which the image will be scaled. Can be one of the following options: 'stitches', 'inches', or 'cm'
#' @param img_size Size of the embroidery, in the unit determined by size_unit. Use a single value (e.g. \code{48}) for a square image.
#' Use a vector of two values for a rectangular image \code{c(width, height)}. NOTE: This parameter sets the size of the embroidery once
#' it will is embroidered. The embroidery pattern that will be outputted will be larger.
#' @param brightness A value >1 will increase the brightness of the image while a positive value <1 will decrease the brightness.
#' @param warhol Array of values \code{c(1, 2, 3)} associated with R, G, B colour channels. Swap values in array to swap colour channels for a fun visual effect.
#' @param method Default 'cie94'. The method to use for colour comparison. Either 'euclidean', 'cie1976', 'cie94', 'cie2000', or 'cmc'.
#' See \code{farver::compare_colour}.
#' @param colour_table Defaults to \code{dmc_colours}. Data frame of  colours to map onto image. Must contain name and red, green, blue channels.
#' See attached data  \code{dmc_colours} as examples. Original source for \code{dmc_colours} is: https://github.com/sharlagelfand/dmc
#' @param colour_palette Used to supply a character vector of specific DMC colours to use. Can be particularly useful in instances where you already have
#' many colours available and maintain a list of their DMC codes. Defaults to NULL - in which case all DMC colours are to be considered.
#' Ignored if a \code{colour_table} is supplied.
#' @param n_colours Optional: Integer representing the maximum number of colours to use. This option selects the \code{n_colours} that appear the most, and remaps all other colours to match these \code{n_colours}. Otherwise, defaults to NULL.
#' @param trans_bg If \code{img} is a png and has a transparent background, name of colour to replace the background.
#' @param dithering Improves colour of large, photo-realistic mosaics.
#' @param style Options for visualisation. Defaults to a tiled pattern with DMC numbers
#' @param colour_list Whether a list of colours should also be output. Defaults to \code{TRUE}.
#' @param file_name Option to name the output files. Defaults to \code{"embroidr_pattern"}
#' @format NULL
#' @usage NULL
#' @return An SVG file with the cross-stitch pattern. If \code{colour_list} is set to \code{TRUE},
#' a second SVG file containing a list of DMC colours used is also output.
#' @export
create_pattern_from_image <- function(image, size_unit = "stitches", img_size = 48,
                                      cloth_count = 16, brightness = 1, warhol = 1:3,
                                      method = "cie94", colour_table = NULL,
                                      colour_palette = NULL, n_colours = NULL,
                                      trans_bg = "B5200", dithering = FALSE,
                                      style = c("numbers", "crosses"),
                                      colour_list = TRUE, file_name = "embroidr_pattern") {

  image %>%
    embroidr::image_to_scaled(size_unit = size_unit, img_size = img_size,
                              cloth_count = cloth_count, brightness = brightness,
                              warhol = warhol) %>%
    embroidr::scaled_to_colours(method = method,
                                colour_table = colour_table,
                                colour_palette = colour_palette,
                                trans_bg = trans_bg,
                                dithering = dithering,
                                n_colours = n_colours) %>%
    embroidr::colours_to_pattern(style = style,
                                 colour_list = colour_list,
                                 file_name = file_name)

}
