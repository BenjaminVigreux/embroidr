#' Visualise the output from scaled_to_colours() into a cross-stitch pattern
#'#'
#' @param mosaic_object List output from scaled_to_colours(). Contains element  \code{Img_dmc}.
#' @param style Options for visualisation. Defaults to a cross-stitch pattern with DMC numbers
#' @param colour_list Whether a list of colours should also be output. Defaults to \code{TRUE}.
#' @param file_name Option to name the outputs. Defaults to \code{"embroidr_pattern"}
#' @format NULL
#' @usage NULL
#' @return An SVG files with the cross-stitch pattern. If \code{colour_list} is set to \code{TRUE},
#' a second SVG file containing a list of DMC colours used is also output.

#' @export
colours_to_pattern <- function(mosaic_object, style = c("numbers", "crosses"),
                               colour_list = TRUE, file_name = "embroidr_pattern") {

  # work out min and max x and y
  min_x <- min(mosaic_object$Img_dmc$x)
  max_x <- max(mosaic_object$Img_dmc$x)

  min_y <- min(mosaic_object$Img_dmc$y)
  max_y <- max(mosaic_object$Img_dmc$y)

  # calculate plot size in inches
  plot_width <- mosaic_object$dims[1]
  plot_height <- mosaic_object$dims[2]

  suppressWarnings(

    # visualisation using crosses
    if(style == "crosses") {
      mosaic_object$Img_dmc %>%
        ggplot2::ggplot(mapping = ggplot2::aes(x = x, y = y, colour = hex)) +
        ggplot2::geom_point(shape = 4, size =5, stroke = 5) +
        ggplot2::scale_colour_identity() +
        ggplot2::scale_x_continuous(breaks = seq(min_x - 0.5, max_x + 0.5, 10),
                                    minor_breaks = (min_x - 0.5):(max_x + 0.5),
                                    expand = c(0,0), limits = c((min_x - 0.5),(max_x + 0.5))) +
        ggplot2::scale_y_continuous(breaks = seq(min_y - 0.5, max_y + 0.5, 10),
                                    minor_breaks = (min_y - 0.5):(max_y + 0.5),
                                    expand = c(0,0), limits = c((min_y - 0.5),(max_y + 0.5))) +
        embroidr_theme()


    } else if (style == "numbers") {
      mosaic_object$Img_dmc %>%
        ggplot2::ggplot(mapping = ggplot2::aes(x = x, y = y, fill = hex, label = dmc)) +
        ggplot2::geom_tile() +
        ggplot2::geom_text(size = 3) +
        ggplot2::scale_fill_identity() +
        ggplot2::scale_x_continuous(breaks = seq(min_x - 0.5, max_x + 0.5, 10),
                                    minor_breaks = (min_x - 0.5):(max_x + 0.5),
                                    expand = c(0,0), limits = c((min_x - 0.5),(max_x + 0.5))) +
        ggplot2::scale_y_continuous(breaks = seq(min_y - 0.5, max_y + 0.5, 10),
                                    minor_breaks = (min_y - 0.5):(max_y + 0.5),
                                    expand = c(0,0), limits = c((min_y - 0.5),(max_y + 0.5))) +
        embroidr_theme()
    } else {
      "ERROR: Choose a valid style"
    }

  )

  # output SVG, where 1 cross is approx. 1 cm^2

  ggplot2::ggsave(paste0(file_name,".svg"), width = plot_width, height = plot_height,
                  units = "cm")

  if(colour_list) {
    # Create and output colour list

    colour_list <- mosaic_object$Img_dmc %>%
      dplyr::group_by(dmc, name, hex) %>%
      dplyr::summarise(n_stitches = length(name)) %>%
      dplyr::arrange(desc(n_stitches))

    ggtab <- colour_list %>%
      dplyr::select(-hex) %>%
      ggpubr::ggtexttable(rows = NULL,
                          theme = ggpubr::ttheme("classic"))

    for(i in 1:nrow(colour_list)){
      row <- i+1
      column <- which(colnames(colour_list) == "dmc")
      ggtab <- ggpubr::table_cell_bg(
        ggtab, row = row, column = column,
        fill = colour_list$hex[i])
    }

    ggplot2::ggsave(paste0(file_name,"_colour_list.svg"), height = nrow(colour_list)*1,
                    units = "cm")

  }

}

embroidr_theme <- function() {
  ggplot2::theme(
    axis.title = ggplot2::element_blank(),
    axis.text = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_line(colour = "black"),
    panel.grid.minor = ggplot2::element_line(colour = "gray40"),
    panel.background = ggplot2::element_rect(fill = NA),
    panel.ontop = TRUE
  )
}
