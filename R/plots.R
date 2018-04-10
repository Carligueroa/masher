#' A donut plot
#'
#' @param .data a `table_monovar`
#' @param .annotate Should the name of the var be displayed at the center of the donut?
plot_donut <- function(.data, .annotate = FALSE) {

  if (.annotate) {
    w <- 0.5
    base <- 1.8
  } else {
    w <- 0.3
    base <- 1.5
  }

  plot <- ggplot2::ggplot(.data, ggplot2::aes(x = "", y = n, fill = .data[, 1]))+
    ggplot2::geom_col(width = w)+
    ggplot2::geom_text(ggplot2::aes(label = percent),
                       position = ggplot2::position_stack(vjust = .5))+
    ggplot2::annotate("text",
                      x = base, y = sum(.data$n)*0.6,
                      label = paste("Base: ", sum(.data$n)),
                      color = "grey50")+
    ggplot2::scale_y_discrete(expand = c(0, 0))+
    ggplot2::coord_polar(theta = "y")

  if (.annotate) {
    plot <- plot +
      ggplot2::annotate(
        "text", x = 0, y = 0,
        label = names(.data)[1],
        color = "dark green",
        fontface = "bold",
        size = 9
      )

    return(plot)
  } else {
    return(plot)
  }
}

#' Make the plot background transparent
#'
transparentize <- function() {
  ggplot2::theme(
    panel.background      = ggplot2::element_rect(fill = "transparent", color = NA),
    plot.background       = ggplot2::element_rect(fill = "transparent", color = NA),
    legend.key            = ggplot2::element_rect(fill = "transparent", color = NA),
    legend.background     = ggplot2::element_rect(fill = "transparent", color = NA),
    legend.box.background = ggplot2::element_rect(fill = "transparent", color = NA),
    strip.background      = ggplot2::element_rect(fill = "transparent", color = NA)
  )
}

#' A theme for the pie charts
#'
theme_pie <- function() {

  ggplot2::theme_bw()+
    ggplot2::theme(
      axis.text         = ggplot2::element_blank(),
      axis.ticks        = ggplot2::element_blank(),
      axis.title        = ggplot2::element_blank(),
      axis.line         = ggplot2::element_blank(),
      legend.title      = ggplot2::element_blank(),
      legend.position   = c(0.5, 0.1),
      legend.direction  = "horizontal",
      panel.border      = ggplot2::element_blank(),
      panel.grid        = ggplot2::element_blank()
  )
}
