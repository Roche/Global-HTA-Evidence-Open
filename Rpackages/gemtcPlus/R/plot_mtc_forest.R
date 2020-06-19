#' Utility function to do forest plot from data.frame with effect estimates.
#'
#' @param x        A data.frame with columns: Comparator, Med, CIlo, CIup.
#' @param sort.by  Character string defining how estimates are sorted, either "name" (the default) or "effect".
#' @param lab      Label to plot underneath x-axis.
#' @param do.log   Logical, whether log transformed x-axis or not (default = TRUE).
#' @param breaks   Where to put x-axis breaks and labels.
#'
#' @return         A ggplot2 plot object which
#' @importFrom graphics plot
#' @export
#'

plot_mtc_forest <- function(x, sort.by = c("name", "effect")[1],
                            lab = NULL,
                            do.log = TRUE,
                            breaks = c(0.25, 0.5, 0.8, 1, 1.25, 2, 4)){

  if (sort.by == "name"){
    ord <- rev(sort(as.character(x$Comparator)))
    x$Comparator <- factor(x$Comparator, ordered = TRUE, levels = ord)
  }
  if (sort.by == "effect"){
    mord <- order(x$Med)
    ord <- rev(as.character(x$Comparator[mord]))
    x$Comparator <- factor(x$Comparator, ordered = TRUE, levels = ord)
  }


  if (do.log){
    yint <- 1
  } else{
    yint <- 0
  }

  p <- ggplot2::ggplot(data = data.frame(x)) +
       ggplot2::geom_hline(yintercept = yint, col = "darkgrey") +
       ggplot2::geom_pointrange(ggplot2::aes_string(x = "Comparator",
                                                    y = "Med",
                                                    ymin = "CIlo",
                                                    ymax = "CIup")) +
       ggplot2::coord_flip() +
       ggplot2::theme_bw()

  if(do.log) {
    p <- p + ggplot2::scale_y_log10(breaks = breaks, limits = range(breaks), labels = breaks)
  }

  if (!is.null(lab)){
    p <- p + ggplot2::ylab(lab)
  }

  p
}
