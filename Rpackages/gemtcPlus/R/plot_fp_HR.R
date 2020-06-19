#' Produce ggplot from HR values in data.frame (medians vs time for several trts, all in one plot)
#'
#' @param HRs 
#'
#' @return fig
#' @export
#' 
plot_fp_HR <- function(HRs, xlab = "Month", legend.pos = "right", breaks = c(0.125, 0.25, 0.5, 1, 2, 4, 8), facet = FALSE, ncol = NULL){
  
  `%>%` <- magrittr::`%>%`
   
  if (facet){
    legend.pos <- "none"
  }
  
  fig <- ggplot2::ggplot(data = HRs) +
    ggplot2::geom_hline(aes(yintercept = 1)) +
    ggplot2::geom_line(aes(x, median, col = lab_s, linetype = lab_s)) +
    ggplot2::scale_y_log10(limits = range(breaks), breaks = breaks, labels = breaks) +
    ggplot2::labs(x = xlab, y = "HR (posterior median) [log scale]") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.title=element_blank(), legend.position = legend.pos)
  
  if (facet) {
    fig <- fig + ggplot2::facet_wrap(~lab_s, ncol = ncol)
  }
  
  return(fig)
}

