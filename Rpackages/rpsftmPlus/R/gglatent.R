

#' Plots latent survival from rpsftm object
#'
#' This function returns a ggplot derived from an rpsftm object showing latent survival time for experimental and control arms. If converged then the two latent survival curves should overlap.
#'
#' @param x rpsftm object (rpsftm)
#' @param incidence TRUE or FALSE (should survival or 1-survival be plotted)
#' @keywords survival, rpsftm
#' @export
#' @examples
#' # use data included in rpsftm package
#' immdef <- rpsftm::immdef
#' immdef$rx <- with(immdef, 1 - xoyrs/progyrs)
#'
#' # fit the model
#' imm.fit <- rpsftm(Surv(progyrs, prog) ~ rand(imm, rx),
#'                   data=immdef,
#'                   censor_time=censyrs)
#'
#' gglatent(imm.fit)
#' @import survival
#' @import rpsftm
#' @import ggplot2
#' @import dplyr


gglatent <- function(x, incidence = FALSE){
  if (!("rpsftm" %in% class(x))){
    stop("Please provide an rpsftm object.")
  }

  latent.df <- as.data.frame(x$fit) %>%
    mutate(type = "Latent")

  plot.df <- latent.df %>%
    mutate(Arm = ifelse(.arm == 0, "Control", "Experimental"))

  if (incidence){
    plot.df <- mutate(plot.df, s = 1-s)
  }

  # make a plot
  rc <- ggplot(plot.df, aes(x=t, y = s, color = Arm, linetype = Arm)) +
    theme_bw() +
    geom_step(direction = "hv") +
    ylab("Survival") +
    scale_linetype_discrete(name = "Arm") +
    theme(legend.position="bottom") +
    guides(col = guide_legend(ncol = 1)) +
    scale_color_manual(values = c("Black", "Red"), name = "Arm") +
    guides(linetype = guide_legend(ncol = 1)) +
    xlab("Latent Time")

  if (incidence){
    rc <- rc + ylab("Incidence")
  }

  if (nrow(filter(plot.df,c == 1))>0){
    rc <- rc + geom_point(aes(x= t, y= s,  shape = "Censored"), data = filter(plot.df,c == 1)) +
      scale_shape(name = " ", solid = FALSE)
  }

  return(rc)

}

