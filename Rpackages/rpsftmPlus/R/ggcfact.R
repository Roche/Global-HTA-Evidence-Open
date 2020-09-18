
#' Plots observed and counterfactual survival from rpsftm object
#'
#' This function returns a ggplot derived from an rpsftm object showing 3 survival curves. Observed survival for experimental and control arm. Latent survival time for control arm.
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
#' imm.fit <- rpsftm::rpsftm(Surv(progyrs, prog) ~ rand(imm, rx),
#'                   data=immdef,
#'                   censor_time=censyrs)
#'
#' ggcfact(imm.fit)
#' @import survival
#' @import rpsftm
#' @import ggplot2
#' @import dplyr

ggcfact <- function(x, incidence = FALSE){
  if (!("rpsftm" %in% class(x))){
    stop("Please provide an rpsftm object.")
  }

  latent.df <- as.data.frame(x$fit) %>%
    mutate(Type = "Latent") %>%
    filter(.arm == 0)

  df <- rpsftm.data(x)

  observed.df <- survfit(Surv(time,cens)~.arm, data = df) %>%
    as.data.frame() %>%
    mutate(Type = "Observed")

  plot.df <- rbind(latent.df, observed.df) %>%
    mutate(Arm = ifelse(.arm == 0, "Control", "Experimental"))

  if (incidence){
    plot.df <- mutate(plot.df, s = 1-s)
  }

  # make the plot
  rc <- ggplot(plot.df, aes(x=t, y = s, color = Arm, linetype = Type)) +
    theme_bw() +
    geom_step(direction = "hv") +
    ylab("Survival") +
    scale_linetype_manual(name = "Type", values = c(2,1)) +
    theme(legend.position="bottom") +
    guides(col = guide_legend(ncol = 1)) +
    scale_color_manual(values = c("Black", "Red"), name = "Arm") +
    guides(linetype = guide_legend(ncol = 1)) +
    xlab("Time")

  if (incidence){
    rc <- rc + ylab("Incidence")
  }

  if (nrow(filter(plot.df,c == 1))>0){
    rc <- rc + geom_point(aes(x= t, y= s,  shape = "Censored"), data = filter(plot.df,c == 1)) +
      scale_shape(name = " ", solid = FALSE)
  }

  return(rc)

}
