
#' Plots hypothetical results from an rpsftm object
#'
#' This function returns a ggplot derived from an rpsftm object showing hazard ratio and 
#' 95 percent CI for each psi tested in the g-estimation. A flat line suggests that there is no 
#' impact of switching on the results. A sloped line indicates that switching does affect the results.
#'
#' @param x rpsftm object (rpsftm)
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
#' ggdiscount(imm.fit)
#' @import survival
#' @import rpsftm
#' @import ggplot2
#' @import dplyr

ggdiscount <- function(x){
  if (!("rpsftm" %in% class(x))){
    stop("Please provide an rpsftm object.")
  }

  psi.range <- x$eval_z$psi

  # get the data
  df <- rpsftm.data(x)

  # get the formula used including strata

  frm <- rpsftm.formula(x)

  # for each value of psi get a cox model

  hr.df <- NULL
  for (this.psi in (psi.range)) {
    this.df <- mutate(df, time = ifelse(.arm == 0, t.off + t.on * exp(-this.psi), time))

    this.cox <-coxph(frm, data = this.df) %>%
      summary()

    this.hr <- data_frame(psi = this.psi,
                          hr = this.cox$conf.int[1],
                          hrcil = this.cox$conf.int[3],
                          hrciu = this.cox$conf.int[4]
    )

    if (is.null(hr.df)){
      hr.df <- this.hr
    } else{
      hr.df <- rbind(hr.df, this.hr)
    }
  }

  rc <- mutate(hr.df, discount = exp(-psi)) %>%
    ggplot(aes(x=discount, y = hr, ymin = hrcil, ymax = hrciu)) +
    theme_bw() +
    geom_ribbon(alpha = 0.5, fill = "blue") +
    geom_line(color = "blue") +
    geom_hline(yintercept = 1, linetype = 1) +
    geom_vline(xintercept = 1, linetype = 1) +
    ylab("Hazard Ratio and 95% CI") +

    xlab(expression(paste("Discount for exposure to switch treatment ", e^{psi}))) +
    scale_x_log10(breaks = c(0.3, 0.5, 0.7, 1, 2, 3))

  return(rc)

}
