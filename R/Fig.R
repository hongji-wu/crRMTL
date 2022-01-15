#' @name Fig
#' @title The curves of Cumulative Cause-specific Hazard or Cumulative Incidence Function.
#'
#' @description Drawing the curves of Cumulative Cause-specific Hazard or Cumulative Incidence Function.
#'
#' @usage  Fig(time, status, group, event.type = 1, fig = 1,
#'            x.lim = c(0, 10), y.lim = c(0, 1),
#'            x.space = seq(0, 10, 2), y.space = seq(0, 1, 0.2),
#'            group.lab = c("group 0", "group 1"),
#'            col = c("steelblue", "red"), lty = c(1, 2))
#'
#' @importFrom cmprsk cuminc
#' @importFrom graphics axis legend lines mtext
#'
#' @param time The follow-up time for right censored data.
#' @param status The status indicator, 1 = event of interest, 2 = competing event and 0 = right censored.
#' @param group The group indicator for comparison. The elements of this vector take either 1 or 0. Normally, 0 = control group, 1 = active treatment group.
#' @param event.type The indicator of event type, 1 = event 1, 2 = event 2.
#' @param fig The indicator of curves, 1 = Cumulative incidence function, 2 = Cumulative Cause-specific Hazard.
#' @param x.lim The range of x-axis.
#' @param y.lim The range of y-axis.
#' @param x.space The scale of x-axis.
#' @param y.space The scale of y-axis.
#' @param group.lab The labels of groups.
#' @param col The color of curves.
#' @param lty The type of curves.
#'
#' @return The curves of Cumulative Cause-specific Hazard or Cumulative Incidence Function.
#'
#' @export
#'
#' @examples
#' data(simdata)
#'time <- simdata$time
#'status <- simdata$status
#'group <- simdata$group

#'open <- par(no.readonly = TRUE)
#'par(mfrow = c(2, 2))
#'Fig(time, status, group, event.type = 1, fig = 1,
#'    x.lim = c(0, 6), y.lim = c(0, 0.8),
#'    group.lab = c("RT", "CRT"), col = c("steelblue", "red"), lty = c(1, 2))
#'Fig(time, status, group, event.type = 2, fig = 1,
#'    x.lim = c(0, 6), y.lim = c(0, 0.8),
#'    group.lab = c("RT", "CRT"), col = c("steelblue", "red"), lty = c(1, 2))
#'Fig(time, status, group, event.type = 1, fig = 2,
#'    x.lim = c(0, 6), y.lim = c(0, 0.8),
#'    group.lab = c("RT", "CRT"), col = c("steelblue", "red"), lty = c(1, 2))
#'Fig(time, status, group, event.type = 2, fig = 2,
#'    x.lim = c(0, 6), y.lim = c(0, 0.8),
#'    group.lab = c("RT", "CRT"), col = c("steelblue", "red"), lty = c(1, 2))
#'par(open)

Fig <- function(time, status, group, event.type = 1, fig = 1,
                x.lim = c(0, 10), y.lim = c(0, 1), x.space = seq(0, 10, 2), y.space = seq(0, 1, 0.2),
                group.lab = c("group 0", "group 1"), col = c("steelblue", "red"), lty = c (1, 2))
{
  ## CIF estimation
  fit <- cuminc(time, status, group, cencode = 0)
  x1 <- data.frame(t = fit $ '0 1' $ time, e = fit $'0 1'$ est)
  x2 <- data.frame(t = fit $ '1 1' $ time, e = fit $'1 1'$ est)
  x3 <- data.frame(t = fit $ '0 2' $ time, e = fit $'0 2'$ est)
  x4 <- data.frame(t = fit $ '1 2' $ time, e = fit $'1 2'$ est)

  ## Cumulative cause-specific hazards
  status1 <- 1 * (status == 1)
  status2 <- 1 * (status == 2)
  example1 <- data.frame(time, status1, status2, group)
  d0 <- example1[example1$group == 0, ]
  d1 <- example1[example1$group == 1, ]
  s01 <- survfit(Surv(d0$time, d0$status1) ~ 1) ## survival subjects of group 0 and event 1
  s11 <- survfit(Surv(d1$time, d1$status1) ~ 1) ## survival subjects of group 1 and event 1
  s02 <- survfit(Surv(d0$time, d0$status2) ~ 1) ## survival subjects of group 0 and event 2
  s12 <- survfit(Surv(d1$time, d1$status2) ~ 1) ## survival subjects of group 1 and event 2
  t01 <- s01[["time"]]
  t11 <- s11[["time"]]
  t02 <- s02[["time"]]
  t12 <- s12[["time"]]
  cumh01<-cumsum(s01[["n.event"]] / s01[["n.risk"]])
  cumh11<-cumsum(s11[["n.event"]] / s11[["n.risk"]])
  cumh02<-cumsum(s02[["n.event"]] / s02[["n.risk"]])
  cumh12<-cumsum(s12[["n.event"]] / s12[["n.risk"]])

  if(event.type == 1 & fig == 1){

    plot(x1$t, x1$e, lwd = 2, "S", xlim = x.lim, ylim = y.lim, col = col[1], lty = lty[1],
         lab=c(12 ,5, 1), cex.lab = 1.3, cex.axis = 1, xlab = " ", ylab = " ",
         xaxt = "n", yaxt = "n", mgp = c(.2, 1, 0), bty = "l")
    lines(x2$t, x2$e, col = col[2], lwd = 2, "S", lty = lty[2])
    axis(side = 1, x.space, cex.axis = 1, las = 1, lwd = 1, line = 0, mgp = c(1, .4, 0), tck = -.02)#,tck=-0.07
    axis(side = 2, y.space, cex.axis = 1, las = 1, lwd = 1, mgp = c(1, .6, 0), tck = -.02)
    mtext("Time", side = 1, line = 1.7, cex = 1)
    mtext("Cumulative Incidence", side = 2, line = 2.2, cex = 1)
    # mtext("A)",side=3,line=1.5,cex=1,adj=-0.15)
    legend("topleft", group.lab, lty = lty, col = col, lwd = 2, cex = 0.9, bty = "n",
           x.intersp = 0.5, y.intersp = 0.95)
  }

  if(event.type == 2 & fig == 1){

    plot(x3$t, x3$e, lwd = 2, "S", xlim = x.lim, ylim = y.lim, col = col[1], lty = lty[1],
         lab=c(12 ,5, 1), cex.lab = 1.3, cex.axis = 1, xlab = " ", ylab = " ",
         xaxt = "n", yaxt = "n", mgp = c(.2, 1, 0), bty = "l")
    lines(x4$t, x4$e, col = col[2], lwd = 2, "S", lty = lty[2])
    axis(side = 1, x.space, cex.axis = 1, las = 1, lwd = 1, line = 0, mgp = c(1, .4, 0), tck = -.02)#,tck=-0.07
    axis(side = 2, y.space, cex.axis = 1, las = 1, lwd = 1, mgp = c(1, .6, 0), tck = -.02)
    mtext("Time", side = 1, line = 1.7, cex = 1)
    mtext("Cumulative Incidence", side = 2, line = 2.2, cex = 1)
    # mtext("A)",side=3,line=1.5,cex=1,adj=-0.15)
    legend("topleft", group.lab, lty = lty, col = col, lwd = 2, cex = 0.9, bty = "n",
           x.intersp = 0.5, y.intersp = 0.95)
  }

  if(event.type == 1 & fig == 2){

    plot(t01, cumh01, lwd = 2, "S", xlim = x.lim, ylim = y.lim, col = col[1], lty = lty[1],
         lab=c(12 ,5, 1), cex.lab = 1.3, cex.axis = 1, xlab = " ", ylab = " ",
         xaxt = "n", yaxt = "n", mgp = c(.2, 1, 0), bty = "l")
    lines(t11, cumh11, col = col[2], lwd = 2, "S", lty = lty[2])
    axis(side = 1, x.space, cex.axis = 1, las = 1, lwd = 1, line = 0, mgp = c(1, .4, 0), tck = -.02)#,tck=-0.07
    axis(side = 2, y.space, cex.axis = 1, las = 1, lwd = 1, mgp = c(1, .6, 0), tck = -.02)
    mtext("Time", side = 1, line = 1.7, cex = 1)
    mtext("Cumulative CSH", side = 2, line = 2.2, cex = 1)
    legend("topleft", group.lab, lty = lty, col = col, lwd = 2, cex = 0.9, bty = "n",
           x.intersp = 0.5, y.intersp = 0.95)
  }

  if(event.type == 2 & fig == 2){

    plot(t02, cumh02, lwd = 2, "S", xlim = x.lim, ylim = y.lim, col = col[1], lty = lty[1],
         lab=c(12 ,5, 1), cex.lab = 1.3, cex.axis = 1, xlab = " ", ylab = " ",
         xaxt = "n", yaxt = "n", mgp = c(.2, 1, 0), bty = "l")
    lines(t12, cumh12, col = col[2], lwd = 2, "S", lty = lty[2])
    axis(side = 1, x.space, cex.axis = 1, las = 1, lwd = 1, line = 0, mgp = c(1, .4, 0), tck = -.02)#,tck=-0.07
    axis(side = 2, y.space, cex.axis = 1, las = 1, lwd = 1, mgp = c(1, .6, 0), tck = -.02)
    mtext("Time", side = 1, line = 1.7, cex = 1)
    mtext("Cumulative CSH", side = 2, line = 2.2, cex = 1)
    legend("topleft", group.lab, lty = lty, col = col, lwd = 2, cex = 0.9, bty = "n",
           x.intersp = 0.5, y.intersp = 0.95)
  }
}

