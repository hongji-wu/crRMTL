% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RMTLd_test.R
\name{RMTLd_test}
\alias{RMTLd_test}
\title{Comparing restricted mean survival time}
\usage{
RMTLd_test(time, status, group, alpha = 0.05, digits = 3, tau = NULL)
}
\arguments{
\item{time}{The follow-up time for right censored data.}

\item{status}{The status indicator, 1 = event of interest, 2 = competing event and 0 = right censored.}

\item{group}{The group indicator for comparison. The elements of this vector take either 1 or 0. Normally, 0 = control group, 1 = active treatment group.}

\item{alpha}{The default is 0.05. (1-\code{alpha}) confidence intervals are reported.}

\item{digits}{The decimal places settings, digitis = 3 are set by default.}

\item{tau}{A scaler value to specify the truncation time point for the RMST calculation.
The default is the minimum of the maximum follow-up time of two groups}
}
\value{
an object of class RMTLd_test.

\item{tau}{the truncation time used in the analyses}

\item{Note}{a note regarding the specification of the truncation time(tau), the default tau will be reported
if tau was not specified.}

\item{RMTL}{RMTL results in two groups, including RMTL of event 1 and event 2, and corresponding variance and CI in group 0 and 1}

\item{Test.event1}{
Three kinds of effect size between groups of event 1, including Cause-specific Hazard Ratios(CHR),
Subdistributional Hazard Ratio(SHR) and RMTLd, and corresponding confidence intervals.
Meanwhile, the Z-statistics and P-value are estimated via log-rank test(CHR), Gray's test(SHR) and RMTLd test(RMTLd)}

\item{Test.event2}{
Three kinds of effect size between groups of event 2, including Cause-specific Hazard Ratios(CHR),
Subdistributional Hazard Ratio(SHR) and RMTLd, and corresponding confidence intervals.}
Meanwhile, the Z-statistics and P-value are estimated via log-rank test(CHR), Gray's test(SHR) and RMTLd test(RMTLd)
}
\description{
Performs two-sample comparisons using the restricted mean time lost(RMTL) as a summary measure of the cumulative incidence function under competing risks
}
\examples{
library(crRMTL)
data(simdata)
# The time point is set as the minimum of the maximum follow-up time of two groups when tau = NULL.
RMTLd_test(simdata$time, simdata$status, simdata$group, alpha = 0.05, digits = 3, tau = NULL)
# The time point is set as tau = 3.
RMTLd_test(simdata$time, simdata$status, simdata$group, alpha = 0.05, digits = 3, tau = 3)

}
