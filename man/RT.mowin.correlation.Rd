% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RT.mowin.correlation.R
\name{RT.mowin.correlation}
\alias{RT.mowin.correlation}
\title{Response Time Moving Windows Function for Correlation}
\usage{
RT.mowin.correlation(
  data,
  timeVarLabel,
  cor1,
  cor2 = NULL,
  PVtype = NULL,
  timeStop,
  PV = TRUE,
  timeStart = 0,
  intLength = 2000,
  windowShift = 100,
  endInclusive = TRUE,
  PVnumber = 10,
  minObserv = 40
)
}
\arguments{
\item{data}{Here input the data frame which contains needed variables.}

\item{timeVarLabel}{String. The name of the time variable.}

\item{cor1}{String which contains the first variable name for which you want the correlation value.}

\item{cor2}{String which contains the second variable name for which you want the correlation value. Defaults to NULL.}

\item{PVtype}{String which contains information on which Plausible Values you want to use – the variable ending (e.g. math, scie, read...).}

\item{timeStop}{Number. Indicates the end time of your analysis.}

\item{PV}{Boolean. TRUE means correlation with Plausible Values is needed. FALSE means correlation with two declared variables is needed. Defaults to TRUE.}

\item{timeStart}{Number. Indicates the starting time of your analysis. Defaults to 0.}

\item{intLength}{Number. Indicates the length of the time window. E.g. If your time variable is in milliseconds then value 2000 will mean 2s intervals (e.g. 0-2, 0.1-2.1, etc.). Defaults to 2000.}

\item{windowShift}{Number. Indicates by how much the window will shift. E.g. If your time variable is in milliseconds then value 100 will mean shift by 0.1s (e.g., 0-2, 0.1-2.1, etc.). Defaults to 100.}

\item{endInclusive}{Boolean. If TRUE then the time intervals will be inclusive of the end of each interval window (e.g. 0-2s inclusive of 2s but not of 0s). If FALSE, then it is inclusive of the beginning (e.g. 0-2s, inclusive of 0s but not of 2s). Defaults to TRUE.}

\item{PVnumber}{Number which represents the number of Plausible Values. Defaults to 10.}

\item{minObserv}{Number. Represents the minimal number of observations required for the correlation to run. If not reached, returns NA. Defaults to 40.}
}
\value{
Data frame which contains the correlation value and confidence intervals for each investigate time interval. First column indicates the investigated time interval. >/< indicates which number is inclusive in the time interval
}
\description{
This function computes the (unweighted) correlation of a variable with PISA scores or with a second declared variable.
PISA scores are computed from Plausible values.
Correlations are computed across time using the moving window approach method.
This method analyzes response time windows of a set length and moves
the windows by a specified jump. This creates overlapping intervals which
allows to see how an observed variable develops over time. The smaller the
window, the more detailed observation of the development of the variable.
This function performs the analysis in individual windows and provides you
with a table with a result for each investigated time interval.
This function assumes that Plausible Values are labeled as PVXSUBJECT (e.g. PV1MATH, PV2SCIE...).
It also assumes that variable does not contain NA codes. Predefined values are in milliseconds.
}
