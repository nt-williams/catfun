% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/matched_or.R
\name{matched_or}
\alias{matched_or}
\title{Matched pairs odds ratio and confidence interval}
\usage{
matched_or(df, ...)
}
\arguments{
\item{df}{a dataframe with binary variables x and y or a 2 x 2 frequency table/matrix. If a table or matrix, x and y must be NULL. Used to select method.}

\item{...}{further arguments passed to or from other methods.}
}
\value{
a list with class "matched_or" with the following components:

\item{tab}{2x2 table using for calculating risk difference}
\item{or}{dataframe with columns corresponding to matched-pairs OR, lower bound, and upper bound of CI}
\item{conf.level}{specified confidence level}
}
\description{
Create odds ratio and confidence interval from matched pairs data.
}
\details{
The matched pairs odds ratio and confidence interval is the equivalent of
   calculating a Cochran-Mantel-Haenszel odds ratio where each pair is treated as a stratum.
}
\examples{
set.seed(1)
gene <- data.frame(pair = seq(1:35),
                   ulcer = rbinom(35, 1, .7),
                   healthy = rbinom(35, 1, .4))

matched_or(gene, ulcer, healthy)
}
