% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{output_to_df}
\alias{output_to_df}
\title{Provide WH model Fit Results as a Data.frame}
\usage{
output_to_df(object, dim1 = "x", dim2 = "t")
}
\arguments{
\item{object}{An object of class  \code{"WH_1d"} or \code{"WH_2d"} returned
by one of the eponymous functions \code{\link[=WH_1d]{WH_1d()}} or \code{\link[=WH_2d]{WH_2d()}}}

\item{dim1}{The optional name to be given to the first dimension}

\item{dim2}{The optional name to be given to the second dimension}
}
\value{
A data.frame regrouping information about the fitted and predicted
values, the model variance, residuals and effective degrees of freedom...
}
\description{
Provide WH model Fit Results as a Data.frame
}
\examples{
d <- portfolio_mort$d
ec <- portfolio_mort$ec

y <- log(d / ec)
y[d == 0] <- - 20
wt <- d

fit_1d <- WH_1d(d, ec)
output_to_df(fit_1d)

keep_age <- which(rowSums(portfolio_LTC$ec) > 5e2)
keep_duration <- which(colSums(portfolio_LTC$ec) > 1e3)

d  <- portfolio_LTC$d[keep_age, keep_duration]
ec <- portfolio_LTC$ec[keep_age, keep_duration]

y <- log(d / ec) # observation vector
y[d == 0] <- - 20
wt <- d

# Maximum likelihood
fit_2d <- WH_2d(d, ec)
output_to_df(fit_2d)

}
