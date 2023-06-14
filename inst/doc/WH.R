## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "",
  fig.width = 10, 
  fig.asp = 0.618,
  fig.align = "center",
  out.width = "100%"
)

library(WH)

## ----fit-1d-------------------------------------------------------------------
# One-dimensional case
d <- portfolio_mort$d
ec <- portfolio_mort$ec

WH_1d_fit <- WH_1d(d, ec)

## ----fit-2d-------------------------------------------------------------------
# Two-dimensional case
keep_age <- which(rowSums(portfolio_LTC$ec) > 5e2)
keep_duration <- which(colSums(portfolio_LTC$ec) > 1e3)

d  <- portfolio_LTC$d[keep_age, keep_duration]
ec <- portfolio_LTC$ec[keep_age, keep_duration]

WH_2d_fit <- WH_2d(d, ec)

## ----print--------------------------------------------------------------------
WH_1d_fit
WH_2d_fit

## ----plot---------------------------------------------------------------------
plot(WH_1d_fit)
plot(WH_1d_fit, "res")
plot(WH_1d_fit, "edf")

plot(WH_2d_fit)
plot(WH_2d_fit, "std_y_hat")

## ----predict------------------------------------------------------------------
WH_1d_fit |> predict(newdata = 18:99) |> plot()
WH_2d_fit |> predict(newdata = list(age = 50:99,
                                    duration = 0:19)) |> plot()

## -----------------------------------------------------------------------------
WH_1d_df <- WH_1d_fit |> output_to_df()
WH_2d_df <- WH_2d_fit |> output_to_df()

