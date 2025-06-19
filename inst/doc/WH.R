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
WH_1d_fit <- WH(portfolio_mort$d, portfolio_mort$ec)

## ----fit-2d-------------------------------------------------------------------
# Two-dimensional case
WH_2d_fit <- WH(portfolio_LTC$d, portfolio_LTC$ec)

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
WH_1d_fit |> predict(newdata = 40:99) |> plot()
WH_2d_fit |> predict(newdata = list(age = 60:109, duration = 0:19)) |> plot()

## -----------------------------------------------------------------------------
WH_1d_df <- WH_1d_fit |> output_to_df()
WH_2d_df <- WH_2d_fit |> output_to_df()

