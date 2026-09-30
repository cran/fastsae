## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)

## ----sfh_fit------------------------------------------------------------------
library(fastsae)
data("mys")
data("mys_proxmat")

# Fit Spatial Fay-Herriot model with REML
fit_sfh <- eblup_sfh(
  formula = y ~ x1 + x2 + x3,
  vardir = ~vardir,
  data = mys,
  W = mys_proxmat,
  method = "REML",
  print_result = FALSE
)

summary(fit_sfh)

## ----unsampled----------------------------------------------------------------
# Count sampled vs unsampled domains
table(is.na(mys$y))

# Inspect estimates for unsampled domains
head(fit_sfh$df_eblup[is.na(mys$y), ])

## ----sfh_pbmse----------------------------------------------------------------
fit_sfh_pb <- eblup_sfh(
  formula = y ~ x1 + x2 + x3,
  vardir = ~vardir,
  data = mys,
  W = mys_proxmat,
  mse_method = "pbmse",
  B = 50,
  n_threads = 2,
  seed = 123,
  print_result = FALSE
)

head(fit_sfh_pb$df_eblup[, c("domain", "y", "eblup", "mse", "mse_pb", "mse_pbbc")])

## ----stfh_fit-----------------------------------------------------------------
data("mys_panel")

# Prepare balanced panel without missing values
panel_data <- mys_panel[!is.na(mys_panel$y) & mys_panel$year >= 2024, ]
W_sub <- mys_proxmat[-c(21, 25), -c(21, 25)]

# Fit Spatio-Temporal model with bootstrap MSE
fit_stfh <- eblup_stfh(
  formula = y ~ x1 + x2 + x3,
  data = panel_data,
  vardir = ~vardir,
  domain = ~area,
  time = ~year,
  W = W_sub,
  model = "ST",
  compute_mse = TRUE,
  B = 25,
  seed = 42,
  print_result = FALSE
)

summary(fit_stfh)

## ----compare_models-----------------------------------------------------------
# Compare Fay-Herriot vs Spatial Fay-Herriot
fit_fh <- eblup_fh(y ~ x1 + x2 + x3, vardir = ~vardir, data = na.omit(mys), print_result = FALSE)
W_clean <- mys_proxmat[!is.na(mys$y), !is.na(mys$y)]
fit_sfh_clean <- eblup_sfh(y ~ x1 + x2 + x3, vardir = ~vardir, data = na.omit(mys), W = W_clean, print_result = FALSE)

autoplot(list("Standard FH" = fit_fh, "Spatial FH" = fit_sfh_clean), type = "comparison")

