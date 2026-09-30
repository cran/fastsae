## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)

## ----load---------------------------------------------------------------------
library(fastsae)
library(ggplot2)

data("mys")
head(mys)

## ----fit_fh-------------------------------------------------------------------
# Fit Fay-Herriot model
fit_fh <- eblup_fh(
  formula = y ~ x1 + x2 + x3,
  vardir = ~vardir,
  data = mys,
  method = "REML",
  print_result = FALSE
)

## ----summary------------------------------------------------------------------
summary(fit_fh)

## ----coef---------------------------------------------------------------------
coef(fit_fh)

## ----fitted_res---------------------------------------------------------------
# Fitted values (EBLUP)
head(fitted(fit_fh))

# Residuals (direct estimate - EBLUP)
head(residuals(fit_fh))

## ----plot_estimates-----------------------------------------------------------
autoplot(fit_fh, type = "estimates")

## ----plot_mse-----------------------------------------------------------------
autoplot(fit_fh, type = "mse")

## ----equivalence, message=FALSE, warning=FALSE--------------------------------
if (requireNamespace("sae", quietly = TRUE)) {
  mys_clean <- as.data.frame(na.omit(mys))
  fit_fast <- eblup_fh(y ~ x1 + x2 + x3, vardir = ~vardir, data = mys_clean, print_result = FALSE)
  fit_sae <- sae::eblupFH(y ~ x1 + x2 + x3, vardir = vardir, data = mys_clean)

  # Check EBLUP estimates
  all.equal(fit_fast$df_eblup$eblup, as.vector(fit_sae$eblup))

  # Check regression coefficients
  all.equal(as.vector(coef(fit_fast)), as.vector(fit_sae$fit$estcoef$beta))

  # Check random effect variance (sigma2_u)
  all.equal(fit_fast$random_effect_var, fit_sae$fit$refvar)
}

