## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)

## ----data_prep----------------------------------------------------------------
library(fastsae)
data("cornsoybean")
data("cornsoybeanmeans")

# Align column names for population auxiliary means
df_pop <- cornsoybeanmeans
names(df_pop)[names(df_pop) == "MeanCornPixPerSeg"] <- "CornPix"
names(df_pop)[names(df_pop) == "MeanSoyBeansPixPerSeg"] <- "SoyBeansPix"
names(df_pop)[names(df_pop) == "CountyIndex"] <- "County"

head(cornsoybean)
head(df_pop)

## ----fit_bhf------------------------------------------------------------------
fit_bhf <- eblup_bhf(
  formula = CornHec ~ CornPix + SoyBeansPix,
  unit_data = cornsoybean,
  Xpop = df_pop,
  domain_var = "County",
  popsize_var = "PopnSegments",
  method = "REML",
  compute_mse = TRUE,
  B = 50,
  seed = 123,
  print_result = FALSE
)

summary(fit_bhf)

## ----eblup_table--------------------------------------------------------------
head(fit_bhf$df_eblup)

## ----plot_bhf-----------------------------------------------------------------
# Plot MSE across counties
autoplot(fit_bhf, type = "mse")

# Plot EBLUP estimates with confidence intervals
autoplot(fit_bhf, type = "comparison")

