## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(mmrm)

## ----getting-started----------------------------------------------------------
library(mmrm)
fit <- mmrm(
  formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
  data = fev_data
)

## ----print--------------------------------------------------------------------
fit

## ----summary------------------------------------------------------------------
summary(fit)

## ----include = FALSE----------------------------------------------------------
library(mmrm)

## ----low-level-control, results = 'hide'--------------------------------------
mmrm_control(
  method = "Kenward-Roger",
  optimizer = c("L-BFGS-B", "BFGS"),
  n_cores = 2,
  start = c(0, 1, 1, 0, 1, 0),
  accept_singular = FALSE,
  drop_visit_levels = FALSE
)

## ----common-changes-reml------------------------------------------------------
fit_ml <- mmrm(
  formula = FEV1 ~ RACE + ARMCD * AVISIT + us(AVISIT | USUBJID),
  data = fev_data,
  reml = FALSE
)
fit_ml

## ----common-changes-optim-----------------------------------------------------
fit_opt <- mmrm(
  formula = FEV1 ~ RACE + ARMCD * AVISIT + us(AVISIT | USUBJID),
  data = fev_data,
  optimizer = "BFGS"
)
fit_opt

## ----common-changes-cov-------------------------------------------------------
fit_cs <- mmrm(
  formula = FEV1 ~ RACE + ARMCD * AVISIT + cs(AVISIT | USUBJID),
  data = fev_data,
  reml = FALSE
)
fit_cs

## ----common-changes-weights---------------------------------------------------
fit_wt <- mmrm(
  formula = FEV1 ~ RACE + ARMCD * AVISIT + us(AVISIT | USUBJID),
  data = fev_data,
  weights = fev_data$WEIGHT
)
fit_wt

## ----group-cov----------------------------------------------------------------
fit_cs <- mmrm(
  formula = FEV1 ~ RACE + ARMCD * AVISIT + cs(AVISIT | ARMCD / USUBJID),
  data = fev_data,
  reml = FALSE
)
VarCorr(fit_cs)

## ----kr-----------------------------------------------------------------------
fit_kr <- mmrm(
  formula = FEV1 ~ RACE + ARMCD * AVISIT + us(AVISIT | USUBJID),
  data = fev_data,
  method = "Kenward-Roger"
)

## ----kr_summary---------------------------------------------------------------
summary(fit_kr)

## ----kr_lin-------------------------------------------------------------------
fit_kr_lin <- mmrm(
  formula = FEV1 ~ RACE + ARMCD * AVISIT + us(AVISIT | USUBJID),
  data = fev_data,
  method = "Kenward-Roger",
  vcov = "Kenward-Roger-Linear"
)

## -----------------------------------------------------------------------------
fit_emp <- mmrm(
  formula = FEV1 ~ RACE + ARMCD * AVISIT + us(AVISIT | USUBJID),
  data = fev_data,
  method = "Satterthwaite",
  vcov = "Empirical"
)

## ----sparse-------------------------------------------------------------------
sparse_data <- fev_data[fev_data$AVISIT != "VIS3", ]
sparse_result <- mmrm(
  FEV1 ~ RACE + ar1(AVISIT | USUBJID),
  data = sparse_data,
  drop_visit_levels = FALSE
)

dropped_result <- mmrm(
  FEV1 ~ RACE + ar1(AVISIT | USUBJID),
  data = sparse_data
)

## ----sparse_cor---------------------------------------------------------------
cov2cor(VarCorr(sparse_result))
cov2cor(VarCorr(dropped_result))

## ----include = FALSE----------------------------------------------------------
library(mmrm)

## ----extraction-summary-fit---------------------------------------------------
fit <- mmrm(
  formula = FEV1 ~ RACE + ARMCD * AVISIT + us(AVISIT | USUBJID),
  data = fev_data
)
fit_summary <- summary(fit)

## ----extraction-summary-fit-coef----------------------------------------------
fit_summary$coefficients

## ----extraction-summary-fit-str-----------------------------------------------
str(fit_summary)

## ----residuals-response-------------------------------------------------------
fit <- mmrm(
  formula = FEV1 ~ RACE + ARMCD * AVISIT + us(AVISIT | USUBJID),
  data = fev_data
)
residuals_resp <- residuals(fit, type = "response")

## ----residuals-pearson--------------------------------------------------------
residuals_pearson <- residuals(fit, type = "pearson")

## ----residuals-norm-----------------------------------------------------------
residuals_norm <- residuals(fit, type = "normalized")

## -----------------------------------------------------------------------------
fit |>
  tidy()

## -----------------------------------------------------------------------------
fit |>
  tidy(conf.int = TRUE, conf.level = 0.9)

## -----------------------------------------------------------------------------
fit |>
  glance()

## -----------------------------------------------------------------------------
fit |>
  augment()

## -----------------------------------------------------------------------------
fit |>
  augment(interval = "confidence", type.residuals = "normalized")

## ----extraction-summary-component---------------------------------------------
component(fit, name = c("convergence", "evaluations", "conv_message"))

## -----------------------------------------------------------------------------
component(fit, name = "call")

## ----eval=FALSE---------------------------------------------------------------
#  component(fit)

## ----include = FALSE----------------------------------------------------------
library(mmrm)

## ----low-level-hmmrmtmb-------------------------------------------------------
fit_mmrm(
  formula = FEV1 ~ RACE + ARMCD * AVISIT + us(AVISIT | USUBJID),
  data = fev_data,
  weights = rep(1, nrow(fev_data)),
  reml = TRUE,
  control = mmrm_control()
)

## ----include = FALSE----------------------------------------------------------
library(mmrm)

## ----1d_satterthwaite---------------------------------------------------------
fit <- mmrm(
  formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
  data = fev_data
)

contrast <- numeric(length(component(fit, "beta_est")))
contrast[3] <- 1

df_1d(fit, contrast)

## ----1d_kr--------------------------------------------------------------------
fit_kr <- mmrm(
  formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
  data = fev_data,
  method = "Kenward-Roger"
)

df_1d(fit_kr, contrast)

## ----md_satterthwaite---------------------------------------------------------
contrast <- matrix(data = 0, nrow = 2, ncol = length(component(fit, "beta_est")))
contrast[1, 2] <- contrast[2, 3] <- 1

df_md(fit, contrast)

## ----md_kr--------------------------------------------------------------------
df_md(fit_kr, contrast)

## ----emmeans------------------------------------------------------------------
library(emmeans)
lsmeans_by_visit <- emmeans(fit, ~ ARMCD | AVISIT)
lsmeans_by_visit

## ----pdiff--------------------------------------------------------------------
pairs(lsmeans_by_visit, reverse = TRUE)

## ----include = FALSE----------------------------------------------------------
library(mmrm)

## ----warning=FALSE, message=FALSE---------------------------------------------
library(tidymodels)

## -----------------------------------------------------------------------------
model <- linear_reg() |>
  set_engine("mmrm", method = "Satterthwaite") |>
  fit(FEV1 ~ RACE + ARMCD * AVISIT + us(AVISIT | USUBJID), fev_data)
model

## -----------------------------------------------------------------------------
model_with_control <- linear_reg() |>
  set_engine("mmrm", control = mmrm_control(method = "Satterthwaite")) |>
  fit(FEV1 ~ RACE + ARMCD * AVISIT + us(AVISIT | USUBJID), fev_data)

## -----------------------------------------------------------------------------
predict(model, new_data = fev_data)

## -----------------------------------------------------------------------------
predict(
  model,
  new_data = fev_data,
  type = "raw",
  opts = list(se.fit = TRUE, interval = "prediction", nsim = 10L)
)

## -----------------------------------------------------------------------------
augment(model, new_data = fev_data) |>
  select(USUBJID, AVISIT, .resid, .pred)

## -----------------------------------------------------------------------------
mmrm_spec <- linear_reg() |>
  set_engine("mmrm", method = "Satterthwaite")

mmrm_wflow <- workflow() |>
  add_variables(outcomes = FEV1, predictors = c(RACE, ARMCD, AVISIT, USUBJID)) |>
  add_model(mmrm_spec, formula = FEV1 ~ RACE + ARMCD * AVISIT + us(AVISIT | USUBJID))

mmrm_wflow |>
  fit(data = fev_data)

## -----------------------------------------------------------------------------
mmrm_recipe <- recipe(FEV1 ~ ., data = fev_data) |>
  step_dummy(ARMCD) |>
  step_interact(terms = ~ starts_with("ARMCD"):AVISIT)

## -----------------------------------------------------------------------------
mmrm_recipe |>
  prep() |>
  juice()

## -----------------------------------------------------------------------------
mmrm_spec_with_cov <- linear_reg() |>
  set_engine(
    "mmrm",
    method = "Satterthwaite",
    covariance = as.cov_struct(~ us(AVISIT | USUBJID))
  )

## -----------------------------------------------------------------------------
(mmrm_wflow_nocov <- workflow() |>
  add_model(mmrm_spec_with_cov, formula = FEV1 ~ SEX) |>
  add_recipe(mmrm_recipe))

## -----------------------------------------------------------------------------
(fit_tidy <- fit(mmrm_wflow_nocov, data = fev_data))

## -----------------------------------------------------------------------------
fit_tidy |>
  hardhat::extract_fit_engine()

