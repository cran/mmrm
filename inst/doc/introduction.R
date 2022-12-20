## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(mmrm)

## ----common-usage-modelcall---------------------------------------------------
fit <- mmrm(
  formula = FEV1 ~ RACE + ARMCD * AVISIT + us(AVISIT | USUBJID),
  data = fev_data
)

## ----common-usage-print-------------------------------------------------------
print(fit)

## ----low-level-control, eval=FALSE--------------------------------------------
#  mmrm_control(
#    method = "Kenward-Roger",
#    optimizer = c("L-BFGS-B", "BFGS"),
#    n_cores = 2,
#    start = c(0, 1, 1, 0, 1, 0),
#    accept_singular = FALSE,
#    drop_visit_levels = FALSE
#  )

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
  method = "Kenward-Roger-Linear"
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

## ----extraction-summary-component---------------------------------------------
component(fit, name = c("convergence", "evaluations", "conv_message"))

## -----------------------------------------------------------------------------
component(fit, name = "call")

## ---- eval=FALSE--------------------------------------------------------------
#  component(fit)

## ----low-level-hmmrmtmb-------------------------------------------------------
fit_mmrm(
  formula = FEV1 ~ RACE + ARMCD * AVISIT + us(AVISIT | USUBJID),
  data = fev_data,
  weights = rep(1, nrow(fev_data)),
  reml = TRUE,
  control = mmrm_control()
)

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
fit <- mmrm(
  formula = FEV1 ~ RACE + ARMCD * AVISIT + us(AVISIT | USUBJID),
  data = fev_data
)

if (require(emmeans)) {
  emmeans(fit, ~ ARMCD | AVISIT)
}

