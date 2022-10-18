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

## ----common-changes-reml------------------------------------------------------
fit_ml <- mmrm(
  formula = FEV1 ~ RACE + ARMCD * AVISIT + us(AVISIT | USUBJID),
  data = fev_data,
  reml = FALSE
)

print(fit_ml)

## ----common-changes-optim-----------------------------------------------------
fit_opt <- mmrm(
  formula = FEV1 ~ RACE + ARMCD * AVISIT + us(AVISIT | USUBJID),
  data = fev_data,
  optimizer = "BFGS"
)

print(fit_opt)

## ----common-changes-cov-------------------------------------------------------
fit_cs <- mmrm(
  formula = FEV1 ~ RACE + ARMCD * AVISIT + cs(AVISIT | USUBJID),
  data = fev_data,
  reml = FALSE
)

print(fit_cs)

## ----common-changes-weights---------------------------------------------------
fit_wt <- mmrm(
  formula = FEV1 ~ RACE + ARMCD * AVISIT + us(AVISIT | USUBJID),
  data = fev_data,
  weights = fev_data$WEIGHT
)

print(fit_wt)

## ----group-cov----------------------------------------------------------------
fit_cs <- mmrm(
  formula = FEV1 ~ RACE + ARMCD * AVISIT + cs(AVISIT | ARMCD / USUBJID),
  data = fev_data,
  reml = FALSE
)

print(VarCorr(fit_cs))

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

## ----low-level-control, eval=FALSE--------------------------------------------
#  mmrm_control(
#    optimizer = stats::nlminb,
#    optimizer_args = list(upper = Inf, lower = 0),
#    optimizer_control = list(),
#    start = c(0, 1, 1, 0, 1, 0),
#    accept_singular = FALSE
#  )

## -----------------------------------------------------------------------------
fit <- mmrm(
  formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
  data = fev_data
)

contrast <- numeric(length(component(fit, "beta_est")))
contrast[3] <- 1

df_1d(fit, contrast)

## -----------------------------------------------------------------------------
fit <- mmrm(
  formula = FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID),
  data = fev_data
)

contrast <- matrix(data = 0, nrow = 2, ncol = length(component(fit, "beta_est")))
contrast[1, 2] <- contrast[2, 3] <- 1

df_md(fit, contrast)

## -----------------------------------------------------------------------------
fit <- mmrm(
  formula = FEV1 ~ RACE + ARMCD * AVISIT + us(AVISIT | USUBJID),
  data = fev_data
)

if (require(emmeans)) {
  emmeans(fit, ~ ARMCD | AVISIT)
}

