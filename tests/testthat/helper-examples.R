.tmb_formula <- FEV1 ~ RACE + us(AVISIT | USUBJID)
.mmrm_tmb_example <- fit_mmrm(.tmb_formula, fev_data, weights = rep(1, nrow(fev_data)))
get_mmrm_tmb <- function() {
  .mmrm_tmb_example
}

.tmb_formula_rank_deficient <- FEV1 ~ SEX + SEX2 + us(AVISIT | USUBJID)
.mmrm_tmb_dat_rank_deficient <- cbind(fev_data, SEX2 = fev_data$SEX) # nolint
.mmrm_tmb_example_rk_deficient <- fit_mmrm(
  .tmb_formula_rank_deficient,
  .mmrm_tmb_dat_rank_deficient,
  weights = rep(1, nrow(fev_data))
)
get_mmrm_tmb_rank_deficient <- function() {
  .mmrm_tmb_example_rk_deficient
}

.mmrm_formula <- FEV1 ~ RACE + SEX + ARMCD * AVISIT + us(AVISIT | USUBJID)
.mmrm_example <- mmrm(.mmrm_formula, fev_data)
get_mmrm <- function() {
  .mmrm_example
}

set.seed(123, kind = "Mersenne-Twister")
.mmrm_weights <- rpois(nrow(fev_data), lambda = 5) + 1
.mmrm_weighted_example <- mmrm(.mmrm_formula, fev_data, weights = .mmrm_weights)
get_mmrm_weighted <- function() {
  .mmrm_weighted_example
}

.mmrm_formula_rank_deficient <- FEV1 ~ RACE + SEX + SEX2 + ARMCD * AVISIT + us(AVISIT | USUBJID)
.mmrm_dat_rank_deficient <- cbind(fev_data, SEX2 = fev_data$SEX) # nolint
.mmrm_example_rank_deficient <- mmrm(
  .mmrm_formula_rank_deficient,
  .mmrm_dat_rank_deficient
)
get_mmrm_rank_deficient <- function() {
  .mmrm_example_rank_deficient
}

.mmrm_group_formula <- FEV1 ~ ARMCD + us(AVISIT | ARMCD / USUBJID)
.mmrm_grouped <- mmrm(.mmrm_group_formula, data = fev_data)
get_mmrm_group <- function() {
  .mmrm_grouped
}

.mmrm_spatial_formula <- FEV1 ~ ARMCD + sp_exp(VISITN | USUBJID)
.mmrm_spatial <- mmrm(.mmrm_spatial_formula, data = fev_data)
get_mmrm_spatial <- function() {
  .mmrm_spatial
}

square_matrix <- function(values_by_row) {
  n <- length(values_by_row)
  size <- sqrt(n)
  assert_integerish(size)
  size <- floor(size)
  matrix(data = values_by_row, nrow = size, ncol = size, byrow = TRUE)
}

map_to_cor <- function(theta) {
  theta / sqrt(1 + theta^2)
}

map_to_theta <- function(rho) {
  sign(rho) * sqrt(rho^2 / (1 - rho^2))
}