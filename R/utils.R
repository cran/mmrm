#' Capture all Output
#'
#' This function silences all warnings, errors & messages and instead returns a list
#' containing the results (if it didn't error), as well as the warnings, errors
#' and messages and divergence signals as character vectors.
#'
#' @param expr (`expression`)\cr to be executed.
#' @param remove (`list`)\cr optional list with elements `warnings`, `errors`,
#'   `messages` which can be character vectors, which will be removed from the
#'   results if specified.
#' @param divergence (`list`)\cr optional list similar as `remove`, but these
#'   character vectors will be moved to the `divergence` result and signal
#'   that the fit did not converge.
#'
#' @return
#' A list containing
#'
#' - `result`: The object returned by `expr` or `list()` if an error was thrown.
#' - `warnings`: `NULL` or a character vector if warnings were thrown.
#' - `errors`: `NULL` or a string if an error was thrown.
#' - `messages`: `NULL` or a character vector if messages were produced.
#' - `divergence`: `NULL` or a character vector if divergence messages were caught.
#'
#' @keywords internal
h_record_all_output <- function(expr,
                                remove = list(),
                                divergence = list()) {
  # Note: We don't need to and cannot assert `expr` here.
  assert_list(remove, types = "character")
  assert_list(divergence, types = "character")
  env <- new.env()
  result <- withCallingHandlers(
    withRestarts(
      expr,
      muffleStop = function(e) structure(e$message, class = "try-error")
    ),
    message = function(m) {
      msg_without_newline <- gsub(m$message, pattern = "\n$", replacement = "")
      env$message <- c(env$message, msg_without_newline)
      invokeRestart("muffleMessage")
    },
    warning = function(w) {
      env$warning <- c(env$warning, w$message)
      invokeRestart("muffleWarning")
    },
    error = function(e) {
      env$error <- c(env$error, e$message)
      invokeRestart("muffleStop", e)
    }
  )
  list(
    result = result,
    warnings = setdiff(env$warning, c(remove$warnings, divergence$warnings)),
    errors = setdiff(env$error, c(remove$errors, divergence$errors)),
    messages = setdiff(env$message, c(remove$messages, divergence$messages)),
    divergence = c(
      intersect(env$warning, divergence$warnings),
      intersect(env$error, divergence$errors),
      intersect(env$message, divergence$messages)
    )
  )
}

#' Trace of a Matrix
#'
#' @description Obtain the trace of a matrix if the matrix is diagonal, otherwise raise an error.
#'
#' @param x (`matrix`)\cr square matrix input.
#'
#' @return The trace of the square matrix.
#'
#' @keywords internal
h_tr <- function(x) {
  if (nrow(x) != ncol(x)) {
    stop("x must be square matrix")
  }
  sum(Matrix::diag(x))
}

#' Split Control List
#'
#' @description Split the [mmrm_control()] object according to its optimizers and use additional arguments
#' to replace the elements in the original object.
#'
#' @param control (`mmrm_control`)\cr object.
#' @param ... additional parameters to update the `control` object.
#'
#' @return A `list` of `mmrm_control` entries.
#' @keywords internal
h_split_control <- function(control, ...) {
  assert_class(control, "mmrm_control")
  l <- length(control$optimizers)
  lapply(seq_len(l), function(i) {
    ret <- utils::modifyList(control, list(...))
    ret$optimizers <- control$optimizers[i]
    ret
  })
}

#' Obtain Optimizer according to Optimizer String Value
#'
#' @description This function creates optimizer functions with arguments.
#'
#' @param optimizer (`character`)\cr names of built-in optimizers to try, subset
#'   of "L-BFGS-B", "BFGS", "CG" and "nlminb".
#' @param optimizer_fun (`function` or `list` of `function`)\cr alternatively to `optimizer`,
#'   an optimizer function or a list of optimizer functions can be passed directly here.
#' @param optimizer_args (`list`)\cr additional arguments for `optimizer_fun`.
#' @param optimizer_control (`list`)\cr passed to argument `control` in `optimizer_fun`.
#'
#' @details
#' If you want to use only the built-in optimizers:
#' - `optimizer` is a shortcut to create a list of built-in optimizer functions
#'   passed to `optimizer_fun`.
#' - Allowed are "L-BFGS-B", "BFGS", "CG" (using [stats::optim()] with corresponding method)
#'   and "nlminb" (using [stats::nlminb()]).
#' - Other arguments should go into `optimizer_args`.
#'
#' If you want to use your own optimizer function:
#' - Make sure that there are three arguments: parameter (start value), objective function
#'   and gradient function are sequentially in the function arguments.
#' - If there are other named arguments in front of these, make sure they are correctly
#'   specified through `optimizer_args`.
#' - If the hessian can be used, please make sure its argument name is `hessian` and
#'   please add attribute `use_hessian = TRUE` to the function,
#'   using `attr(fun, "use_hessian) <- TRUE`.
#'
#' @return Named `list` of optimizers created by [h_partial_fun_args()].
#'
#' @keywords internal
h_get_optimizers <- function(optimizer = c("L-BFGS-B", "BFGS", "CG", "nlminb"),
                             optimizer_fun = h_optimizer_fun(optimizer),
                             optimizer_args = list(),
                             optimizer_control = list()) {
  if ("automatic" %in% optimizer) {
    lifecycle::deprecate_warn(
      when = "0.2.0",
      what = I("\"automatic\" optimizer"),
      details = "please just omit optimizer argument"
    )
    optimizer_fun <- h_optimizer_fun()
  }
  assert(
    test_function(optimizer_fun),
    test_list(optimizer_fun, types = "function", names = "unique")
  )
  if (is.function(optimizer_fun)) {
    optimizer_fun <- list(custom_optimizer = optimizer_fun)
  }
  lapply(optimizer_fun, function(x) {
    do.call(h_partial_fun_args, c(list(fun = x, control = optimizer_control), optimizer_args))
  })
}

#' Obtain Optimizer Function with Character
#' @description Obtain the optimizer function through the character provided.
#' @param optimizer (`character`)\cr vector of optimizers.
#'
#' @return A (`list`)\cr of optimizer functions generated from [h_partial_fun_args()].
#' @keywords internal
h_optimizer_fun <- function(optimizer = c("L-BFGS-B", "BFGS", "CG", "nlminb")) {
  optimizer <- match.arg(optimizer, several.ok = TRUE)
  lapply(stats::setNames(optimizer, optimizer), function(x) {
    switch(x,
      "L-BFGS-B" = h_partial_fun_args(fun = stats::optim, method = x),
      "BFGS" = h_partial_fun_args(fun = stats::optim, method = x),
      "CG" = h_partial_fun_args(fun = stats::optim, method = x),
      "nlminb" = h_partial_fun_args(fun = stats::nlminb, additional_attr = list(use_hessian = TRUE))
    )
  })
}

#' Create Partial Functions
#' @description Creates partial functions with arguments.
#'
#' @param fun (`function`)\cr to be wrapped.
#' @param ... Additional arguments for `fun`.
#' @param additional_attr (`list`)\cr of additional attributes to apply to the result.
#'
#' @details This function add `args` attribute to the original function,
#' and add an extra class `partial` to the function.
#' `args` is the argument for the function, and elements in `...` will override the existing
#' arguments in attribute `args`. `additional_attr` will override the existing attributes.
#'
#' @return Object with S3 class `"partial"`, a `function` with `args` attribute (and possibly more
#' attributes from `additional_attr`).
#' @keywords internal
h_partial_fun_args <- function(fun, ..., additional_attr = list()) {
  assert_function(fun)
  assert_list(additional_attr, names = "unique")
  a_args <- list(...)
  assert_list(a_args, names = "unique")
  args <- attr(fun, "args")
  if (is.null(args)) {
    args <- list()
  }
  do.call(
    structure,
    args = utils::modifyList(
      list(
        .Data = fun,
        args = utils::modifyList(args, a_args),
        class = c("partial", "function")
      ),
      additional_attr
    )
  )
}

#' Obtain Default Covariance Method
#'
#' @description Obtain the default covariance method depending on
#' the degrees of freedom method used.
#'
#' @param method (`string`)\cr degrees of freedom method.
#'
#' @details The default covariance method is different for different degrees of freedom method.
#' For "Satterthwaite" or "Between-Within", "Asymptotic" is returned.
#' For "Kenward-Roger" only, "Kenward-Roger" is returned.
#' For "Residual" only, "Empirical" is returned.
#'
#' @return String of the default covariance method.
#' @keywords internal
h_get_cov_default <- function(method = c("Satterthwaite", "Kenward-Roger", "Residual", "Between-Within")) {
  assert_string(method)
  method <- match.arg(method)
  switch(method,
    "Residual" = "Empirical",
    "Satterthwaite" = "Asymptotic",
    "Kenward-Roger" = "Kenward-Roger",
    "Between-Within" = "Asymptotic"
  )
}

#' Complete `character` Vector Names From Values
#'
#' @param x (`character` or `list`)\cr value whose names should be completed
#'   from element values.
#'
#' @return A named vector or list.
#'
#' @keywords internal
fill_names <- function(x) {
  n <- names(x)
  is_unnamed <- if (is.null(n)) rep_len(TRUE, length(x)) else n == ""
  names(x)[is_unnamed] <- x[is_unnamed]
  x
}

#' Drop Items from an Indexible
#'
#' Drop elements from an indexible object (`vector`, `list`, etc.).
#'
#' @param x Any object that can be consumed by [seq_along()] and indexed by a
#'   logical vector of the same length.
#' @param n (`integer`)\cr the number of terms to drop.
#'
#' @return A subset of `x`.
#'
#' @keywords internal
drop_elements <- function(x, n) {
  x[seq_along(x) > n]
}

#' Ask for Confirmation on Large Visit Levels
#'
#' @description Ask the user for confirmation if there are too many visit levels
#' for non-spatial covariance structure in interactive sessions.
#'
#' @param x (`numeric`)\cr number of visit levels.
#'
#' @return Logical value `TRUE`.
#' @keywords internal
h_confirm_large_levels <- function(x) {
  assert_count(x)
  allowed_lvls <- x <= getOption("mmrm.max_visits", 100)
  if (allowed_lvls) {
    return(TRUE)
  }
  if (!interactive()) {
    stop("Visit levels too large!", call. = FALSE)
  }
  proceed <- utils::askYesNo(
    paste(
      "Visit levels is possibly too large.",
      "This requires large memory. Are you sure to continue?",
      collapse = " "
    )
  )
  if (!identical(proceed, TRUE)) {
    stop("Visit levels too large!", call. = FALSE)
  }
  return(TRUE)
}

#' Default Value on NULL
#' Return default value when first argument is NULL.
#'
#' @param x Object.
#' @param y Object.
#'
#' @details If `x` is NULL, returns `y`. Otherwise return `x`.
#'
#' @keywords internal
h_default_value <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}

#' Warn on na.action
#' @keywords internal
h_warn_na_action <- function() {
  if (!identical(getOption("na.action"), "na.omit")) {
    warning("na.action is always set to `na.omit` for `mmrm` fit!")
  }
}

#' Obtain `na.action` as Function
#' @keywords internal
h_get_na_action <- function(na_action) {
  if (is.function(na_action) && identical(methods::formalArgs(na_action), c("object", "..."))) {
    return(na_action)
  }
  if (is.character(na_action) && length(na_action) == 1L) {
    assert_subset(na_action, c("na.omit", "na.exclude", "na.fail", "na.pass", "na.contiguous"))
    return(get(na_action, mode = "function", pos = "package:stats"))
  }
}

#' Validate mmrm Formula
#' @param formula (`formula`)\cr to check.
#'
#' @details In mmrm models, `.` is not allowed as it introduces ambiguity of covariates
#' to be used, so it is not allowed to be in formula.
#'
#' @keywords internal
h_valid_formula <- function(formula) {
  assert_formula(formula)
  if ("." %in% all.vars(formula)) {
    stop("`.` is not allowed in mmrm models!")
  }
}

#' Standard Starting Value
#'
#' @description Obtain standard start values.
#'
#' @param cov_type (`string`)\cr name of the covariance structure.
#' @param n_visits (`int`)\cr number of visits.
#' @param n_groups (`int`)\cr number of groups.
#' @param ... not used.
#'
#' @details
#' `std_start` will try to provide variance parameter from identity matrix.
#' However, for `ar1` and `ar1h` the corresponding values are not ideal because the
#' \eqn{\rho} is usually a positive number thus using 0 as starting value can lead to
#' incorrect optimization result, and we use 0.5 as the initial value of \eqn{\rho}.
#'
#' @return A numeric vector of starting values.
#'
#' @export
std_start <- function(cov_type, n_visits, n_groups, ...) {
  assert_string(cov_type)
  assert_subset(cov_type, cov_types(c("abbr", "habbr")))
  assert_int(n_visits, lower = 1L)
  assert_int(n_groups, lower = 1L)
  start_value <- switch(cov_type,
    us = rep(0, n_visits * (n_visits + 1) / 2),
    toep = rep(0, n_visits),
    toeph = rep(0, 2 * n_visits - 1),
    ar1 = c(0, 0.5),
    ar1h = c(rep(0, n_visits), 0.5),
    ad = rep(0, n_visits),
    adh = rep(0, 2 * n_visits - 1),
    cs = rep(0, 2),
    csh = rep(0, n_visits + 1),
    sp_exp = rep(0, 2)
  )
  rep(start_value, n_groups)
}

#' Empirical Starting Value
#'
#' @description Obtain empirical start value for unstructured covariance
#'
#' @param data (`data.frame`)\cr data used for model fitting.
#' @param model_formula (`formula`)\cr the formula in mmrm model without covariance structure part.
#' @param visit_var (`string`)\cr visit variable.
#' @param subject_var (`string`)\cr subject id variable.
#' @param subject_groups (`factor`)\cr subject group assignment.
#' @param ... not used.
#'
#' @details
#' This `emp_start` only works for unstructured covariance structure.
#' It uses linear regression to first obtain the coefficients and use the residuals
#' to obtain the empirical variance-covariance, and it is then used to obtain the
#' starting values.
#'
#' @note `data` is used instead of `full_frame` because `full_frame` is already
#' transformed if model contains transformations, e.g. `log(FEV1) ~ exp(FEV1_BL)` will
#' drop `FEV1` and `FEV1_BL` but add `log(FEV1)` and `exp(FEV1_BL)` in `full_frame`.
#'
#' @return A numeric vector of starting values.
#'
#' @export
emp_start <- function(data, model_formula, visit_var, subject_var, subject_groups, ...) {
  assert_formula(model_formula)
  assert_data_frame(data)
  assert_subset(all.vars(model_formula), colnames(data))
  assert_string(visit_var)
  assert_string(subject_var)
  assert_factor(data[[visit_var]])
  n_visits <- length(levels(data[[visit_var]]))
  assert_factor(data[[subject_var]])
  subjects <- droplevels(data[[subject_var]])
  n_subjects <- length(levels(subjects))
  fit <- stats::lm(formula = model_formula, data = data)
  res <- rep(NA, n_subjects * n_visits)
  res[
    n_visits * as.integer(subjects) - n_visits + as.integer(data[[visit_var]])
  ] <- residuals(fit)
  res_mat <- matrix(res, ncol = n_visits, nrow = n_subjects, byrow = TRUE)
  emp_covs <- lapply(
    unname(split(seq_len(n_subjects), subject_groups)),
    function(x) {
      stats::cov(res_mat[x, , drop = FALSE], use = "pairwise.complete.obs")
    }
  )
  unlist(lapply(emp_covs, h_get_theta_from_cov))
}
#' Obtain Theta from Covariance Matrix
#'
#' @description Obtain unstructured theta from covariance matrix.
#'
#' @param covariance (`matrix`) of covariance matrix values.
#'
#' @details
#' If the covariance matrix has `NA` in some of the elements, they will be replaced by
#' 0 (non-diagonal) and 1 (diagonal). This ensures that the matrix is positive definite.
#'
#' @return Numeric vector of the theta values.
#' @keywords internal
h_get_theta_from_cov <- function(covariance) {
  assert_matrix(covariance, mode = "numeric", ncols = nrow(covariance))
  covariance[is.na(covariance)] <- 0
  diag(covariance)[diag(covariance) == 0] <- 1
  # empirical is not always positive definite in some special cases of numeric singularity.
  qr_res <- qr(covariance)
  if (qr_res$rank < ncol(covariance)) {
    covariance <- Matrix::nearPD(covariance)$mat
  }
  emp_chol <- t(chol(covariance))
  mat <- t(solve(diag(diag(emp_chol)), emp_chol))
  ret <- c(log(diag(emp_chol)), mat[upper.tri(mat)])
  unname(ret)
}

#' Register S3 Method
#' Register S3 method to a generic.
#'
#' @param pkg (`string`) name of the package name.
#' @param generic (`string`) name of the generic.
#' @param class (`string`) class name the function want to dispatch.
#' @param envir (`environment`) the location the method is defined.
#'
#' @details This function is adapted from `emmeans:::register_s3_method()`.
#'
#' @keywords internal
h_register_s3 <- function(pkg, generic, class, envir = parent.frame()) {
  assert_string(pkg)
  assert_string(generic)
  assert_string(class)
  assert_environment(envir)
  fun <- get(paste0(generic, ".", class), envir = envir)
  if (isNamespaceLoaded(pkg)) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }
  setHook(packageEvent(pkg, "onLoad"), function(...) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  })
}

#' Check if a Factor Should Drop Levels
#'
#' @param x (`vector`) vector to check.
#'
#' @keywords internal
h_extra_levels <- function(x) {
  is.factor(x) && length(levels(x)) > length(unique(x))
}

#' Drop Levels from Dataset
#' @param data (`data.frame`) data to drop levels.
#' @param subject_var (`character`) subject variable.
#' @param visit_var (`character`) visit variable.
#' @param except (`character`) variables to exclude from dropping.
#' @keywords internal
h_drop_levels <- function(data, subject_var, visit_var, except) {
  assert_data_frame(data)
  assert_character(subject_var)
  assert_character(visit_var)
  assert_character(except, null.ok = TRUE)
  all_cols <- colnames(data)
  to_drop <- vapply(
    data,
    h_extra_levels,
    logical(1L)
  )
  to_drop <- all_cols[to_drop]
  # only drop levels for those not defined in excep and not in visit_var.
  to_drop <- setdiff(to_drop, c(visit_var, except))
  data[to_drop] <- lapply(data[to_drop], droplevels)
  # subject var are always dropped and no message given.
  dropped <- setdiff(to_drop, subject_var)
  if (length(dropped) > 0) {
    message(
      "Some factor levels are dropped due to singular design matrix: ",
      toString(dropped)
    )
  }
  data
}

#' Predicate if the TMB Version Used to Compile the Package is Sufficient
#'
#' @return Flag whether the TMB version is sufficient.
#' @keywords internal
h_tmb_version_sufficient <- function() {
  # Note: There is no version information saved in the dynamic library, but
  # we can check like this:
  tmb_config <- TMB::config(DLL = "mmrm")
  tape_deterministic <- tmb_config$tmbad_deterministic_hash
  !is.null(tape_deterministic)
}

#' Warn if TMB is Configured to Use Non-Deterministic Hash for Tape Optimizer
#'
#' This function checks the TMB configuration for the `tmbad_deterministic_hash` setting
#' If it is set to `FALSE`, a warning is issued indicating that this may lead to
#' unreproducible results.
#'
#' @return No return value, called for side effects.
#' @keywords internal
h_tmb_warn_non_deterministic <- function() {
  if (!h_tmb_version_sufficient()) {
    return()
  }
  tmb_config <- TMB::config(DLL = "mmrm")
  tape_deterministic <- tmb_config$tmbad_deterministic_hash
  if (!tape_deterministic) {
    msg <- paste(
      "TMB is configured to use a non-deterministic hash for its tape optimizer,",
      "and this may lead to unreproducible results.",
      "To disable this behavior, use `TMB::config(tmbad_deterministic_hash = 1)`.",
      sep = "\n"
    )
    warning(msg)
  }
}
