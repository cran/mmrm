# This dummy function definition is included with the package to ensure that
# 'tools::package_native_routine_registration_skeleton()' generates the required
# registration info for the 'run_testthat_tests' symbol.
# Important: The function does not take any arguments, please adjust the
# auto-generated content accordingly!
(function() {
  .Call("run_testthat_tests", FALSE, PACKAGE = "mmrm")
})
