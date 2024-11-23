convert_wide_to_pairwise <- function (input) {
  if (length(input[1, ]) > 2) {
    long_form0 <- apply(input, 1, function(x) {combn(na.omit(x), 2)})
    long_form <- t(do.call(cbind, long_form0))
  } else {
    long_form <- input
  }
  
  return(long_form)
}