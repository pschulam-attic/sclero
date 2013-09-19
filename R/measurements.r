lifetime_worst <- function(measurements, low = TRUE) {
  quant <- if (low) 0.1 else 0.9
  worst.set <- quantile(measurements, quant, na.rm = TRUE)
  worst <- median(worst.set)

  # median returns infinity if length(worst.set) == 0
  if (is.infinite(worst)) NA else worst
}
