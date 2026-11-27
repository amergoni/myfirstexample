#' Classify Body Mass Index (BMI)
#'
#' Computes the body mass index from weight and height and classifies it
#' into the four WHO categories.
#'
#' @param weight Weight in kilograms (numeric vector).
#' @param height Height in metres (numeric vector, same length as `weight`).
#' @return A factor with levels "underweight", "normal", "overweight"
#'   and "obese".
#' @examples
#' bmi_category(weight = c(50, 70, 85, 110),
#'              height = c(1.70, 1.75, 1.80, 1.75))
#' @export
bmi_category <- function(weight, height) {
  stopifnot(is.numeric(weight), is.numeric(height))
  bmi <- weight / height^2
  cut(bmi,
      breaks = c(-Inf, 18.5, 25, 30, Inf),
      labels = c("underweight", "normal", "overweight", "obese"),
      right  = FALSE)
}
