# Internal helper for x-axis text alignment in SCF plots.
.scf_axis_text_x <- function(angle) {
  is_zero_angle <- isTRUE(all.equal(angle, 0))
  
  ggplot2::element_text(
    angle = angle,
    hjust = if (is_zero_angle) 0.5 else 1,
    vjust = if (is_zero_angle) 0.5 else 1
  )
}