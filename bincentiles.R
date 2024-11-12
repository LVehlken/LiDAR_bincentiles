# Define the function bincentile
bincentile <- function(Z, height_cut_off, bincentile) {
  # Filter points above the height cutoff
  points_above_cutoff <- Z[Z > height_cut_off]
  
  # Check if there are any points above the height cutoff
  if (length(points_above_cutoff) == 0) {
    return(NA)  # Return NA if no points meet the height cutoff
  }
  
  # Calculate zmax for points above the height cutoff
  zmax <- max(points_above_cutoff)
  
  # Calculate the target height h
  h <- bincentile * (zmax - height_cut_off) + height_cut_off
  
  # Calculate the percentage of points that are below h
  points_below_h <- points_above_cutoff[points_above_cutoff < h]
  percentage_below_h <- (length(points_below_h) / length(points_above_cutoff)) * 100
  
  return(percentage_below_h)
}