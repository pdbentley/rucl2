# Custom print function for AQS objects
print.AQS <- function(x, ...) {
  
  cat("AQS OBJECT\n")
  cat("----------\n")
  cat("DATA SOURCE\n ", attr(x, "file"), "\n")
  cat("DATA SOURCE TYPE\n ", attr(x, "type"), "\n")
  cat("DATE RANGE\n ", 
      format(min(x$readings$sample.date, na.rm=TRUE), "%Y-%m-%d"),
      "through",
      format(max(x$readings$sample.date, na.rm=TRUE), "%Y-%m-%d"),
      "\n")
  cat("TOTAL READINGS\n ", nrow(x$readings), "\n")
  cat("MONITORS\n")
  cat(paste(" ", x$monitors$monitor.id), sep="\n")
  cat("PARAMETERS\n")
  cat(paste(" ", x$lu$parameter$name), sep="\n")
  
}