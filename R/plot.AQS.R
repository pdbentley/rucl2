#Creates a boxplot of the readings in the AQS object using ggplot2
plot.AQS <- function(x, ...) {
  
  x$readings <- cbind(x$readings, 
                      parameter.name = x$lu$parameter$name[match(x$readings$parameter.code, 
                                                                 x$lu$parameter$code)])
    
  x$readings <- x$readings[!is.na(x$readings$value), ]
  qplot(x = monitor.id, y = value, data = x$readings, 
        facets=parameter.name~. , geom="boxplot", ylab="Concentration", xlab="Monitor ID", ...)
  
}
