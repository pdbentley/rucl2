summary.AQS <- function(object, ...) {
  
  src <- attr(object, "file")
  type <- attr(object, "type")

  dr <- is.na(object$readings$mdl)
  dr[!dr] <- object$readings$value[!dr] > object$readings$mdl[!dr]
  dr[is.na(object$readings$value)] = FALSE
  dr[is.na(dr)] <- FALSE
  
  object$readings <- cbind(object$readings, detection = dr)
  
  monitor.info <- as.data.frame(object$monitors[, c("monitor.id", "city.code", "street.address", "mon.obj1", "land.use")])
  
  state   <- object$lu$state$name[match(as.integer(substr(object$monitor$monitor.id, 1, 2)), object$lu$state$code)]
  county <- object$lu$county$name[match(as.integer(substr(object$monitor$monitor.id, 4, 6)), object$lu$county$code)]
  city   <- object$lu$city$name[match(object$monitor$city.code, object$lu$city$code)]
  
  monitor.info <- cbind(state, county, city, object$monitors[, c("street.address", "mon.obj1", "land.use")])
  rownames(monitor.info) <- object$monitors$monitor.id
  
  reading.counts <- matrix(1, nrow = nrow(object$monitors), 
                           ncol = nrow(object$lu$parameter),
                           dimnames = list(object$monitors$monitor.id, object$lu$parameter$code))
  start.dates <- matrix("1900-01-01", nrow = nrow(object$monitors), 
                        ncol = nrow(object$lu$parameter),
                        dimnames = list(object$monitors$monitor.id, object$lu$parameter$code))
  end.dates <- start.dates
  detection.rates <- reading.counts
  
  g <- expand.grid(object$monitors$monitor.id, object$lu$parameter$code)
  g[, 1] <- as.character(g[, 1])
  g[, 2] <- as.character(g[, 2])
  
  for(i in seq(nrow(g))) {
    
    x <- object$readings[object$readings$monitor.id == g[i,1] & object$readings$parameter.code == g[i,2], ]
    reading.counts[g[i,1], g[i,2]] <- nrow(x)
    start.dates[g[i,1], g[i, 2]] <- format(min(x$sample.date, na.rm=T), "%Y-%m-%d")
    end.dates[g[i,1], g[i, 2]] <- format(max(x$sample.date, na.rm=T), "%Y-%m-%d")
    detection.rates[g[i,1], g[i, 2]] <- signif(sum(x$detection, na.rm=T)/nrow(x),2)*100
    
  }
  
  cn <- object$lu$parameter$name[match(colnames(reading.counts), object$lu$parameter$code)]
  colnames(reading.counts) <- cn
  colnames(start.dates) <- cn
  colnames(end.dates) <- cn
  colnames(detection.rates) <- cn
  
  o <- list(src = src, type=type, monitor.info = monitor.info, 
            reading.counts = reading.counts, start.dates = start.dates, 
            end.dates = end.dates, detection.rates = detection.rates)
  
  class(o) <- "AQS.summary"
  o
  
}

print.AQS.summary <- function(x, ...) {
  
  cat("AQS Object Summary\n")
  cat("---------------------\n")
  cat(" WORKFILE TYPE\n", x$type, "\n\n")
  cat(" DATA SOURCE\n  ", x$src, "\n\n")
  cat(" MONITOR INFO\n")
  cat(" ------------\n")
  print(x$monitor.info)
  cat("\n")
  cat(" START DATES\n")
  cat(" -----------\n")
  print(x$start.date)
  cat("\n")
  cat(" END DATES\n")
  cat(" -----------\n")
  print(x$end.date)  
  cat("\n")
  cat(" READING COUNTS\n")
  cat(" --------------\n")
  print(x$reading.counts)
  cat("\n")
  cat(" DETECTION RATES (%)\n")
  cat(" -------------------\n")
  print(x$detection.rates)
  
}