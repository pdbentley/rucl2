as.data.frame.AQS <- function(x, ..., columns) {
  
  df <- x$readings[, c("monitor.id", "parameter.code", "sample.date")]
      
  if(missing(columns)) {
    
    columns <- c("monitor.id", "parameter.name", "sample.date", "hour", "value", "mdl")
    
  }
    
  for(col in columns) {
    
    if(!(col %in% colnames(df))) {
    
      if(length(grep(col, colnames(x$readings))) > 0) {
        
        df <- cbind(df, x$readings[, col])
        colnames(df)[ncol(df)] <- col
        
      } else if(length(grep(col, colnames(x$monitors))) > 0) {
        
        df <- merge(df, x$monitors[, c("monitor.id", col)])
        
      } else if(length(grep(col, colnames(x$samples))) > 0) {
        
        df <- merge(df, x$samples[, c("monitor.id", "parameter.code", "sample.date", col)])
            
      } else if(length(lu <- grep(substr(col, 1, nchar(col) - 5), names(x$lu))) > 0) {
        
        cn <- paste(names(x$lu)[lu], "code", sep=".")
        tn <- grep(cn, list(colnames(x$readings),
                            colnames(x$monitors), 
                            colnames(x$samples)))[1]
        if(tn == 1) {
          df <- merge(df, x$lu[[lu]], by.x = cn, by.y = "code")
          colnames(df)[colnames(df) %in% c("name")] <- col
        } else if(tn == 2) {        
          y <- match(x$monitors[, cn], x$lu[[lu]][["code"]])
          tdf <- cbind(x$monitors["monitor.id"], x$lu[[lu]][y, "name"])
          colnames(tdf)[2] <- col
          df <- merge(df, tdf)
        } else if(tn == 3) {
          y <- merge(x$samples[, c("monitor.id", "parameter.code", "sample.date", cn)], x$lu[[lu]], by.x = cn, by.y = "code")
          y <- y[, !colnames(y) %in% cn]
          colnames(y)[colnames(y) %in% c("name")] <- col
          df <- merge(df, y)
        }
  
      }

    }
    
  }
  
  columns <- columns[columns %in% colnames(df)]
  
  df[, unlist(columns)]
    
}