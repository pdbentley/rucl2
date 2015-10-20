#Creates a dataframe of UCLS for each monitor/parameter combo found in the AQS object
ucl.AQS <- function(x, type = "fast", ...) {
  
  r <- x$readings
  d <- split(r, list(r$monitor.id, r$parameter.code)) 
  
  calcs <- lapply(d, function(df) {
    df <- df[!is.na(df$value), ]
    n <- nrow(df)
    dr <- round(sum(df$detection)/n * 100, 1)
    if(n < 8 | dr < 25) {
      u <- rep(NA, times=length(type))
    } else if(dr >= 100) {
      u <- signif(ucl(df$value, type = type), 2)
    } else {
      v <- df$value
      v[!df$detection] = df$mdl[!df$detection]
      u <- signif(ucl(v, df$detection, type = type), 2)
    }
    return(c(n, dr, u))
  })
  a <- as.data.frame(do.call(rbind, calcs))
  s <- regexpr("\\.", rownames(a))
  mon <- substr(rownames(a), 1, s-1)
  pol.code <- substr(rownames(a), s+1,1000)
  pol.name <- x$parameter$name[match(pol.code, x$parameter$code)]
  
  a <- cbind(mon, pol.name, a)
  colnames(a)[1:4] <- c("monitor.id", "parameter", "n", "detect.rate")
  return(a)
  
}