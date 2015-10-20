read.AMP350 <- function(file) {

  con <- file(file, "rt")
  
  data <- readLines(con, n=4)
  data <- strsplit(data, "|", TRUE)
  header <- sapply(data, function(x) {tolower(gsub("[[:punct:]|[:blank:]]+",
                                                 ".", x))})
  header[[1]][8] <- "parameter.name"
  header[[2]][15] <- "pqao.code"
    
  data <- suppressWarnings(read.table(con, header=FALSE, sep="|", fill=TRUE, 
                                      comment.char="", stringsAsFactors=FALSE))
  
  close(con)
  
  #Initial Formatting of the monitors table
  monitors <- data[data[, 1] == 1, 1:length(header[[1]])]
  rownames(monitors) <- NULL
  colnames(monitors) <- header[[1]]
  monitors[, 1] <- sprintf("%02i-%03i-%04i-%i", 
                           as.integer(monitors$state.code), 
                           as.integer(monitors$county.code), 
                           as.integer(monitors$site.id), 
                           as.integer(monitors$poc))
  colnames(monitors)[1] <- "monitor.id"
  
  #initial formatting of the readings table
  readings <- data[data[, 1] == 2, 1:length(header[[2]])]
  rownames(readings) <- NULL
  colnames(readings) <- header[[2]]
  readings[, 1] <- sprintf("%02i-%03i-%04i-%i", 
                           as.integer(readings$state.code), 
                           as.integer(readings$county.code), 
                           as.integer(readings$site.id), 
                           as.integer(readings$poc))
  colnames(readings)[1] <- "monitor.id"
  sd <- paste(substr(readings$sample.date, 1, 4), 
              substr(readings$sample.date, 5, 6),
              substr(readings$sample.date, 7, 8),
              sep="-")
  readings$sample.date <- as.Date(sd, format = "%Y-%m-%d")
  
  rm(data)
    
  lu.names <- c("state", "county", "city", "parameter", "aqcr", "cbsa", "csa", "uar")
  lu.tables <- lapply(lu.names, function(x) {
    cn <- grep(x, colnames(monitors))
    u <- unique(monitors[, cn[1]])
    o <- monitors[match(u, monitors[, cn[1]]), cn]
    colnames(o) <- c("code", "name")
    o$code <- as.integer(o$code)
    rownames(o) <- NULL
    o
  })
  names(lu.tables) <- lu.names
  
  cn <- grep("pqao", colnames(readings))
  u <- unique(readings[, cn[1]])
  lu.tables[["pqao"]] <- readings[match(u, readings[, cn[1]]), cn]
  colnames(lu.tables[["pqao"]]) <- c("code", "name")
  rownames(lu.tables[["pqao"]]) <- NULL
  
  # Create final monitors table
  mon.col.miss <- c(2:9, 11, 14, 16, 18, 24)
  mon.col.class <-  c(as.factor, as.integer, as.character, rep(c(as.integer), 4),
                      rep(c(as.factor), 3), as.integer, as.factor, as.factor,
                      as.numeric, as.numeric, as.integer, as.numeric, as.numeric,
                      as.integer, as.factor, as.factor, as.numeric, as.numeric,
                      as.numeric, as.numeric, as.factor, as.factor, as.numeric)
  monu <- unique(monitors$monitor.id)
  monitors <- monitors[match(monu, monitors$monitor.id), -mon.col.miss]
  for(i in seq(length(mon.col.class))) monitors[, i] <- mon.col.class[[i]](monitors[, i])
  
  samples <- readings[, c(1, 5, 7:9, 11:15, 17)] 
  samp.col.class <- c(as.factor, as.integer, as.factor, as.integer, as.factor,
                      as.numeric, as.factor, as.factor, as.factor, as.integer,
                      as.Date)
  for(i in seq(length(samp.col.class))) samples[, i] <- samp.col.class[[i]](samples[, i])
  
  readings <- cbind(readings[, c("monitor.id", "parameter.code", 
                    "sample.date", "mdl")],
                    hour = matrix(sapply(seq(24), rep, times = nrow(readings)), ncol=1),
                    value = matrix(as.matrix(readings[, seq(18, ncol(readings), 2)]), ncol=1), 
                    qualifier = matrix(as.matrix(readings[, seq(19, ncol(readings), 2)]), ncol=1))
  readings$value <- as.numeric(as.character(readings$value))
  read.col.class <- c(as.factor, as.integer, as.Date, as.numeric, as.integer,
                      as.numeric, as.factor)
  for(i in seq(length(read.col.class))) readings[, i] <- read.col.class[[i]](readings[, i])
  readings$qualifier[readings$qualifier == ""] = NA
  readings <- readings[(!is.na(readings$value) | !is.na(readings$qualifier)), ]
  readings$qualifier[is.na(readings$qualifier)] = ""
  
  op <- list(lu = lu.tables, 
             monitors = monitors, 
             readings = readings, 
             samples = samples)
  attr(op, "file") <- file
  attr(op, "type") <- "AMP350"
  class(op) <- "AQS"
  op
  
}