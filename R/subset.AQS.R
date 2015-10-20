subset.AQS <- function(x, criteria, start.date, end.date, ...) {
  
  err <- "No records remain. Select less restrictive subsetting criteria."
  
  # Remove readings and samples before start.date
  if(!missing(start.date)) {
    
    start.date <- as.Date(start.date)
    x$samples <- x$samples[x$samples$sample.date >= start.date, ]
    x$readings <- x$readings[x$readings$sample.date >= start.date, ]
    if(nrow(x$readings) == 0) stop(err)
    
  }
  
  # Remove readings and samples after end.date
  if(!missing(end.date)) {
    
    end.date <- as.Date(end.date)
    x$samples <- x$samples[x$samples$sample.date <= end.date, ]
    x$readings <- x$readings[x$readings$sample.date <= end.date, ]
    if(nrow(x$readings) == 0) stop(err)
    
  }
  
  change = TRUE
  
  if(!missing(criteria)) {
    
    while(change) {
      
      change = FALSE
      
      read.count <- nrow(x$readings)
      mon.count <- nrow(x$monitors)
      samp.count <- nrow(x$samples)
      
      # Creates lists of criteria that are contained in lookup tables, readings
      # table, monitors table, and samples table
      luc <- grep("name|code", names(criteria))
      names(luc) <- names(criteria)[luc]
      rc <- grep(paste(names(criteria), collapse="|"), colnames(x$readings))
      names(rc) <- colnames(x$readings)[rc]
      mc <- grep(paste(names(criteria), collapse="|"), colnames(x$monitors))
      names(mc) <- colnames(x$monitors)[mc]
      sc <- grep(paste(names(criteria), collapse="|"), colnames(x$samples))
      names(sc) <- colnames(x$samples)[sc]
      
      # Loops through criteria matching lookup tables (if any) and removes records
      # from lookup tables that don't match
      if(length(luc) > 0) {
        for(i in luc) {
          
          # Look up type, either 'code' or 'name'
          lut <- substring(names(criteria)[i], nchar(names(criteria)[i]) - 3)
          # Look up name, the name of the table in the lookup table list
          lun <- substr(names(criteria)[i], 1, nchar(names(criteria)[i]) - 5)
          # Does the actual subsetting
          x$lu[[lun]] <- x$lu[[lun]][grep(criteria[[i]], x$lu[[lun]][[lut]]), ]
          # Stops with an error if a table is completely subsetted away
          if(nrow(x$lu[[lun]]) == 0) stop(err)
          
        }
      }
      
      # After subsetting the lookup tables is complete, then cycles through the 
      # lookup tables and removes rows from the three main tables that no longer 
      # have matches in the lookup tables. Special care has to be taken for state
      # and county because those codes reside as part of the 'monitor.id' column.
      
      if(nrow(x$lu$state) > 0) {
        re <- paste(x$lu$state$code, collapse="|")
        x$monitors <- x$monitors[grep(re, substr(x$monitors$monitor.id, 1, 2)), ]
        x$samples <- x$samples[grep(re, substr(x$samples$monitor.id, 1, 2)), ]
        x$readings <- x$readings[grep(re, substr(x$readings$monitor.id, 1, 2)), ]
      }
      if(nrow(x$lu$county) > 0) {
        re <- paste(x$lu$county$code, collapse="|")
        x$monitors <- x$monitors[grep(re, substr(x$monitors$monitor.id, 4, 6)), ]
        x$samples <- x$samples[grep(re, substr(x$samples$monitor.id, 4, 6)), ]
        x$readings <- x$readings[grep(re, substr(x$readings$monitor.id, 4, 6)), ]      
      }
      
      # Another check for empty tables
      if(nrow(x$monitors) == 0 | nrow(x$samples) == 0 | nrow(x$readings) == 0) {
        stop(err)
      }
      
      for(n in names(x$lu)) {
        
        if(nrow(x$lu[[n]]) > 0) {
          
          # Builds the regular expression for checking by pasting names together
          # and collapsing on '|'
          re <- paste(x$lu[[n]][, "code"], collapse="|")
          # Builds the code column name for convienience
          cn <- paste(n, "code", sep=".")
          # Checks each of the three main tables for a matching column name and
          # subsets on that column if it finds one.
          if(length(grep(cn, colnames(x$monitors))) > 0) {
            x$monitors <- x$monitors[grep(re, x$monitors[, cn]), ]
          }
          if(length(grep(cn, colnames(x$samples))) > 0) {
            x$samples <- x$samples[grep(re, x$samples[, cn]), ]
          }
          if(length(grep(cn, colnames(x$readings))) > 0) {
            x$readings <- x$readings[grep(re, x$readings[, cn]), ]
          }
          # Another check for empty tables
          if(nrow(x$monitors) == 0 | nrow(x$samples) == 0 | nrow(x$readings) == 0) {
            stop(err)
          }
          
        }
        
      }
      
      # Subsets on criteria matching monitors table column names
      if(length(mc) > 0) {
        for(i in seq(length(mc))) {
          
          x$monitors <- x$monitors[grep(criteria[names(mc)[i]], x$monitors[, mc[[i]]]), ]
          
        }
      }
      
      # Subsets on criteria matching samples table column names
      if(length(sc) > 0) {
        for(i in seq(length(sc))) {
          
          x$samples <- x$samples[grep(criteria[names(sc)[i]], x$samples[, sc[i]]), ]
          
        }
      }
      
      # Subsets on criteria matching readings table column names
      if(length(rc) > 0) {
        for(i in seq(length(rc))) {
          
          x$readings <- x$readings[grep(criteria[names(rc)[i]], x$readings[, rc[i]]), ]
          
        }
      }
      
      # Does a comparison of 'readings' and 'samples' table based on monitor.id,
      # parameter.code, and sample.date, subsets accordingly
      read.list <- interaction(list(x$readings$monitor.id, x$readings$parameter.code, x$readings$sample.date))
      samp.list <- interaction(list(x$samples$monitor.id, x$samples$parameter.code, x$samples$sample.date))
      temp <- intersect(read.list, samp.list)
      
      x$readings <- x$readings[read.list %in% temp, ]
      x$samples <- x$samples[samp.list %in% temp, ]
      
      # Creates a vector of monitor.id values that are shared by all three main
      # tables, then subsets main tables based on that list.
      mon.list <- Reduce(intersect, list(x$readings$monitor.id, 
                                         x$monitors$monitor.id, 
                                         x$samples$monitor.id))
      
      x$monitors <- x$monitors[x$monitors$monitor.id %in% mon.list, ]
      x$readings <- x$readings[x$readings$monitor.id %in% mon.list, ]
      x$samples <- x$samples[x$samples$monitor.id %in% mon.list, ]
      
      # Another empty table check
      if(nrow(x$monitors) == 0 | nrow(x$samples) == 0 | nrow(x$readings) == 0) {
        stop(err)
      }
      
      # Rebuilds the look up tables based on values still contained in the main
      # tables
      x$lu$state <- x$lu$state[x$lu$state$code %in% as.integer(unique(substr(x$monitors$monitor.id, 1, 2))), ]
      x$lu$county <- x$lu$county[x$lu$county$code %in% as.integer(unique(substr(x$monitors$monitor.id, 4, 6))), ]
      
      m.lu <- colnames(x$monitors)[grep("code", colnames(x$monitors))]
      m.lu <- substr(m.lu, 1, nchar(m.lu)-5)
      
      for(i in m.lu) {
        
        x$lu[[i]] <- x$lu[[i]][x$lu[[i]][, "code"] %in% as.integer(unique(x$monitors[[paste(i, "code", sep=".")]])), ]
        
      }
      
      s.lu <- colnames(x$samples)[grep("code", colnames(x$samples))]
      s.lu <- substr(s.lu, 1, nchar(s.lu)-5)
      
      for(i in s.lu) {
        
        x$lu[[i]] <- x$lu[[i]][x$lu[[i]][, "code"] %in% as.integer(unique(x$samples[[paste(i, "code", sep=".")]])), ]
        
      }
      
      r.lu <- colnames(x$readings)[grep("code", colnames(x$readings))]
      r.lu <- substr(r.lu, 1, nchar(r.lu)-5)
      
      for(i in r.lu) {
        
        x$lu[[i]] <- x$lu[[i]][x$lu[[i]][, "code"] %in% as.integer(unique(x$readings[[paste(i, "code", sep=".")]])), ]
        
      }
      
      if(nrow(x$readings) != read.count | 
           nrow(x$samples) != samp.count |
           nrow(x$monitors) != mon.count) {
        change = TRUE
      }
      print(change)
    }
    
  }
  
  x

}