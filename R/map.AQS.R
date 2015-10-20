map <- function(database = "world", regions = ".", exact = FALSE, boundary = TRUE,
                interior = TRUE, projection = "", parameters = NULL, orientation = NULL,
                fill = FALSE, col = 1, plot = TRUE, add = FALSE, namesonly = FALSE,
                xlim = NULL, ylim = NULL, wrap = FALSE, resolution = if(plot) 1 else 0,
                type = "l", bg = par("bg"), mar = c(4.1, 4.1, par("mar")[3], 0.1),
                myborder = 0.01, ...){
  UseMethod("map")
}

map.default <- maps::map

# Creates a map of monitor locataions in the AMP350 database
map.AQS <- function(database, ...) {
  
  if(class(database) != "AQS") stop("must provide AQS database")
  mapNorthAmerica <- function() {
    
    x <- map("world", xlim=c(-170,-60), ylim=c(15,72))
    map("state", col="darkgrey", boundary=FALSE, add=TRUE)
    box()
    map.scale(x=-168, y=30, relwidth=0.2, metric=FALSE)
    map.axes()
    
  }
  
  mapContUs <- function(...) {
    
    x <- map("state", ...)
    box()
    map.scale(x=-121, y=27, relwidth=0.2, metric=FALSE)
    map.axes()
    
  }
  
  mapCounties <- function(...) {
    
    x <- map("county", ...)
    box()
    map.axes()
    
  }
  
  d <- database$monitors[, c("monitor.id", "longitude", "latitude")]
  d$longitude <- as.numeric(d$longitude)
  d$latitude <- as.numeric(d$latitude)
  d <- unique(d)
  d <- d[is.numeric(d$longitude) & is.numeric(d$latitude), ]
  d <- d[!is.na(d$longitude) & !is.na(d$latitude), ]
  w <- grep("[Aa]laska|[Hh]awaii", 
            unique(map.where("world", d$longitude, d$latitude)))
  s <- unique(map.where("state", d$longitude, d$latitude))
  if(length(w) > 0) {
    mapNorthAmerica()
  } else if(length(s) == 1) {
    mapCounties(regions=s)
  } else {
    mapContUs()
  }
  
  points(d$longitude, d$latitude, 
         bg=rgb(61, 184, 104, maxColorValue=255), 
         col=rgb(0, 0, 0, maxColorValue=255),
         pch=21, lwd=1.5)
  
}