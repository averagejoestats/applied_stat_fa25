
# load the hurdat data
load("../tropical_cyclones/processed_data/storms_hurdat.RData")
tail(storms)

# use only storms after 1970-01-01
storms <- storms[ storms$start_date >= "1970-01-01", ]

# use maps package for plotting country boundaries
library("maps")

# make a plot of the data
par(mfrow=c(2,2))
plot( storms$start_lon, storms$start_lat, xlab = "Longitude", ylab = "Latitude",
      main = "Starting Locations of Tropical Cyclones (HURDAT)", pch = 1
)
map("world", add = TRUE)
plot( storms$min_pressure_lon, storms$min_pressure_lat, xlab = "Longitude", ylab = "Latitude",
      main = "Minimum Pressure Locations of Tropical Cyclones (HURDAT)", pch = 1
)
map("world", add = TRUE)
plot( storms$max_wind_lon, storms$max_wind_lat, xlab = "Longitude", ylab = "Latitude",
      main = "Max Wind Speed Locations of Tropical Cyclones (HURDAT)", pch = 1
)
map("world", add = TRUE)

#
# do a manual kernel density estimate on a latitude longitude grid
#

# define the grid box centers
lon_grid <- seq(-110, 0, by = 0.5 )
lat_grid <- seq(0, 65, by = 0.5 )
lonlat_grid <- expand.grid(lon = lon_grid, lat = lat_grid)

# calculate the approximate area of each grid box in square kilometers
# uses the approximation that 1 degree latitude = 111.11 km
# and 1 degree longitude = 111.11 * cos( latitude ) km
lon_width <- diff(lon_grid)[1]
lat_width <- diff(lat_grid)[1]
lonlat_grid$area <- ( 111.32*lat_width )*( 111.32*lon_width*cos( 2*pi*lonlat_grid$lat/360 ) )

# bandwidth in kilometers, convert lonlat grid to matrix, for use in rdist.earth
h <- 150  # 150 miles in kilometers
lonlat_grid_mat <- as.matrix( lonlat_grid[,c("lon","lat")] )

# initialize the kernel density estimate
lonlat_grid$kde_genesis <- 0
lonlat_grid$kde_max_wind <- 0

# loop over the storm starting locations and calculate the density
for(i in 1:nrow(storms)) {
    # get lonlat for this storm, calculate distances to all grid points
    this_storm_lonlat <- matrix( c(storms$start_lon[i], storms$start_lat[i]), nrow = 1 )
    dists <- fields::rdist.earth( lonlat_grid_mat, this_storm_lonlat, miles = FALSE )
    # add to the kernel density estimate
    lonlat_grid$kde_genesis <- lonlat_grid$kde_genesis + 1/(2*pi*h^2)*exp(-1/2*dists^2/h^2)

    # get lonlat for this storm, calculate distances to all grid points
    this_storm_lonlat <- matrix( c(storms$max_wind_lon[i], storms$max_wind_lat[i]),nrow = 1 )
    dists <- fields::rdist.earth( lonlat_grid_mat, this_storm_lonlat, miles = FALSE )
    # add to the kernel density estimate
    lonlat_grid$kde_max_wind <- lonlat_grid$kde_max_wind + 1/(2*pi*h^2)*exp(-1/2*dists^2/h^2)
}

# plot the kernel density estimate
par(mfrow=c(1,2))
fields::image.plot(
    lon_grid, lat_grid, matrix( lonlat_grid$kde_genesis, length(lon_grid), length(lat_grid) )
)
map("world", add = TRUE, col = "white" )
fields::image.plot(
    lon_grid, lat_grid, matrix( lonlat_grid$kde_max_wind,length(lon_grid), length(lat_grid) )
)
map("world", add = TRUE, col = "white" )

# check that we calculated the units correctly
sum( lonlat_grid$kde_genesis * lonlat_grid$area )
nrow(storms)

# 
# analyze data with the spatstat package
# 
library("spatstat")

# make point pattern objects
win <- owin( xrange = c(-110, 0), yrange = c(0, 65) )
start_ppp <- ppp(storms$start_lon, storms$start_lat, window = win )
min_pres_ppp <- ppp(storms$min_pressure_lon, storms$min_pressure_lat, window = win )
max_wind_ppp <- ppp(storms$max_wind_lon, storms$max_wind_lat, window = win )

# plot the point pattern objects
par(mfrow=c(2,2))
plot( start_ppp, main = "Starting Locations of Tropical Cyclones (HURDAT PP)" )
map("world", add = TRUE)
plot( min_pres_ppp, main = "Minimum Pressure Locations of Tropical Cyclones (HURDAT PP)" )
map("world", add = TRUE)
plot( max_wind_ppp, main = "Maximum Wind Locations of Tropical Cyclones (HURDAT PP)" )
map("world", add = TRUE)

# do kernel density estimation for different bandwidths
start_kde1 <- density( start_ppp, sigma = 1 )
start_kde2 <- density( start_ppp, sigma = 2 )
start_kde4 <- density( start_ppp, sigma = 4 )
start_kde8 <- density( start_ppp, sigma = 8 )

par(mfrow=c(2,2), mar = c(1,1,0,1))
plot( start_kde1, main = "KDE of Starting Locations (sigma=1)" )
map("world", add = TRUE, col = "white" )
plot( start_kde2, main = "KDE of Starting Locations (sigma=2)" )
map("world", add = TRUE, col = "white" )
plot( start_kde4, main = "KDE of Starting Locations (sigma=4)" )
map("world", add = TRUE, col = "white" )
plot( start_kde8, main = "KDE of Starting Locations (sigma=8)" )
map("world", add = TRUE, col = "white" )

# show kde for different point patterns with bandwidth = 2
par(mfrow=c(2,2), mar = c(0,0,0,0), oma = c(1,1,1,1))
start_kde2 <- density( start_ppp, sigma = 2 )
min_pres_kde2 <- density( min_pres_ppp, sigma = 2 )
max_wind_kde2 <- density( max_wind_ppp, sigma = 2 )
plot( start_kde2, main = "KDE of Starting Locations (sigma=2)")
map("world", add = TRUE, col = "white" )
plot( min_pres_kde2, main = "KDE of Min Pressure Locations (sigma=2)" )
map("world", add = TRUE, col = "white" )
plot( max_wind_kde2, main = "KDE of Max Wind Locations (sigma=2)" )
map("world", add = TRUE, col = "white" )


# convert point data to quadrat counts

# create the lon/lat grid
lon_breaks <- seq(win$xrange[1], win$xrange[2], by = 1)
lat_breaks <- seq(win$yrange[1], win$yrange[2], by = 1)

# window
win_grid <- owin(xrange = range(lon_breaks), yrange = range(lat_breaks))

# Make a tessellation from explicit breaks
tess_grid <- tess(xgrid = lon_breaks, ygrid = lat_breaks)

# count points per cell
Q <- quadratcount(max_wind_ppp, tess = tess_grid)  

# or like this
Q <- quadratcount(max_wind_ppp, xbreaks = lon_breaks, ybreaks = lat_breaks)

# plot isn't great
plot(Q)

# try using fields (not right)
fields::image.plot(lon_breaks[-1], lat_breaks[-1],  as.matrix(t(Q)))
map("world", add = TRUE, col = "white" )

# try again (figured out using trial and error)
Qmat <- as.matrix(Q)
fields::image.plot(lon_breaks[-1] - 0.5, lat_breaks[-1] - 0.5,  t(Qmat[nrow(Qmat):1, ]) )
map("world", add = TRUE, col = "white" )

# calculate an approximate area for each quadrat
# size of 1 degree latitude is 40000km / 360 = 111.11 km
# size of 1 degree longitude is (40000km / 360) * cos(2*pi*lat/360)
# use the center latitude and longitude for each cell
lat_centers <- (lat_breaks[-1] + lat_breaks[-length(lat_breaks)]) / 2
lon_centers <- (lon_breaks[-1] + lon_breaks[-length(lon_breaks)]) / 2
areas <- matrix(0, nrow = length(lon_centers), ncol = length(lat_centers))
for (i in 1:length(lon_centers)) {
    for (j in 1:length(lat_centers)) {
        areas[i,j] <- 111.11 * 111.11 * cos( 2*pi*lat_centers[j]/360 )
    }
}

# or, in vectorized code
area_fun <- function(lon, lat) {111.11 * 111.11 * cos( 2*pi*lat/360 )}
areas2 <- outer( lon_centers, lat_centers, FUN = area_fun )
range( areas - areas2 )

fields::image.plot(lon_centers, lat_centers, areas )
map("world", add = TRUE, col = "white" )

# plot the raw counts and the density
par(mfrow = c(1,2))
fields::image.plot(lon_breaks[-1], lat_breaks[-1],  t(Qmat[nrow(Qmat):1, ]),
    main = "Counts per pixel" )
map("world", add = TRUE, col = "white" )
fields::image.plot(lon_breaks[-1], lat_breaks[-1],  t(Qmat[nrow(Qmat):1, ])/areas,
    main = "Counts per square kilometer")
map("world", add = TRUE, col = "white" )

