
dat <- read.csv("../raw_data/hurdat2-1851-2024-040425.txt",header=FALSE)

# create an id column
dat$id <- NA
dat$name <- NA

# fill in the id column with the id if the row starts with AL,
# otherwise fill it with the previous row's id
for(j in 1:nrow(dat)){
  if(substr(dat$V1[j],1,2)=="AL"){
    dat$id[j] <- dat$V1[j]
    dat$name[j] <- dat$V2[j]
  } else {
    dat$id[j] <- dat$id[j-1]
    dat$name[j] <- dat$name[j-1]
  }
}

colnames(dat)[c(7,8)] <- c("wind_speed","pressure")

# remove rows that start with AL
dat <- dat[ substr(dat$V1,1,2) != "AL", ]

# add a date column
dat$date <- as.Date( dat$V1, format="%Y%m%d" )

# add a year column
dat$year <- as.numeric( format(dat$date, "%Y") )

# add some time columns
dat$days_since_1800 <- as.numeric( dat$date - as.Date("1800-01-01") )
dat$time_since_1800 <- dat$days_since_1800 + as.numeric(dat$V2)/2400
dat$decimal_doy <- as.numeric( format(dat$date, "%j") ) + as.numeric(dat$V2)/2400

# remove white space from V4
dat$V3 <- gsub(" ", "", dat$V3)
dat$V4 <- gsub(" ", "", dat$V4)
dat$name <- gsub(" ", "", dat$name)

# fix the lat and lon values
dat$V5 <- gsub(" ", "", dat$V5)
dat$lat <- as.numeric( substr(dat$V5, 1, nchar(dat$V5)-1) )
dat$V6 <- gsub(" ", "", dat$V6)
dat$lon <- -as.numeric( substr(dat$V6, 1, nchar(dat$V6)-1) )

# create an in_usa indicator
# get the continental US polygon
library("maps")
library("sp")
usa <- map("usa", fill=TRUE, plot=FALSE)
na_inds <- c(0,which( is.na( usa$x ) ))
usa_list <- list( length=length(na_inds) )
for(j in 1:length(usa_list)){
    usa_list[[j]] <- data.frame(
        x=usa$x[(na_inds[j]+1):(na_inds[j+1]-1)], y=usa$y[(na_inds[j]+1):(na_inds[j+1]-1)],
        name = usa$names[j]
    )
}

in_usa_fun <- function(lon, lat){
    in_usa <- rep(FALSE, length(lon))
    for(j in 1:length(usa_list)){
        in_usa <- in_usa | point.in.polygon(lon, lat, usa_list[[j]]$x, usa_list[[j]]$y) 
    }
    return(in_usa)
}
                          
dat$in_usa <- in_usa_fun( dat$lon, dat$lat )

#inout <- c("blue","black")
#plot( dat$lon, dat$lat, col=inout[1+dat$in_usa], pch=16, cex=0.5, xlim = c(-120,-60), ylim=c(20,60) )
#map("usa", add = TRUE)


# create a new data frame with a row for each storm and some summary statistics
# you'll want to put more info in here, like date-time of landfall, date-time it first becomes
# a tropical storm, hurricane, and whatever else you'll need
library("dplyr")
storms <- dat %>%
    group_by(id) %>%
    summarize(
        name = first(name),
        start_date = min(date),
        end_date = max(date),
        max_wind = max(wind_speed),
        year = first(year),
        hurricane = any(V4 == "HU"),
        hurricane_time = first( time_since_1800[ V4 == "HU" ] ),
        tropical_storm_time = first( time_since_1800[ V4 %in% c("TS","HU") ] ),
        H_decimal_doy = first( decimal_doy[ V4 == "HU" ] ),
        TS_decimal_doy = first( decimal_doy[ V4 %in% c("TS","HU") ] ),
        cyclone_time = first( time_since_1800[ V4 %in% c("TS","HU","TD") ] ),
        landfall_hurricane = any(V4 == "HU" & in_usa),
        tropical_storm = any(V4 == "TS") | any( V4 == "HU" ),
        start_lon = first(lon),
        start_lat = first(lat),
        min_pressure_lon = lon[ which.min(pressure) ],
        min_pressure_lat = lat[ which.min(pressure) ],
        max_wind_lon = lon[ which.max(wind_speed) ],
        max_wind_lat = lat[ which.max(wind_speed) ]
    ) %>% as.data.frame()
storms <- storms[ order(storms$start_date), ]
       
# get some yearly statistics
yearly <- data.frame( year = 1851:2024 )
for(j in 1:nrow(yearly)){
    yearly$n_storms[j] <- sum( storms$year == yearly$year[j] )
    yearly$n_tropical_storms[j] <- sum( storms$year == yearly$year[j] & storms$tropical_storm )
    yearly$n_hurricanes[j] <- sum( storms$year == yearly$year[j] & storms$hurricane )
    yearly$n_landfall_hurricanes[j] <- sum( storms$year == yearly$year[j] & storms$landfall_hurricane )
}

#hist( yearly$n_hurricanes, breaks=200 )
#
#ii <- yearly$year >= 1980
#mean( yearly$n_hurricanes[ii] )
#mean( yearly$n_hurricanes[!ii] )
#
#
#var( yearly$n_hurricanes[ii] )
#var( yearly$n_hurricanes[!ii] )


# save the processed data
hurdat <- dat
save(hurdat, file="../processed_data/hurdat.RData")
save(storms, file="../processed_data/storms_hurdat.RData")
save(yearly, file="../processed_data/yearly_hurdat.RData")
