
# load the processed hurdat data:
load("../processed_data/hurdat.RData")

# subset to the 2024 season
dat <- hurdat[ hurdat$year == 2024, ]


# use the maps package to draw a map of the Atlantic basin
library(maps)

pdf( "../figures/2024_season_tracks.pdf", width = 8, height = 6 )
map("world",
    xlim = c(-100, 20), ylim = c(0, 50), col = "lightgray", fill = TRUE, bg = "white",  lwd = 0.5
)
# add the tracks for the 2024 season
for( s in unique(dat$id) ) {
  storm_dat <- dat[ dat$id == s, ]
  points(storm_dat$lon, storm_dat$lat, type = "o", col = "blue", lwd = 2)
}
dev.off()
