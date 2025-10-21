
reports/report.pdf: reports/report.Rmd figures/2024_season_tracks.pdf
	cd reports;R -e "rmarkdown::render('report.Rmd')"

figures/2024_season_tracks.pdf: scripts/plot_2024_season.R processed_data/hurdat.RData
	cd scripts;Rscript plot_2024_season.R

processed_data/hurdat.RData: scripts/process_hurdat.R raw_data/hurdat2-1851-2024-040425.txt
	cd scripts;Rscript process_hurdat.R
