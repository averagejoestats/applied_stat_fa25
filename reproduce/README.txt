
Ultimate goal is for someone to be able to clone your github repository, run
the code, and produce the same results that you got, whether that be tables of
results, figures, or a full report with text, figures, and tables.

This is harder than it sounds, but it's attainable!

Why might it fail?
  * your github is not up to date
  * you don't include key datasets or scripts in the github
  * you don't have instructions for how to run the code
  * the code depends on other software that the user doesn't have
  * various other reasons

Sometimes the "user" is you, but 3 months in the future


How do move in the right direction:

Organize your project with subdirectories:
  * raw_data/ - for data that was given to you or downloaded
  * processed_data/ - datasets that you have cleaned 
  * scripts/ - R or python scripts for processing the data and doing analysis
  * figures/ - save your figures here
  * tables/ - write tables to text files for inputting into latex
  * reports/ - for latex or Rmd files
  * packages/ - source code for R packages if your project depends on specific versions

These are just the main ones I tend to use. There could be other logical organizing folders
Organization helps the user (which might be you) work through any problems on their own.


Choose good names for things:
  * clean_raw_hurdat2_data.R (is this specific enough?)
  * poisson_dispersion_analysis.R
  * plot_representative_tracks.R
  * processed_data/hurdat_processed.RData
  * figure3.pdf
  * figure3_helene_track.pdf
  * use 2025-10-21 date formats


Document things with README files
  * You can put one in any directory
  * Use these to explain what the subdirectories are
  * Use these to explain how to run the code
  * explain how to download the latest version of the data, if too big for github


Separate analysis tasks into logical steps, each with its own script
  * script for downloading the raw data (or scraping)
  * script for cleaning, which saves a cleaned dataset
  * script for analysis, which saves fitted models
  * script for generating figures and tables
    - you can save figures as pdf or png files
    - far superior to saving them in RStudio!
  * put comments in your code when appropriate!

This makes it easier to make logical sense of what's going on, as opposed
to putting everything in one big script.


Use a Makefile or an executable to clearly document what needs to be run
  * show the vaporfly executable
  * show an example of a Makefile

Use git and github!

What do you do if the data file is too big to put on github?
  * Create the directory for the data, and include a README.md file in it, 
    explaining what data file should go in that directory.


Try to implement at least some of these ideas to your projects this semester
Don't feel that your next project has to be perfect. Organization is a learned
skill that you can work towards with every project.
You must take it seriously though.
There are many other tools available. It's not important to learn them all, though
they may help. It's more important to bring the right mindset to your project.


More information from Karl Broman's website and youtube talk:
https://www.biostat.wisc.edu/~kbroman/presentations/steps2rr.pdf
https://www.youtube.com/watch?v=rNQ-RlG3JnQ





