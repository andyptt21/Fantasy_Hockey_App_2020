# Fantasy_Hockey_App_2020

I use these scripts to make an RShiny app for visualizing the current state of my fantasy hockey league.

scraper.py extracts the data from the webpage and reads it into R using the reticulate package.

clean_and_save_data.R calls scraper.py, cleans up the input, calculates z scores, rankings, averages, etc. and saves the result in the Rda file. This only needs to be run once a week.

app.R makes the Shiny app.
