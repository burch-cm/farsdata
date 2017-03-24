## ---- echo = FALSE-------------------------------------------------------
library(farsdata)

## ------------------------------------------------------------------------
filename <- system.file("extdata", "accident_2015.csv.bz2", package = "farsdata")
fars2015 <- fars_read(filename)
head(fars2015)

## ------------------------------------------------------------------------
my_data <- fars_read_years(2015)


## ------------------------------------------------------------------------
fars_summarize_years(c(2014, 2015))
y <- c("2014", "2015")
fars_summarize_years(y)

## ---- warning=FALSE, message=FALSE---------------------------------------
fars_map_state(17, 2015)

