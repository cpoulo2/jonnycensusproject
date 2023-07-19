#
#
# What:         R script template for data analysis
# 
# Objective:    Provide a didactic framework for exploratory data analysis that 
#               will provide 1) a method for organizing analyses, how to use
#               some of the more common tool for cleaning and preparing data,
#               and tools for visualizing data.
#
# Prepared by:  Chris
#
#_______________________________________________________________________________

# Prepare a work space ----

# First create a folder with three folders in it: 1) "origdata" (for original data 
# that hasn't been altered), 2) "scripts" (to save your R scripts in), and 3)
# "figurestables". Within the "origdata" folder create another folder "processed"
# for processed data. The purpose of the folders is to keep a tidy workspace for
# downloading and processing data. The idea behind origdata and processed data
# is to have a back up of the original data in case something goes wrong as you
# are playing around with it.

# Set working directory ----

# A working directory basically creates a path a list of other files/folders.
# I'm setting this working directory to the workspace where my origdata, etc.
# folders are.

setwd("C:/Users/cdpou/Documents/GitHub/jonnycensusproject")

# Install packages ----

# Packages are various tools libraries for examining, cleaning, preparing, and analyzing
# data. When you first use R you will have to use "install.packages()" to
# download the package. After you finish installing the packages you can
# delete or comment out (which means add a "#" as the first charcter of the line)
# so that R will not beable to perform the command.

#install.packages("tidyverse") # This package contains pretty much all the tools
                              # you will need for analysis

#install.packages("tidycensus") # This allows you to get the census data without
                               # downloading it from data.census.gov.

# Load in tool libraries ----

library(tidyverse)
library(tidycensus)

# Set options ----

options(scipen=999)   # R displays large numbers in scientific notion. This command
                      # prevents that.

options(tigris_use_cache = TRUE) # This will allow you to download Census shapefiles
                                 # if you want to make maps.

# Getting started with census data ----

# We'll use the census API to get data. It's more convenient to download data 
# and cuts down on as you can clearly see the data sets you are downloading in
# you script.

# The census API is very well documented. Begin by going to the Census API
# site and requesting a key: https://www.census.gov/data/developers/data-sets.html

# Enter the key into the command below.

census_api_key("YOUR API KEY GOES HERE")

# A brief note on data frames and census data ----

# A data frame is basically a table with columns and rows. You create dataframes
# by getting data from somewhere and assigning to a variable or in R language an
# object. An object can be a single value (ex, a number or a "string" which means
# text or it can be a dataframe. You create variables using "<-"

a <- 123
b <- "Hello Jonny"

# If you the following command you will create a data frame. You can explore
# the data frame by clicking on it in the Environment tab below.

df <- data.frame(
  variable1 = c("Jonny","Is","learning","R"), # the c("xxx","xxx") is creating a "vector" which is basically a list or a column with the rows "Jonny", "Is", etc. In this case, "variable1" is a list of strings.
  variable2 = c(123,456,789,10112) # now I created another row with numeric values.
)

# That's what it will look like when we bring in the census data.

# Ok now a bit on the census data.

# As you can see from Census API link, the census has many surveys. The American Community Survey (ACS)
# is probably the most widely used survey because it allows for local analysis
# down to the tract and block level. This is what you'd use to look at 
# socio-economic and demographic variables.

# Open the ACS tab and open the 5-year data link. The ACS is distributed every
# year (since 2005). The data is provided on a 1-year basis and then pooled to
# a 3-year basis and a 5-year basis. Pooling data is basically taking the average
# for those years. For example, if I am looking at total rent collected in a
# census tract in the 2021 5-year data, the estimate will be total rent in
# 2017+ total rent in 2018 + total rent in 2019 and so on divided by 5.

# The benefit is that it is more "accurate" meaning that the margin
# of error decreases with a larger pool (i.e. a larger sample sized pooled together).

# Create a data frame pulling census data ----

# Select a year (FYI, 2021 5-year data will pool 2017-2021).

# Open the html link by detailed tables to find the name for the variable
# you are interested in. It takes a little digging to find what you want.

medianrent <- get_acs(geography = "tract",
                      variables = "B25058_001",
                      state = "IL",
                      county = "Cook",
                      year = 2021,
                      survey = "acs5")

black_per <- get_acs(geography = "tract",
                      variables = "DP05_0078P",
                      state = "IL",
                      county = "Cook",
                      year = 2021,
                      survey = "acs5")

white_per <- get_acs(geography = "tract",
                     variables = "DP05_0077P",
                     state = "IL",
                     county = "Cook",
                     year = 2021,
                     survey = "acs5")

plot(black_per$estimate,medianrent$estimate)
plot(white_per$estimate,medianrent$estimate)

# Clean and prepare data ----
# Analyze and visualize data ----