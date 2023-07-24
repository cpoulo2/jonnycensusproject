#
#
# What:         R script template for data analysis
# 
# Objective:    Provide a didactic framework for exploratory data analysis that 
#               will provide 1) a method for orGganizing analyses, how to use
#               some of the more common tool for cleaning and preparing data,
#               and tools for visualizing data (including mapping).
#
# Prepared by:  Chris
#
#_______________________________________________________________________________

# Prepare a work space ----

# First create a folder with three folders in it: 1) "origdata" (for original data 
# that hasn't been altered), 2) "scripts" (to save your R scripts in), and 3)
# "figurestables". Within the "origdata" folder create another folder "processed"
# for processed data. The purpose of these folders is to keep a tidy work space.
# The idea behind origdata and processed data is to have a back up of the 
# original unaltered copy in case something goes wrong as you are playing around 
# with it.

# Set working directory ----

# A working directory basically creates a path name for the directory (re: folder)
# that you'll be working out of. I'm setting it to the work space that I made
# for this project.

setwd("C:/Users/cdpou/Documents/GitHub/jonnycensusproject")
      
# Install packages ----

# Packages are various tool libraries for examining, cleaning, preparing, and 
#analyzing data. When you first use R you will have to use "install.packages()" to
# download the package. After you finish installing the packages you can
# delete or comment out (which means add a "#" as the first character of the line)
# so that R will not be able to perform the command again. To make the lines below
# commandable just delete the "#".

#install.packages("tidyverse") # This package contains pretty much all the tools
                              # you will need for analysis

#install.packages("tidycensus") # This allows you to get the census data without
                               # downloading it from data.census.gov.
#install.packages("sf") # This allows you to work with spatial data.
#install.packages("rgdal")
#install.packages("rgeos")
#install.packages("leaflet") # This allows you to make maps.

# Load in tool libraries ----

library(tidyverse)
library(tidycensus)
library(sf)
library(rgdal)
library(rgeos)
library(leaflet)
require(scales)

# Set options ----

options(scipen=999)   # R displays large numbers in scientific notion. This command
                      # prevents that.

options(tigris_use_cache = TRUE) # This will allow you to download Census 
# shapefiles when downloading the census data.

# Installing the Census API key ----

# We'll use the census API to get data. It's more convenient to download data 
# and it cuts down on errors since you can see the data sets you are downloading 
# in you script.

# The census API is very well documented. Begin by going to the Census API
# site and requesting a key: https://www.census.gov/data/developers/data-sets.html

# Enter the key into the command below.

# census_api_key("YOUR API KEY GOES HERE") # To install your API key for use in 
                                         # future sessions, run this function 
                                         # with `install = TRUE`.

# Some basics on objects, data frames, and census data ----

# A data frame is basically a table with columns and rows. Data frames are
# interesting because they structure data in a way in which we can try and 
# discover patterns by comparing variables.

# You create data frames by getting data from somewhere (ex. the census API or a
# spreadsheet saved on your computer) and assigning it to a variable or in R 
# language an object. An object can be a single value  (ex, a number or a string
# which means text or it can be a data frame. You create variables using "<-"

a <- 123 # a is a "double" variable, which is a type of numeric variable that
         # allows for decimals.
b <- "Hello Jonny" # b is a "character" variable since it is a string (Hello
                   # Jonny is enclosed by quotations)
c <- "123" # c is also a "character" variable because although it has numbers
           # it is surrounded by quotations.

# The following command will create a data frame. You can explore
# the data frame by clicking on it in the Environment tab below.

df <- data.frame(
  variable1 = c("Jonny","Is","learning","R"), # the c("xxx","xxx") is creating a "vector" which is basically a list or a column with the rows "Jonny", "Is", etc. In this case, "variable1" is a list of strings.
  variable2 = c(123,456,789,10112) # now I created another row with numeric values.
)

# A quick way to see the type of variables that you are dealing with is by using
# the glimpse() command.

glimpse(df)

# Using glimpse and clicking on the data frame to view it is are good ways to
# do some initial exploration of the type of data that you are dealing with.

# Ok now a bit on the census data.

# As you can see from Census API link, the census has many surveys. 
# The American Community Survey (ACS) is probably the most widely used census survey
# because it allows for local analysis down to the tract and block level, and
# it is the census survey that collects a bunch of socio-economic data, like rent,
# income, etc. It's different from the decennial census, which is designed 
# mainly to get a full count of the population, where as the ACS collects a 
# sample and infers what is going on in the larger population based on that sample.
# The decennial census is distributed once every 10 years. The scope of the
# decennial census is much more limited than the ACS. It asks basic
# demographic questions (race, household size, age, gender, and some other stuff) 
# but it doesn't have anywhere near the amount of variables that the ACS has.

# The ACS is distributed every year (since 2005). The data is provided on a 
# 1-year basis and then pooled to a 3-year basis and a 5-year basis. Pooling data 
# is basically taking the average for those years. For example, if I am looking 
# at total rent collected in a census tract in the 2021 5-year data, the estimate 
# will be total rent in 2017 + total rent in 2018 + total rent in 2019 and so on 
# divided by 5 (i.e. the 5 years of data).

# The benefit of pooling data is that it makes it more "accurate" since larger
# sample sizes are generally better. The down side is that in pooling the data
# you are capturing the year-to-year changes.

# Creating a data frame with census data ----

# Go to the API website and click on the ACS menu. Select a survey (1,3,or 5 year) 
# and year. 

# Open the html link by detailed tables to find the name for the variable
# you are interested in. It takes a little digging to find what you want.

# This will all all depend on your question or the topic you are interested
# in exploring.

# Question: What is the relationship between latinx, white, and black population 
# change and rental prices?

# This question was inspired by a conversation with the North River Commission
# who noted the importance of the loss of lower rental units in Albany Park.

# I can study this question using the ACS data on rental prices by quartiles.
# The census has the lower, median, and upper quartile contract rent. Quartiles
# divide the distribution of data (in this case rent) into quarters. The lower
# quarter covers 25% of the observations that fall into the lowest rent,
# the median is the 50% marker (50% of all observations/survey respondents are
# renting in housing at or below this price), and the upper quartile is the 75% 
# cut off (i.e 75% of all the observations/survey responses fall into this bucket).

# I'll compare the 2010 5 year ACS to the 2019 5 year ACS (i'm chosing this
# because the census tracts are the same - I can show you how I figured that
# out)

# Unfortunately it's not possible to get the the 
# census tracts at the city (in census term "place") geography, but
# we'll do this shortly when we add in the community area details.

# Lower quartile rent (lqrent) B25057_001

lqrent10 <- get_acs(geography = "tract",
                  variables = "B25057_001",
                  state = "IL",
                  county = "Cook",
                  year = 2010,
                  survey = "acs5",
                  geometry = T) # geometry downloads the spatial data, if you 
                                # download multiple files you only need to do 
                                # this once otherwise it'll cause problems
                                # when you join.

lqrent19 <- get_acs(geography = "tract",
                    variables = "B25057_001",
                    state = "IL",
                    county = "Cook",
                    year = 2019,
                    survey = "acs5")

# Median rent (mrent) B25058_001

mrent10 <- get_acs(geography = "tract",
                   variables = "B25058_001",
                   state = "IL",
                   county = "Cook",
                   year = 2010,
                   survey = "acs5")

mrent19 <- get_acs(geography = "tract",
                 variables = "B25058_001",
                 state = "IL",
                 county = "Cook",
                 year = 2019,
                 survey = "acs5")

# Upper quartile rent (uprent) B25059_001

uqrent10 <- get_acs(geography = "tract",
                    variables = "B25059_001",
                    state = "IL",
                    county = "Cook",
                    year = 2010,
                    survey = "acs5")

uqrent19 <- get_acs(geography = "tract",
                      variables = "B25059_001",
                      state = "IL",
                      county = "Cook",
                      year = 2019,
                      survey = "acs5")

# White population

white10 <- get_acs(geography = "tract",
                   variables = "B03002_003",
                   state = "IL",
                   county = "Cook",
                   year = 2010,
                   survey = "acs5")

white19 <- get_acs(geography = "tract",
                      variables = "B03002_003",
                      state = "IL",
                      county = "Cook",
                      year = 2019,
                      survey = "acs5")

# Black population

black10 <- get_acs(geography = "tract",
                   variables = "B03002_004",
                   state = "IL",
                   county = "Cook",
                   year = 2010,
                   survey = "acs5")

black19 <- get_acs(geography = "tract",
                   variables = "B03002_004",
                   state = "IL",
                   county = "Cook",
                   year = 2019,
                   survey = "acs5")

# Latine population

latine10 <- get_acs(geography = "tract",
                   variables = "B03002_012",
                   state = "IL",
                   county = "Cook",
                   year = 2010,
                   survey = "acs5")

latine19 <- get_acs(geography = "tract",
                   variables = "B03002_012",
                   state = "IL",
                   county = "Cook",
                   year = 2019,
                   survey = "acs5")

# Clean and prepare data ----

# Now I'm gets to the fun part(!) thinking through how to work with the data
# to get it in the right place to answer a question/tell a story.

# In my case, I'm going to compare rates of change in rent (by quartile) and
# population by community area. This will require a couple things.

# First I need to join the data together. This is where glimpse() and opening the
# dataframe are useful. You can see what to use as the basis of joining
# data. In order to join data you need a unique identifier. GEOIDs are unique.
# Technically NAME is unique but I prefer alpha-numeric codes because names
# can change, because joins are case sensitive. 

# For example. if it is spelled "census tract" one year "CENSUS TRACT"
# capitalized another year they will not join.

# GEOIDs are made up of FIPS codes for the state (IL = 17), county (Cook = 031),
# and census tract (the last 6 numbers).

# In order to join I need to clean the data up a little since each data frame
# has similar variable names. This is ideal for the variable that will be the
# basis of the join (i.e. GEOID) but it causes problems when other variables
# have similar names (e.g. estimate and moe are in every data set).

# I'll clean this up in the following code:

black10 <- black10[,c(1,4)]  |>
  rename(black10 = estimate)
black19 <- black19[,c(1,4)]  |>
  rename(black19 = estimate)
latine10 <- latine10[,c(1,4)] |>
  rename(latine10 = estimate)
latine19 <- latine19[,c(1,4)] |>
  rename(latine19 = estimate)
white10 <- white10[,c(1,4)] |>
  rename(white10 = estimate)
white19 <- white19[,c(1,4)] |>
  rename(white19 = estimate)
lqrent10 <- lqrent10[,c(1,4,6)] |>
  rename(lqrent10 = estimate)
lqrent19 <- lqrent19[,c(1,4)] |>
  rename(lqrent19 = estimate)
mrent10 <- mrent10[,c(1,4)] |>
  rename(mrent10 = estimate)
mrent19 <- mrent19[,c(1,4)] |>
  rename(mrent19 = estimate)
uqrent10 <- uqrent10[,c(1,4)] |>
  rename(uqrent10 = estimate)
uqrent19 <- uqrent19[,c(1,4)] |>
  rename(uqrent19 = estimate)

# Now I'll join these together. It does matter which one you start with.

analysis <- lqrent10 |> # Here we are defining analysis as the lqrent10 data frame.
  left_join(lqrent19, by="GEOID") |>
  left_join(mrent10, by="GEOID") |>
  left_join(mrent19, by="GEOID") |>
  left_join(uqrent10, by="GEOID") |>
  left_join(uqrent19, by="GEOID") |>
  left_join(white10, by="GEOID") |>
  left_join(white19, by="GEOID") |>
  left_join(black10, by="GEOID") |>
  left_join(black19, by="GEOID") |>
  left_join(latine10, by="GEOID") |>
  left_join(latine19, by="GEOID")

# Adjusting for inflation 

inflationadjustment <- 1+((256.903 - 218.576)/218.576)

analysis <- analysis |>
  mutate(lqrent10 = lqrent10*inflationadjustment,
         uqrent10 = uqrent10*inflationadjustment,
         mrent10 = mrent10*inflationadjustment)


# Here is where the spatial stuff will come into play. In order to make sense
# of this data, I am going to see what's going on at the community area level.
# Here's how I'll go about this. First I'll assign each census tract a 
# community area code and name. I can do this using the spatial data
# from the census but we'll have to download the community area spatial
# data from the City of Chicago's data portal.

# In order to save, create a new folder in your origdata folder "shp_communityareas".
# Download the data and unzip to this folder.

# The export has a long name copy and paste it.

# geo_export_48d3414c-200a-4dd1-a415-b32130f58a42

# I'll do the same for the city boundary.

# geo_export_7b6f062d-3668-4902-9884-d5129718f67e

# Read in the shape files and project them on a common basis using the st_transform() command

ca <- st_read("origdata/shp_communityarea/geo_export_48d3414c-200a-4dd1-a415-b32130f58a42.shp")
ca <- st_transform(ca,crs = 4326) #WGS84 coordinate system

city <- st_read("origdata/shp_city/geo_export_7b6f062d-3668-4902-9884-d5129718f67e.shp")
city <- st_transform(city,crs = 4326)

# Do the same for the census data

analysis <- st_transform(analysis, crs = 4326)

# The next step is to examine rent change by community area. The quick and dirty
# way to map this out would be doing something like the following: add the census
# tract layer and then add the community area layer on top. I like using leaflet.
# For the data I'll just the change in lower quartile rental prices.

analysis <- analysis |>
  mutate(blackchange = (black19-black10)/black10, na.rm=T,
         whitechange = (white19-white10)/white10, na.rm=T,
         mrentchange = (mrent19-mrent10)/mrent10, na.rm=T,
         lqrentchange = (lqrent19-lqrent10)/lqrent10, na.rm=T,
         uqrentchange = (uqrent19-uqrent10)/uqrent10, na.rm=T,
         latinechange = (latine19-latine10)/latine10, na.rm=T)

pal <- colorQuantile(palette = "Blues",
                     domain = analysis$lqrentchange,
                     n = 5)

map1 <- leaflet() |>
  addProviderTiles(provider = "CartoDB.Positron") |>
  addMapPane("lowerquartrent",zIndex=500) |>
  addMapPane("communityareas",zIndex=400) |>
  addPolygons(data=analysis,
              weight = .5,
              fillOpacity = .6,
              fillColor = ~pal(lqrentchange),
              popup = paste("Lower quartile rent change: ", analysis$lqrentchange,
                            "<br> 2010 to 2019 difference: ",
                            dollar(analysis$lqrent19-analysis$lqrent10)),
              options = pathOptions(pane = "lowerquartrent")) |>
  addPolygons(data = ca,
              weight = 3,
              fillOpacity = .1,
              color = "black",
              fillColor = "grey",
              options = pathOptions(pane = "communityareas")) |>
  addLegend(data = analysis,
            position = "bottomright",
            values = ~lqrentchange,
            pal = pal,
            title = paste("Change in lower quartile rent"))
map1

# You could even add multiple layers to explore the relationship between
# two variable.

pal <- colorQuantile(palette = "Blues",
                     domain = analysis$lqrentchange,
                     n = 5)

pal2 <- colorQuantile(palette = "Blues",
                     domain = analysis$latinechange,
                     n = 5)

# I'm going to create a layer to emphasize albany park:

albanypark <- ca |>
  filter(area_num_1 == "14")

ca_line <- st_cast(ca,"MULTILINESTRING")
albanypark <- st_cast(albanypark,"MULTILINESTRING") # I am going to change it from a polygon to a polyline (so it done't)

map2 <- leaflet() |>
  addProviderTiles(provider = "CartoDB.Positron") |>
  addMapPane("lowerquartrent",zIndex=500) |>
  addMapPane("latinechange",zIndex=500) |>
  addMapPane("communityareas",zIndex=500) |>
  addMapPane("albanypark",zIndex=500) |>
  addPolygons(data=analysis,
              weight = .5,
              fillOpacity = .6,
              fillColor = ~pal(lqrentchange),
              popup = paste("Lower quartile rent change: ", percent(analysis$lqrentchange, accuracy = 2),
                            "<br> 2010 to 2019 difference: ",
                            dollar(analysis$lqrent19-analysis$lqrent10),
                            "<br> Change in latine population: ",
                            percent(analysis$latinechange, accuracy = 2)),
              options = pathOptions(pane = "lowerquartrent"),
              group = "Change in lower quartile rental prices") |>
  addPolygons(data=analysis,
              weight = .5,
              fillOpacity = .6,
              fillColor = ~pal2(latinechange),
              popup = paste("Lower quartile rent change: ", percent(analysis$lqrentchange, accuracy = 2),
                            "<br> 2010 to 2019 difference: ",
                            dollar(analysis$lqrent19-analysis$lqrent10),
                            "<br> Change in latine population: ",
                            percent(analysis$latinechange,accuracy =2)),
              options = pathOptions(pane = "lowerquartrent"),
              group = "Change in latine population") |>
  addPolylines(data=ca_line,
              weight = 2,
              opacity = .3,
              color = "black",
              fillColor = "grey",
              options = pathOptions(pane = "communityareas")) |>
  addPolylines(data = albanypark,
              weight = 3,
              stroke = TRUE,
              color = "red",
              opacity = 1,
              options = pathOptions(pane = "albanypark"),
              group = "Albany Park") |>
  addLayersControl(overlayGroups = c("Change in lower quartile rental prices","Change in latine population","Albany Park")) |>
  hideGroup(c("Change in latine population", "Albany Park")) |>
  addLegend(data = analysis,
            position = "bottomright",
            values = ~lqrentchange,
            pal = pal,
            title = paste("Changes (quintiles)"))
map2

# This allows us to visually see what's going on but it's difficult to compare
# across community areas. In order to do that I'm going to spatially join the 
# community area data and the census tract data.

# There is one complication. In order to do this we will have to transform the
# census tract polygon into it's "centroid" which is the point that lands 
# geometrically in the middle otherwise census tract's outside of albnay
# part are included, since the census tract geography and the community area
# geography are incongruous.

# I'll show an example by grabbing the census tracts in albany park.

# First, I'll get the census tracts in ablany park using a join.

ap_cts_ver1 <- st_intersection(albanypark,analysis,) # 21 census tracts


centroid <- st_centroid(analysis)

albanypark2 <- ca |>
  filter(area_num_1 == "14")

ap_ct_ver3 <- st_within(centroid,albanypark2)

ap_cts_ver2 <- st_join(centroid,left=TRUE,albanypark2["community"]) # 11 census tracts

# Here's what's going on with the difference

ct <- analysis[,c(14)]
ct_centroid <- st_centroid(ct)
ap_cts <- ap_cts_ver1[30]
plot(ap_cts)
plot(ct_centroid,pch=20,add=T)
plot(ct,add=T,col="Red")
plot(albanypark,add=T,col="Blue")
plot(ct_centroid,pch=20,add=T)

# I'll now join the community area data to the census tract data:

censusdata_ca <- st_join(centroid,left=TRUE,ca["community"])

censusdata_ca <- censusdata_ca |>
  filter(!is.na(community))

cityboundary <- city[,c(5)]
chi_ct <- censusdata_ca[,c(22)]

plot(cityboundary)
plot(chi_ct,pch=20,add=T)

# Now I'll summarize the changes by community area

ca_analysis <- censusdata_ca |>
  group_by(community) |>
  dplyr::summarise(black10 = sum(black10,na.rm=T),
                   black19 = sum(black19,na.rm=T),
                   latine10 = sum(latine10,na.rm=T),
                   latine19 = sum(latine19,na.rm=T),
                   white10 = sum(white10,na.rm=T),
                   white19 = sum(white19,na.rm=T),
                   lqrent10 = median(lqrent10,na.rm=T),
                   lqrent19 = median(lqrent19,na.rm=T),
                   uqrent10 = median(uqrent10,na.rm=T),
                   uqrent19 = median(uqrent19,na.rm=T),
                   mrent10 = median(mrent10,na.rm=T),
                   mrent19 = median(mrent19,na.rm=T)
                  )

ca_analysis <- ca_analysis |>
  mutate(blackchange = (black19-black10)/black10, na.rm=T,
         whitechange = (white19-white10)/white10, na.rm=T,
         mrentchange = (mrent19-mrent10)/mrent10, na.rm=T,
         lqrentchange = (lqrent19-lqrent10)/lqrent10, na.rm=T,
         uqrentchange = (uqrent19-uqrent10)/uqrent10, na.rm=T,
         latinechange = (latine19-latine10)/latine10, na.rm=T)

#go1028go
Leo Panitch"goldstanard"




# Analyze and visualize data ----