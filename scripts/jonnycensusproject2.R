# Making a map with ggplot

# Set working directory ----

setwd("C:/Users/cdpou/Documents/GitHub/jonnycensusproject") # By setting the
                                                            # working directory
                                                            # when you read in 
                                                            # or save a file 
                                                            # later on you 
                                                            # don't have to type 
                                                            # in the entire path.
                                                            # It's not necessary
                                                            # it's just a little
                                                            # short cut.

# The work space I created  "jonnycensusproject" has three main folders: 
# origdata,
# figurestables,
# scripts.

#################### jonnycensusproject workspace ##############################
#
# origdata      - original unmanipulated data, which includes:
#               - shp_communityarea - folder with the community area shapefile
#                                   - A shapefile (.shp) is a spatial file which
#                                     can be a polygon, lines, points all with
#                                     some data attached to it.
#               - "Community_Data_Snapshots_2023_-7949553649742586148.csv" the
#                 community area data downloaded from CMAP.
#
# figurestables - this has an example of the maps that I made using this script
#
# scripts       - has this script: "jonnycensusproject2.R".
#
################################################################################

# Install packages (if need be) ----

# You'll need tidyverse, sf, and cowplot (cowplot allows you to place maps
# side by side). To run the installs below delete the "#" on the line of code
# and run it.

#install.packages("tidyverse")
#install.packages("sf")
#install.packages("cowplot")

# Load in tool libraries ----

library(tidyverse)
library(sf)
library(cowplot)

# Set options ----

options(scipen=999)   # R displays large numbers in scientific notion. This command
                      # prevents that.

# Read in data ----

# Community area shapefile (downloaded from the City of Chicago Data portal)
commareas <- st_read("origdata/shp_communityarea/geo_export_48d3414c-200a-4dd1-a415-b32130f58a42.shp") # since the workspace is set to the main "jonnycensusproject" folder, when you read in or save a file you don't ahve to type in the path prior to the working directory.

# CMAP data (downloaded from the CMAP data hub)
cmapdata <- read_csv("origdata/Community_Data_Snapshots_2023_-7949553649742586148.csv")

# Clean data ----

# You need to merge the community area shapefile (which has the spatial data)
# to the CMAP data.

# In order to do this you need a common basis for merging (a unique identifier 
# that links the data sets). In this case, the community area number works. 
# If you use glimpse() you can get a sense of what variables will be the basis
# of merging and how you might need to change them. 

glimpse(commareas)
glimpse(cmapdata)

# cmapdata  - GEOID is a numeric unique identifier for community areas.
# commareas - area_num_1 is the corresponding unique identifier, but it's text.

# You either have to make GEOID into text or area_num_1 into a number.
# I'll make the GEOID text using the mutate function and using the
# as.character() function which changes a variable into text.

cmapdata <- cmapdata |>
  mutate(GEOID_text = as.character(GEOID))

# for good measure I view the data frame to makes sure it worked. When
# you hover over the column name it'll tell you the type of data. The new 
# variable will be the very last column. You could also use glimpse() to see this.

# Now i'll create a new data frame which merges the community area 
# shapefile to the cmapdata using merge().

datamerge <- merge(x=commareas,y=cmapdata,by.x = "area_num_1",by.y="GEOID_text")

# Again for good meausure, I'll veiw the data frame to make sure the 
# "geometry" variable is there.

# I'm going to look at the spatial relationship between the percent of black 
# residents in a community and the percent of unemployment.

# fyi I found CMAPs data dictionary useful (https://datahub.cmap.illinois.gov/maps/05831ce4f3064b3f8e26c85be10d9836/about).

# The data has counts so I need to create a percent variable.

datamergeformaps <- datamerge |>
  mutate(black_per = BLACK/TOT_POP, # taking the total black population divided by the total population
         unemployment_per = UNEMP/EMP) # taking the total unemployed population divided by the total employed population

# Making a maps! ----

# I'll start with making maps with little formatting. Then below that I'll do more 
# formatting to make it look at little nicer

# Maps with little formatting.

map1 <- ggplot() +
  geom_sf(data = datamergeformaps, # geom_sf visualizes data spatially - analogous to the way geom_bar visualizes data in a bar chart format
          aes(fill=black_per), # fill is the variable that you will visualize.
          color = "grey") + # color sets the polygon outline color. I'm setting it to a lighter color because it makes it a little easier to see the variation between polygons.
  ggtitle("Percent Black by Chicago community area") + # ggtittle() is a way to put a title to the plot.
  theme_void() # thinks creates a white background otherwise it has the latitude and longitude with tick marks. If you run it without this code you can see what I mean.
map1

map2 <- ggplot() +
  geom_sf(data = datamergeformaps,
          aes(fill=unemployment_per),
          color = "grey") + 
  ggtitle("Percent unemployment by Chicago community area") +
  theme_void() 
map2

# I'll view them side by side using plot_grid().

plot_grid(map1, map2, ncol = 2, align = "v", axis = "tb", rel_widths = c(1, 1))

# And i'll save it as a jpeg using ggsave()

ggsave("figurestables/maps_littleformating.jpeg",width = 11, height = 8.5)

# Now i'll do a little formatting to make these look a little nicer

#install.packages("scales") # to change the legend to a percent format and give it a title you need to install "scales"
library(scales)
  
map3 <- ggplot() +
    geom_sf(data = datamergeformaps,
            aes(fill=black_per),
            color = "grey") + 
    ggtitle("Percent Black \nby Chicago community area") + # I added a line break using backslash n (\n). You just type it in the text.
    theme(plot.title = element_text(hjust = 0.5, face = "bold"), # using theme() I'm centering the title and making it bold
          panel.background = element_rect("White"), # making the background white
          axis.text=element_blank(), # removing the axis text
          axis.ticks = element_blank()) + # removing the axis tick marks
  
                                          # I haven't found a way to do this with theme_void().
                                          # Using theme() you have to basically manually do all the stuff
                                          # theme_void() did (making the background white
                                          # and removing the tick marks and text in order
                                          # to adjust the title and other things. If you
                                          # add theme_void() after theme() it will override
                                          # all the commands in theme() as if you didn't do it.
    scale_fill_continuous(labels=percent_format(),  # scales() lets you change the format of the data,
                          name = "Percent black",   # add a title to the legend
                          low = "lightgoldenrod1", high = "lightgoldenrod4") # and change the color (I found the colors here: http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf)
                                                           # I suck at design so ignore the color choice, but that's code to redo the color scheme.
  map3

map4 <- ggplot() +
  geom_sf(data = datamergeformaps,
          aes(fill=unemployment_per),
          color = "grey") + 
  ggtitle("Percent unemployment \nby Chicago community area") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.background = element_rect("White"),
        axis.text=element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_continuous(labels=percent_format(), 
                        name = "Percent unemployed",
                        low = "lightgoldenrod1", high = "lightgoldenrod4")
map4

# I'll view them side by side using plot_grid().

plot_grid(map3, map4, ncol = 2, align = "v", axis = "tb", rel_widths = c(1, 1))

# And i'll save it as a jpeg 

ggsave("figurestables/maps_moreformating.jpeg",width = 11, height = 8.5)
