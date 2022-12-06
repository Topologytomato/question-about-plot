# setwd("/Users/willowwu/Documents/678midtermprojectdata")
# the data is downloaded from http://insideairbnb.com/get-the-data
# load the data
library(tidyverse)
library(ggplot2)
library(sf)
library(lme4)

# read Boston data
listings_boston <- read.csv("BOSTON/listings 2.csv")
neighbourhood_boston <- st_read("BOSTON/neighbourhoods.geojson")
# neighbourhood_boston_list <- read.csv("BOSTON/neighbourhoods.csv")
# neighbourhood_boston_list <- as.list(neighbourhood_boston_list$neighbourhood)

# read Chicago data
listings_chicago <- read.csv("CHICAGO/listings 2.csv")
neighbourhood_chicago <- st_read("CHICAGO/neighbourhoods.geojson")
# neighbourhood_chicago_list <- read.csv("CHICAGO/neighbourhoods.csv")

# read Hawaii data
listings_hawaii <- read.csv("HAWAII/listings 2.csv")
neighbourhood_hawaii <- st_read("HAWAII/neighbourhoods.geojson")
# neighbourhood_hawaii_list <- read.csv("HAWAII/neighbourhoods.csv")

# add a column of city
listings_boston$city <- "Boston"
listings_chicago$city <- "Chicago"
listings_hawaii$city <- "Hawaii"

# filter the data
listings_boston <-listings_boston %>% 
  filter(neighbourhood_cleansed %in% neighbourhood_boston_list)

# combine the three data
col_need <- c("host_id", "city", "neighbourhood_cleansed", "latitude", 
              "longitude", "room_type", "accommodates",
              "bathrooms_text", "bedrooms", "beds", "price")
data_bind <- bind_rows(listings_boston, listings_chicago, listings_hawaii)[,col_need]

# deal with bedrooms and bedrooms_text
data_clean <- data_bind %>% separate(
  col = `bathrooms_text`,
  into = c("bathroom_numbers", "bathroom_type"),
  sep = " ",
  fill = "right"
)

# clean the data
# deal with half-bathroom
data_clean[which(data_clean$bathroom_type == "baths"), "bathroom_type"] <- "bath"
row_halfbath <- which(data_clean$bathroom_type == "half-bath")
data_clean[row_halfbath,"bathroom_type"] <- data_clean[row_halfbath,"bathroom_numbers"]
data_clean[row_halfbath,"bathroom_numbers"] <- 0.5
data_clean$bathroom_type <- sapply(data_clean$bathroom_type,tolower)

row_halfbath2 <- which(data_clean$bathroom_numbers == "Half-bath")
data_clean[row_halfbath2,"bathroom_type"] <- data_clean[row_halfbath2,"bathroom_numbers"]
data_clean[row_halfbath2,"bathroom_numbers"] <- 0.5
data_clean$bathroom_numbers <- sapply(data_clean$bathroom_numbers, as.numeric)

# deal with other numeric variables
data_clean$accommodates <- sapply(data_clean$accommodates, as.numeric)
data_clean$price <- gsub("\\$", "", data_clean$price)
data_clean[,10:12] <- sapply(data_clean[,10:12], as.numeric)
names(data_clean)[3]<-paste("neighbourhood")

# data cleaning finished

# EDA with different region and the house price
col1 <- c("city", "neighbourhood","price")
data_1 <- data_clean[,col1]
ggplot(data_1, aes(x=city, y=price)) + geom_boxplot(fill='lightblue') +
labs(x = "city", y = "price", title = "Price vs City") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20L,
                              hjust = 0.5),
    axis.title.y = element_text(size = 15L),
    axis.title.x = element_text(size = 15L)
  )


# pool with neighbourhoods
col2 <- c("city", "neighbourhood", "latitude", "longitude","price")
data_boston <- data_clean[which(data_clean$city == "Boston"),col2]

price_map_boston <- ggplot()+
  geom_sf(data=neighbourhood_boston,colour='black',fill=NA)+
  geom_point(mapping = aes(x = longitude, y = latitude, 
                            color = price, size = price),data = data_boston)+
  scale_colour_gradientn(colors = c(rev(RColorBrewer::brewer.pal(10, "RdBu")))) + 
  scale_size_continuous(range=c(0.2,2))+
  labs(title = "Boston ")
  theme_bw()


# fit multilevel model with price data and room variables

# fit the model with group of city
# fit the model with group of households
col1 <- c("city", "neighbourhood","room_type","accommodates",
          "bathroom_numbers", "bathroom_type","bedrooms", "beds","price")
data_1 <- data_clean[,col1]

M1 <- lm(price ~ room_type + accommodates + bathroom_numbers +
           bathroom_type + bedrooms + beds + neighbourhood,
         data=data_1)

M2 <- lmer(price ~ room_type + accommodates + bathroom_numbers +
             bathroom_type + bedrooms + beds + (1 | neighbourhood),
           data=data_1)

# M3 <- lmer(price ~ room_type + accommodates + bathroom_numbers +
#              bathroom_type + bedrooms + beds + (neighbourhood | city),
#            data=data_1)

# fit model with price change and time



