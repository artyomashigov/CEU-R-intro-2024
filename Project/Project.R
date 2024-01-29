# import packages
library(data.table)
# to use fread
install.packages("curl")


# import data, we use fread to get a data.table
locations = fread('https://raw.githubusercontent.com/artyomashigov/CEU-R-intro-2024/main/Project/Data/locations.csv')
locations

str(locations)

#import make details
make_details = fread('https://raw.githubusercontent.com/artyomashigov/CEU-R-intro-2024/main/Project/Data/make_details.csv')
make_details

stolen_vehicles = fread('https://raw.githubusercontent.com/artyomashigov/CEU-R-intro-2024/main/Project/Data/stolen_vehicles.csv')
stolen_vehicles

# exploratory analysis and cleaning








# visualization