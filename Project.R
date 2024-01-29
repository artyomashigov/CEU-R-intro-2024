# import packages
library(data.table)
# to use fread
install.packages("curl")


# import data, we use fread to get a data.table
locations = fread('https://raw.githubusercontent.com/artyomashigov/CEU-R-intro-2024/main/Project/Data/locations.csv')
locations
# exploratory analysis and cleaning
str(locations)

