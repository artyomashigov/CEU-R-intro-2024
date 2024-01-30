# import packages
library(data.table)
# to use fread
install.packages("curl")


# import data, we use fread to get a data.table ----
locations = fread('https://raw.githubusercontent.com/artyomashigov/CEU-R-intro-2024/main/Project/Data/locations.csv')
locations

str(locations)
summary(locations)

# import make details ----
make_details = fread('https://raw.githubusercontent.com/artyomashigov/CEU-R-intro-2024/main/Project/Data/make_details.csv')
make_details

str(make_details)

# import stolen_vehicles ----
stolen_vehicles = fread('https://raw.githubusercontent.com/artyomashigov/CEU-R-intro-2024/main/Project/Data/stolen_vehicles.csv')
stolen_vehicles

str(stolen_vehicles)

# Data transformation and analysis ----

# Merge datasets
df <- merge(stolen_vehicles, make_details, by = "make_id", all.x = TRUE)
df <- merge(df, locations, by = "location_id", all.x = TRUE)

# check missing values
sapply(df, function(x) sum(is.na(x)))

# drop missing values
df <- na.omit(df)

# use summary
summary(df)

df


# Count the frequency of each vehicle type
vehicle_type_counts <- df[, .N, by = .(vehicle_type)][order(-N)]

# View the counts
print(vehicle_type_counts)

# visualization