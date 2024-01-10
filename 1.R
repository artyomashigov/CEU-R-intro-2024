plot(seq(0,20,0.1), sin(seq(0,20,0.1)), type = 'l')

?curve
curve(sin, from = 0, to = pi * 2)

curve(cos, from = 0, to = pi * 2, add = TRUE, col ='red')

## TO DO random walk

x <- 0

## 100 iterations

for (i in 1:100) {
  if (runif(1) < 0.5) {
    x <- x + 1
  } else {
    x <- x-1
  }
}
x

## Vecorize
runif(15)
round(runif(100))
cumsum(round(runif(100)*2 -1))
set.seed(42)

# combine vectors
h <- c(174, 170, 160)
w <- c(90, 80, 70)

min(w)
max(w)
range(w)
diff(range(w))

mean(w)
median(w)
sum(w)
summary(w)

# vector is a list

cor(w, h)
lm(w ~ h)


# let's see predicted for 165 cm
-146.154 + 165 * 1.346

fit <- lm(w ~ h)
str(fit)

summary(fit)

predict(fit, newdata = list(h = 165))


predict(fit, newdata = list(h = 52))
plot(h,w)
abline(fit, col = "red")

?data.frame
df <- data.frame(weight = w , height = h)
df
str(df)

df$weight
df$weight[1]


df[1,1]
df[2,2]
df
df[1,2]
nrow(df)
ncol(df)
dim(df)

plot(df)

cor(df)

# compute BMI
df$bmi <- df$weight/(df$height/100)^2
df

df <- read.csv("http://bit.ly/CEU-R-heights")
# compute BMI again

df$bmi = (df$weightLb/2.2)/((df$heightIn*2.5)/100)^2
df


plot(df)

install.packages("pairsD3")
library(pairsD3)
2+3
pairsD3::pairsD3(df)

library(GGally)
install.packages("GGally")

ggpairs(df)

library(ggplot2)

ggplot(df, aes(x = heightIn)) + geom_histogram()

ggplot(df, aes(x = heightIn, y = weightLb)) + geom_point()

ggplot(df, aes(x = heightIn, y = weightLb, color = sex)) + geom_point()

g <- ggplot(df, aes(x = heightIn, y = weightLb, color = sex)) + geom_point()
g
system.time(print(g))

# to change themes
g + theme_bw()
g + theme_void()

g + geom_smooth()

g + geom_smooth(method = 'lm')          

g + geom_smooth(method = 'lm', se  = FALSE)  


# different result

ggplot(df, aes(x = heightIn, y = weightLb)) + geom_point(aes(color = sex)) +
  geom_smooth(method = 'lm', se  = FALSE,color = 'black')  


# different result #2

ggplot(df, aes(x = heightIn, y = weightLb)) + geom_point(aes(color = sex)) +
  geom_smooth(method = 'lm', se  = FALSE,color = 'black')  + 
  geom_smooth(aes(color = sex),method = 'lm', se  = FALSE)

# different scale 

g + scale_y_log10()

# boxplot

ggplot(df, aes(x = heightIn)) + geom_boxplot()


ggplot(df, aes(sex, heightIn)) + geom_boxplot()
ggplot(df, aes(sex, heightIn)) + geom_violin()


ggplot(df, aes(sex, heightIn)) + geom_boxplot() + geom_violin(alpha = 0.5) +
  geom_jitter()

# density plot
ggplot(df, aes(heightIn, fill = sex)) + geom_density(alpha = 0.5) + theme_bw() + 
  ggtitle("Height of boys and girls") + 
  xlab("Height (cm)") + ylab("") + theme(legend.position = "top")

# TODO bar chart #females and males
# TODO histogram of weight
# TODO histogram of weight split by sex
# TODO bar chart #females and males taller than 57
#1 
ggplot(df, aes(sex, fill = sex)) + geom_bar() + ggtitle("Number of males and females")+
  ylab("") + theme_bw()

#2 
ggplot(df, aes(weightLb)) + geom_histogram() +
  ggtitle("Histogram of weight distribution")

#3 
ggplot(df, aes(weightLb, fill = sex)) + geom_histogram() +
  ggtitle("Histogram of weight distribution by sex") + facet_wrap(~sex)
#4
filtered_df <- df[df$heightIn > 60, ]
ggplot(filtered_df, aes(sex, fill = sex)) + geom_bar() + ggtitle("Number of males and females")+
  ylab("") + theme_bw()

# 4.2
df$height_cat = df$heightIn < 62
ggplot(df, aes(sex)) + geom_bar() + facet_wrap(~height_cat)


# 4.3 
df$height_cat = cut(df$heightIn, breaks = c(0, 62, Inf))
ggplot(df, aes(sex)) + geom_bar() + facet_wrap(~height_cat)

ggplot(df, aes(sex, fill = height_cat)) + geom_bar()
# for 100%
ggplot(df, aes(sex, fill = height_cat)) + geom_bar(position = 'fill')

# separated
ggplot(df, aes(sex, fill = height_cat)) + geom_bar(position = 'dodge')


# TODO average weight per gender
mean(df$weightLb)

mean(df[df$sex =='f', 'weightLb'])
mean(df[df$sex =='m', 'weightLb'])

aggregate(weightLb ~ sex, FUN = mean, data = df)


subset(df, sex == 'f')
df
## tidyverse, dplyr

# data.table

library(data.table)
install.packages("data.table")

dt <- data.table(df)
dt
str(dt)
dt[1:5]
df[1:5,]

# dt is less coding, like comma
dt[sex =='f'][1:5]

dt[ageYear == min(ageYear)]
# order 
dt[ageYear == min(ageYear)][order(bmi)]

# dt[i,j]
dt[,mean(heightIn)]

dt[,hist(heightIn)]
dt[sex == 'm',mean(heightIn)]
dt[sex == 'f',mean(heightIn)]

# dt [i,j, by =]
dt[,mean(heightIn), by = sex]
dt[,list(heightIn =mean(heightIn)), by = sex]
dt[,list(heightIn =mean(heightIn),weightLb = mean(weightLb)), by = sex]
dt[,list(heightIn =mean(heightIn),weightLb = mean(weightLb)),
   by = list(gender = sex,height_cat)]

## TODO new variable: elementary school = age < 14
# median weight for elementary vs non-elementary
# draw 5 random students from dt 
dt$elementary <- dt$ageYear < 14
# another way
dt[,elementary := ageYear<14]
dt$elementary <- cut(dt$ageYear, breaks = c(0,14,18))

#2 
dt[,list(median(weightLb)), by  = list(elementary)]

#3
random_students <- dt[sample(nrow(dt), 5), ]
random_students
# another way
dt[.N]
dt[,.N]

?sample

sample(1:10,2)
# .N is number of rows or columns
set.seed(100)
dt[sample(1:.N,5)]

# runif

runif(3, min = 10, max = 212)
round(runif(3, min = 10, max = 212))


?fread
booking <- fread("http://bit.ly/CEU-R-hotels-2018-prices")
booking

## TODO count the number of bookings below 100 EUR
## TODO count the number of bookings below 100 EUR without an offer
## TODO avg price of bookings below 100 EUR

## TODO avg price of bookings on weekdays
## TODO avg price of bookings on weekends
## TODO include nnights, holiday and year in the aggregate variables
## TODO compute the average price per number of stars

#1
nrow(booking[booking$price < 100, ])
#or
booking[price<100,.N]
#2
booking[offer == 0][price<100,.N]
booking[offer == 0 & price<100,.N]
#3
booking[price<100, mean(price)]
#4
booking[weekend == 0][price<100, mean(price)]
#5
booking[weekend == 1][price<100, mean(price)]
#or 
booking[,mean(price), by = weekend]

#6
booking[,mean(price), by = list(weekend,nnights,holiday,year)]
#7
feature <- fread('http://bit.ly/CEU-R-hotels-2018-features')

?merge

merged_df <- merge(booking, feature)
# to find hotels that are not in feature dataset
booking[!hotel_id %in% feature$hotel_id]
#7 answer
merged_df[,mean(price), by = stars][order(-stars)]
#or 
merged_df[,mean(price), by = stars][order(stars, decreasing = TRUE)]



