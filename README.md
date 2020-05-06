# 251-secondlab
This lab allowed for us to analyze data by splitting in into parts and conducting confidence intervals for both. 


myfile <- file.choose()
height.data <- read.csv(myfile,header=TRUE)

summary(height.data)

height.female <- height.data[which(height.data$gender=='F'),]
summary(height.female)

height.male <- height.data[which(height.data$gender=='M'),]
summary(height.male)

male.mean <- mean(height.male$height)
sd.male <- sd(height.male$height)
male.ss <- length(height.male$height)

female.mean <- mean(height.female$height)
female.sd <- sd(height.female$height)
female.ss <- length(height.female$height)


hist(height.male$height)
hist(height.female$height)

var.test(height.female$height,height.male$height)

spooled <- sqrt(((female.ss-1)*female.sd^2+(male.ss-1)*sd.male^2)/(male.ss+female.ss-2))
spooled

se <- spooled * sqrt(1/female.ss+1/male.ss)
se

df <- female.ss + male.ss - 2
df

t.star <- qt(0.975,df)
t.star

margin.error <- t.star*se
margin.error

x.diff <- female.mean - male.mean
x.diff

CI95 <- x.diff + c(-margin.error, margin.error)
CI95
