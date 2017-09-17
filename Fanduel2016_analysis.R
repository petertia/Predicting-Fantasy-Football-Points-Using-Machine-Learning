# Fanduel 2016 Data Analysis

# look at summary stats of top 20 qbs in week 1
week1 <- X2016_Fanduel_Results[which(X2016_Fanduel_Results$Week == 1),]
week1qb <- week1[which(week1$Pos == "QB"),]
mean(week1qb$`FD points`[1:20])
sd(week1qb$`FD points`[1:20])

# summary stats of projected top 20 qbs in week 1
proj_week1 <-FD_2016_Projections[which(FD_2016_Projections$week==1),]
proj_week1qb <- proj_week1[which(proj_week1$position=="QB"),]
mean(proj_week1qb$proj[1:20])
sd(proj_week1qb$proj[1:20])

#hypotheses to test
#1. qbs facing weaker defenses outperform projections
#2. certain qbs consistenly outperform projections


# change last name, first name to first name last name
week1qb$Name_mod <- sub("(\\w+),\\s(\\w+)","\\2 \\1", week1qb$Name)

#merge projected points and actual points
combined_week1qb <- merge(week1qb, proj_week1qb, by.x=c("Name_mod","Week"), by.y=c("player","week"))

#calculate the difference between projection and actual performance in fantasy points
combined_week1qb$point_diff <- combined_week1qb$`FD points` - combined_week1qb$proj

#order the data frame by largest differential between projection and actual
sorted_def <- combined_week1qb[order(combined_week1qb$point_diff, decreasing = TRUE),]

#create vector of names of qb
qb_df <- X2015_Fanduel_Results[which(X2015_Fanduel_Results$Pos=="QB"),]
qb_vector <- unique(qb_df$Name)
qb_points <-data.frame(qb_vector)

#find total scored points in 2015 season by qb
qb_total <- vector(mode="double",length=length(qb_vector))
qb_points["qb_total"] <- qb_total

sum(qb_df[which(qb_df$Name=="Newton, Cam"),]$'FD points')

#find total home scored points in 2015 season by qb

#find total away scored points in 2015 season by qb

#calculate the average points scored in 2015 for each qb
