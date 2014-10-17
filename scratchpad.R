################################################################################
## PROJECT SCRATCHPAD TO TRY OUT THINGS
################################################################################

# Try reading in the data with a simple read.csv call
activities = read.csv("activity.csv")
activities$date = as.Date(activities$date)
head(activities)
str(activities)

# now lets try plyr and dplyr to summarise what we've got
library('plyr')
library('dplyr')
activities <- tbl_df(activities)
activities
dailyStepsTally <- activities %>% 
    filter(!is.na(steps)) %>%
    group_by(date) %>% 
    summarise(totalSteps = sum(steps))
dailyStepsTally

# Q1 What are total steps in a day
# Lets see what it looks like on a plot in qplot
library(ggplot2)
qplot(date, totalSteps, data=dailyStepsTally)

# Hmmmm yeah....
# gives the answer but not really what I was looking for.
# Lets try the full ggplot to get a bar chart
thePlot <- ggplot(subset(dailyStepsTally, !is.na(totalSteps)), aes(date, totalSteps))
thePlot + geom_bar(stat="identity", aes(fill=totalSteps))

# better, now lets tidy it up a bit
last_plot() + ggtitle("Total Daily Steps")
last_plot() + xlab("Date")
last_plot() + ylab("Number of Steps")
last_plot() + theme(axis.text.x=element_text(angle=90))

# Now to calculate the mean and median of these steps
with (dailyStepsTally, {
    print(median(totalSteps))
    print(mean(totalSteps)) 
})


# Oh yeah baby that sanswers my first question so lets move on to....

# Q2