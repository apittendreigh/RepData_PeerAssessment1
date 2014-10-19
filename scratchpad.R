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

# Q2 What is the average daily activity pattern

#create an aggregate data set from the activities
activityMeans <- aggregate(steps ~ interval, data=activities, FUN=mean) 
activityMeans

#looks good so lets plot
qplot(interval, steps, data=activityMeans) + geom_line()

# OK lets put together a full ggplot
thePlot <- ggplot(activityMeans, aes(interval, steps))
thePlot + geom_line()
last_plot() + ggtitle("Average Steps over 5 Minute Time Intervals")
last_plot() + xlab("time Intervals") + ylab("Number of Steps")

#get the maximum number of steps from original source
steps <- activities %>% filter(!is.na(steps)) %>% arrange(steps)
tail(steps)
nrow(steps)
steps$interval[nrow(steps)]
format(steps$date[nrow(steps)], '%B %d, %Y')


# Now to question 3 Inputing Missing Values

# Check the number of na values
count(activities$date[is.na(activities$date)])
count(activities$interval[is.na(activities$interval)])
count(activities$steps[is.na(activities$steps)])

# Try replacing the NA values with mean of all non na steps
activities.complete <- activities %>%
    group_by(interval) %>%
    mutate(steps = ifelse(is.na(steps), round(mean(steps, na.rm = T), 0), steps))

count(activities.complete$steps[is.na(activities.complete$steps)])
View(activities.complete)

# all looks complete - at least no more NA's, but lets try 
# something a little smarter

# lets do a median for each time interval
med.act <- activities %>% 
    filter(!is.na(steps)) %>%
    group_by(interval) %>% 
    summarise(medianSteps = median(steps))

# And substitude NAs with values from the median dataframe
activities.complete <- activities %>%
    group_by(interval) %>%
    mutate(steps = ifelse(is.na(steps), med.act$medianSteps[interval = med.act$interva], steps))

table(is.na(activities.complete$steps))

# not working - it's still leaving in NA values
# due time cinstrainsts go with first option


# On to Q4 differences between qweekdays and weekendsx

# Indicate if each day in the data set is wekday or weekend
activities.complete <- activities.complete %>%
    mutate(dayType = ifelse(weekdays(date) == 'Saturday' | 
                            weekdays(date) == 'Sunday', 'Weekend', 'Weekday'))
activities.complete$dayType <- as.factor(activities.complete$dayType)    

# Summarise the data
stepsTally <- activities.complete %>%
    group_by(dayType, interval) %>%
    summarise(avgSteps = mean(steps))

# got the data set now lets plot
ggplot(stepsTally, aes(interval, avgSteps)) +
    geom_line() +
    facet_grid( dayType ~ .) + 
    ggtitle("Average Steps over 5 Minute Time Intervals") + 
    xlab("time Intervals") + 
    ylab("Number of Steps")