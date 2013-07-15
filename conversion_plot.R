library(ggplot2)
library(reshape)
library(scales)

# Make a plot with current conversion rate on the x-axis and sample size on the y-axis
# This will tell us the sample size needed spot a 10% decrease in conversion rate 

# Use power.prop.test to generate necessare sample sizes
# p1 is the current conversion rate, and p2 is a 10% decrease of that rate
# I set the power to be either .8 or .9
# We are using a 2 tailed test, with a 95% significance level
df <- data.frame()
for(i in 100:300){
  df[i,1] <- i/10000
  df[i,2] <-   power.prop.test(p1=i/10000, p2=0.9*(i/10000), power=0.8, alternative='two.sided', sig.level=0.05)[1]
  df[i,3] <-   power.prop.test(p1=i/10000, p2=0.9*(i/10000), power=0.9, alternative='two.sided', sig.level=0.05)[1]
}

# Prep data for plotting
df <- df[100:nrow(df),]
colnames(df) <- c("V1", "Power = .80", "Power = .90")
df2 <- melt(df, id="V1")

#Plot the data
plot <- ggplot(df2, aes(x=V1, y =value, colour=variable)) + geom_line()
plot <- plot + scale_y_continuous(breaks = round(seq(40000, 200000, by = 20000),1)) + ylab("N Of Each Group") + xlab("Current Conversion Rate")+scale_colour_manual(values=c("black", "red")) + labs(title="Sample Size To Stop A/B Test (10% Decrease)")
plot

