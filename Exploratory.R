#import csv file
df <- read.csv(file = "online_shoppers_intention.csv")
View(df)

#descriptive statistics
str(df)
head(df)
summary(df)

#removing duplicates
df_duplicate <- nrow(df[duplicated(df),])
df <- df[!duplicated(df),]
str(df)

#identification of missing values
which(is.na(df))

#Renaming June to Jun for convenience of plotting
df$Month <- as.character(df$Month)
df$Month[df$Month == "June"] <- "Jun"
df$Month <- as.factor(df$Month)
df$Month = factor(df$Month, levels = month.abb)

#*******************************************************************************
#EXPLORATORY DATA ANALYSIS

install.packages("ggplot2")
library(ggplot2)
#Administrative pages: number of pages visited
plot1 <- ggplot(df, aes(x=1, y=Administrative)) + geom_violin() + geom_violin(trim=FALSE, fill='#E69F00', color='gray') + coord_flip() + labs(x = " ") + labs(y = "Number of Administrative pages visited") + theme(axis.text.y = element_blank(), axis.ticks = element_blank())
#Administrative pages: total time spent
plot2 <- ggplot(df, aes(x=1, y=Administrative_Duration)) + geom_violin() + geom_violin(trim=FALSE, fill='#E69F00', color='gray') + coord_flip() + labs(x = " ") + labs(y = "Total time spent in Administrative pages") + theme(axis.text.y = element_blank(), axis.ticks = element_blank())

#Informational pages: number of pages visited
plot3 <- ggplot(df, aes(x=1, y=Informational)) + geom_violin() + geom_violin(trim=FALSE, fill='#56B4E9', color='gray') + coord_flip() + labs(x = " ") + labs(y = "Number of Informational pages visited") + theme(axis.text.y = element_blank(), axis.ticks = element_blank())
#Informational pages: total time spent
plot4 <- ggplot(df, aes(x=1, y=Informational_Duration)) + geom_violin() +  geom_violin(trim=FALSE, fill='#56B4E9', color='gray') + coord_flip() + labs(x = " ") + labs(y = "Total time spent in Informational pages") + theme(axis.text.y = element_blank(), axis.ticks = element_blank())

#Product related pages: number of pages visited
plot5 <- ggplot(df, aes(x=1, y=ProductRelated)) + geom_violin() + geom_violin(trim=FALSE, fill='#FF9999', color='gray') + coord_flip() + labs(x = " ") + labs(y = "Number of ProductRelated pages visited") + theme(axis.text.y = element_blank(), axis.ticks = element_blank())
#Product related: total time spent
plot6 <- ggplot(df, aes(x=1, y=ProductRelated_Duration)) + geom_violin() + geom_violin(trim=FALSE, fill='#FF9999', color='gray') + coord_flip() + labs(x = " ") + labs(y = "Total time spent in ProductRelated pages") + theme(axis.text.y = element_blank(), axis.ticks = element_blank())

install.packages("gridExtra")
library(gridExtra)
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, nrow = 3, ncol = 2)

#Side-by-side comparison charts
plot1 <- ggplot(df, aes(x=Revenue, y=Administrative)) + geom_violin() + geom_violin(trim=FALSE, fill='#E69F00', color='gray') + labs(x = "Administrative") + labs(y = " ") + theme(axis.text.y = element_blank(), axis.ticks = element_blank())
plot4 <- ggplot(df, aes(x=Revenue, y=Administrative_Duration)) + geom_violin() + geom_violin(trim=FALSE, fill='#E69F00', color='gray') + labs(x = "Administrative_Duration") + labs(y = " ") + theme(axis.text.y = element_blank(), axis.ticks = element_blank())
plot2 <- ggplot(df, aes(x=Revenue, y=Informational)) + geom_violin() + geom_violin(trim=FALSE, fill='#56B4E9', color='gray') + labs(x = "Informational") + labs(y = " ") + theme(axis.text.y = element_blank(), axis.ticks = element_blank())
plot5 <- ggplot(df, aes(x=Revenue, y=Informational_Duration)) + geom_violin() +  geom_violin(trim=FALSE, fill='#56B4E9', color='gray') + labs(x = "Informational_Duration") + labs(y = " ") + theme(axis.text.y = element_blank(), axis.ticks = element_blank())
plot3 <- ggplot(df, aes(x=Revenue, y=ProductRelated)) + geom_violin() + geom_violin(trim=FALSE, fill='#FF9999', color='gray') + labs(x = "ProductRelated") + labs(y = " ") + theme(axis.text.y = element_blank(), axis.ticks = element_blank())
plot6 <- ggplot(df, aes(x=Revenue, y=ProductRelated_Duration)) + geom_violin() + geom_violin(trim=FALSE, fill='#FF9999', color='gray') + labs(x = "ProductRelated_Duration") + labs(y = " ") + theme(axis.text.y = element_blank(), axis.ticks = element_blank())

grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, nrow = 2, ncol = 3)

install.packages("ggpubr")
library(ggpubr)

#BounceRates, ExitRates and PageValues
plot1 <- ggdensity(df, x = "BounceRates", fill = "thistle2", color = "thistle2", add = "median", rug = TRUE) + labs(y = " ")
plot2 <- ggdensity(df, x = "ExitRates", fill = "skyblue1", color = "skyblue1", add = "median", rug = TRUE) + labs(y = " ")
plot3 <- ggdensity(df, x = "PageValues", fill = "sienna3", color = "sienna3", add = "median", rug = TRUE) + labs(y = " ")
grid.arrange(plot1, plot2, plot3, nrow = 3)


plot1 <- ggplot(df, aes(x=BounceRates, fill=Revenue)) + geom_density(alpha=0.4) + labs(y = " ")
plot2 <- ggplot(df, aes(x=ExitRates, fill=Revenue)) + geom_density(alpha=0.4) + labs(y = " ")
plot3 <- ggplot(df, aes(x=PageValues, fill=Revenue)) + geom_density(alpha=0.4) + labs(y = " ")
grid.arrange(plot1, plot2, plot3, nrow = 3)

#Special and non-special days
plot1 <- ggplot(df, aes(x = factor(1), y = SpecialDay)) + geom_boxplot(width = 0.4, fill = "white") + geom_jitter(color = "deepskyblue4", width = 0.1, size = 1, alpha=0.4) + labs(x = "Special Day") + labs(y = "Closeness") + theme(axis.text.x = element_blank(), axis.ticks = element_blank())
plot2 <- ggplot(df, aes(x = Revenue, y = SpecialDay)) + geom_boxplot(width = 0.4, fill = "white") + geom_jitter(color = "deepskyblue4", width = 0.2, size = 1, alpha=0.4) + labs(x = "Special Day") + labs(y = " ") + theme(axis.ticks = element_blank())
grid.arrange(plot1, plot2, ncol = 2)

#Month-wise distribution
plot <- ggplot(data.frame(df), aes(Month, fill=Revenue)) + geom_bar() + labs(x = "Month") + labs(y = " ")
plot

#Categorization based upon OS, browser, region, traffic type, weekend and visitor type
plot1 <- ggplot(data.frame(df), aes(OperatingSystems, fill=Revenue)) + geom_bar() + labs(x = "Operating Systems") + labs(y = " ") + scale_x_continuous(breaks = 1:8)
plot2 <- ggplot(data.frame(df), aes(Browser, fill=Revenue)) + geom_bar() + labs(x = "Browser") + labs(y = " ") + scale_x_continuous(breaks = 1:13)
plot3 <- ggplot(data.frame(df), aes(Region, fill=Revenue)) + geom_bar() + labs(x = "Region") + labs(y = " ") + scale_x_continuous(breaks = 1:9)
plot4 <- ggplot(data.frame(df), aes(TrafficType, fill=Revenue)) + geom_bar() + labs(x = "Traffic Type") + labs(y = " ")
plot5 <- ggplot(data.frame(df), aes(Weekend, fill=Revenue)) + geom_bar() + labs(x = "Weekend") + labs(y = " ")
plot6 <- ggplot(data.frame(df), aes(VisitorType, fill=Revenue)) + geom_bar() + labs(x = "Visitor Type") + labs(y = " ") + scale_x_discrete(labels = c("New_Visitor" = "New", "Other" = "Other", "Returning_Visitor" = "Return"))
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, nrow = 3, ncol = 2)

#Target feature distribution
plot <- ggplot(data.frame(df$Revenue), aes(x=df$Revenue)) + geom_bar() + labs(x = "Target Feature Distribution")
plot