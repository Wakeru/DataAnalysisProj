install.packages(c("Cairo", "GISTools","RColorBrewer"))
library(RColorBrewer)
library(GISTools)
library(Cairo)

colours <- brewer.pal(18, "Set3")
coloursT <- add.alpha(colours, .6)

#The data has 1000 people which shows how much peoople watch them, how mcuh they stream, when was their hieghest peak of view and ect
#To determine who is the top streamer, it needs to be determined by how much people watch them, not how much people spent time on their stream.
Data <- read.csv("twitchdata-update.csv")

#Question 1


#Easy enough of course
#Here is the top streamer
max(Data$Average.viewers)
#which gices us the number of 147643
#BUt that only gives us the number, not the channel
#So I get the row matching that number.
Data[Data$Average.viewers == 147643,]
#the top Streamer is "dota2ti"
#Pretty shocked but this data is old
#I wonder how Xqc is, considering he is the top these days.
(Data$Channel)
Data[Data$Channel == "xQcOW",]
#Lets see who is the top for Watch time, and followers.
max(Data$Watch.time.Minutes)
# Watch time is 6196161750
max(Data$Followers)
# followers is 8938903
#Lets see who are they
Data[Data$Watch.time.Minutes == 6196161750,]
Data[Data$Followers == 8938903,]
#Top watch time is "xQcOW"
# top followers is "Tfue"

#Question 2
#Lets put these top three streamers side by side with a histogram??
#First, lets take out irrelevant info, like Partnered, Peak Viewers, Folowers gained, and views gained.
#I am somwhat going for like a very simplified way of comparing the three.
#im going to make all three histograms either side by side or overlapping
NData <- Data[,-c(3:4,7:11)] #removes everything but the three factors
#The reason i removed "Channel" is so i can make the histogram much easier and since we have the specified row of the three.
#It would make things complicated plus i could just add their names in the histogram using mtext or something
summary(NData)
# NOTE: THESE ARE NOT HISTOGRAMS, took a long time to wonder why it was so hard but IT IS A BARPLOT TYPE GRAPH
Xqc <- NData[NData$Watch.time.Minutes == 6196161750,] #row 1
Tfue <- NData[NData$Followers == 8938903,] #Row 5
Dota <- NData[NData$Average.viewers == 147643,]# Row 85
WData <- NData[-c(2:4, 6:84, 86:1000),]

#It keeps saying its wrong but i can do other types of numbers???
#par(mfrow = c(1:3))
#nope
#testing <- cbind(a,b,c)
#barplot(testing, beside = T)
#Ill just work on each one individually i guess


par(lwd = 2, mai = c(.5, 1.5 ,1, .75))
#mat <- matrix(c(1,1,2,2), nrow = 2, ncol = 2, byrow = TRUE)
#layout(mat, heights = c(1,.5))

#Figure 1
barplot(WData$Followers, xaxt = "n", yaxt = "n",col = colours[1:3])
legend("topright", legend = c("xQcOW", "Tfue", "Dota2ti"), col = colours[1:3], pch = 19, pt.cex = 1.2, box.lwd = 2)
axis(side = 2, las = 2)
mtext("Followers between the Streamers", 3, line =2, font = 2)
mtext("Followers", 2, line = 5, cex = 1.5)

#mtext("Followers", 1, line = 2, font = 2)

#Figure 2
barplot(WData$Watch.time.Minutes, xaxt = "n", yaxt = "n",col = colours[1:3])
legend("topright", legend = c("xQcOW", "Tfue", "Dota2ti"), col = colours[1:3], pch = 19, pt.cex = 1.2, box.lwd = 2)
axis(side = 2, las = 2)
mtext("Watch-Time between the Streamers (Minutes)", 3, line =2, font = 2)
mtext("Minutes", 2, line = 5, cex = 1.5)
#I could do log10() but it would not look well


#Figure 3
barplot(WData$Average.viewers, xaxt = "n", yaxt = "n",col = colours[1:3])
legend("topright", legend = c("xQcOW", "Tfue", "Dota2ti"), col = colours[1:3], pch = 19, pt.cex = 1.2, box.lwd = 2)
axis(side = 2, las = 2)
mtext("Average Viewers between the Streamers", 3, line =2, font = 2)
mtext("Viewers", 2, line = 5, cex = 1.5)




#Question 3
#Now that we compared the three in those three factors, we need to know if mature streamers are better than family friendly streamersd, essentially.
#First check that its only true and FALSE
unique(Data$Mature)
#IT is
#so now we check how much there is mature and family friendly
table(Data$Mature)
#FALSE = 770
#TRUE = 230
#Figure 4
par(lwd = 2, mai = c(.5, 1.5 ,1, .75))
barplot(table(Data$Mature),main = "Mature vs Family Friendly Streamers", xaxt = "n", col = colours[4:5])
mtext("Streamers", 2, line = 4, cex = 1.5)
legend("topright", legend = c("Family Friendly", "Mature (18+)"), col = colours[4:5], pch = 19, pt.cex = 1.2, box.lwd = 2)


MData <- Data[Data$Mature == "True",]
sum(MData$Average.viewers) #809315
sum(MData$Followers) #101161076

FData <- Data[Data$Mature == "False",]
sum(FData$Average.viewers) # 3971725
sum(FData$Followers) #468892992


#now lets make a barplot of it
#Figure 5
par(lwd = 2, mai = c(.5, 1.5 ,1, .75))
barplot(c(468892992,101161076), main = "Mature vs Family Friendly Streamers(Sum of Followers)", xaxt = "n", col = colours[4:5])
mtext("Followers", 2, line = 4, cex = 1.5)
legend("topright", legend = c("Family Friendly", "Mature (18+)"), col = colours[4:5], pch = 19, pt.cex = 1.2, box.lwd = 2)
#Quick shocking, i thought there would be more Mature streamers since the top streamers are only for Mature audiences

#Figure 6
par(lwd = 2, mai = c(.5, 1.5 ,1, .75))
barplot(c(3971725,809315), main = "Mature vs Family Friendly Streamers(Sum of Average Viewers)", xaxt = "n", col = colours[4:5])
mtext("Viewers", 2, line = 4, cex = 1.5)
legend("topright", legend = c("Family Friendly", "Mature (18+)"), col = colours[4:5], pch = 19, pt.cex = 1.2, box.lwd = 2)

#THE END
