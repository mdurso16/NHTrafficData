setwd("/Users/MeaganDurso/Desktop/Graduate Certificate in Analytics/R Files")
library(readr)
mydata<-read_csv(file="NH_cleaned.csv")
library(Hmisc)
describe(mydata)
library(dplyr)
View(mydata)

#Adding a column, "Weekday" that converts the date of stop to a Weekday day
library(lubridate)
mydata$Weekday <- lubridate::wday(as.Date(mydata$stop_date), label=TRUE, abbr=TRUE)
class(mydata$Weekday)

#Creating barplot, "vbyc" that Shows All Violations by County
vbyc<- ggplot(mydata,aes(x=location_raw))+geom_bar(colour="black", fill="darkblue")
vbyc<- vbyc + ggtitle("Bar Chart of Traffic Stops Per County")
vbyc<- vbyc+ theme_classic()
vbyc<- vbyc + theme(text = element_text(size=10),
                    axis.text.x = element_text(angle=45, hjust=1))
vbyc<- vbyc+ theme(plot.title=element_text(size=16,face="bold", hjust=0.5))                    
vbyc<-vbyc + geom_text(stat = 'count', aes(label=..count..),vjust=-1, size=3) +xlab("County") +ylab("Frequency")
vbyc
# (Note: Merrimack and Rockingham County have the greatest # of violations per "vbyc" bar chart)


#Creation of values that shows the Total Number of Tickets and the total number
# of tickets in Merrimack and Rockingham County specifically
Tickets<-nrow(filter(mydata,stop_outcome=="Ticket"))
nrowM<-nrow(filter(mydata,stop_outcome=="Ticket"& county_name=="Merrimack County"))
nrowR<-nrow(filter(mydata,stop_outcome=="Ticket"& county_name=="Rockingham County"))

#Summarizing Top Violation Types by County for Rockingham and Merrimack Counties and storing in dataframe

Rtable<-mydata%>%
  filter(county_name!="")%>%
  filter(county_name=="Rockingham County")%>%
  group_by(violation)%>%
  summarise(countrv=n(), v2=((n()/nrowR)*100)) %>%
  arrange(desc(countrv))

Mtable<-mydata%>%
  filter(county_name !="")%>%
  filter(county_name=="Merrimack County")%>%
  group_by(violation)%>%
  summarise(v=n(),v3=n()/nrowM)%>%
  arrange(desc(v))

#(Note: Results show that Speeding is the Number 1 Violation in both Merrimack and Rockingham County)


#Extracting the town string from the "fine_grained location" and storing as new column, town1
library(stringr)
mydata$town<-mydata$fine_grained_location
mydata$town1<-word(mydata$town,1)


# Creation of dataframes that summarizes counts of speeding tickets by town within each county
#Also gives % of Speeding Tickets Given within that location 

Merrimack<-mydata%>%
  filter(county_name !="")%>%
  filter(county_name=="Merrimack County")%>%
  filter(violation=="Speeding")%>%
  filter(stop_outcome=="Ticket")%>%
  group_by(town1)%>%
  summarize(Countm=n(),Percentage=((n()/nrowM)*100))%>%
  arrange(desc(Countm))


Rockingham<-mydata%>%
  filter(county_name !="")%>%
  filter(county_name=="Rockingham County")%>%
  filter(violation=="Speeding")%>%
  filter(stop_outcome=="Ticket")%>%
  group_by(town1)%>%
  summarize(Countr=n(),Percentage=((n()/nrowR)*100))%>%
  arrange(desc(Countr))


# Creation of dataframe that filters out rows for speeding tickets in Merrimack County and groups by Town 
#Addition of a column that shows number of speeding tickets within this data frame
Me<-mydata%>%
  filter(county_name!="")%>%
  filter(county_name=="Merrimack County")%>%
  filter(violation=="Speeding")%>%
  filter(stop_outcome=="Ticket")%>%
  filter(!is.na(Weekday))%>%
  group_by(town1)%>%
  mutate(freq=n())%>%
  arrange(desc(freq))

#Changing the Weekday column to an ordered factor and arranging Weekday to correct order of week
Me$Weekday<-factor(Me$dayofweek,levels =as.character(wday(1:7),label=TRUE,abbr=FALSE))
class(Me$Weekday)

##Bar Plot of Merrimack County data using the Me data frame. Plotted the Top 3 Towns with Highest Number of Speeding Tickets by Day of Week
merplot<- ggplot(data=subset(Me,town1 %in% c("HOPKINTON", "CONCORD","BOW")),aes(x=town1, fill=Weekday))+geom_bar()
merplot<- merplot + geom_text(stat = 'count', aes(label=..count..),position=position_stack(vjust=0.5), size=3) +xlab("Town") +ylab("Frequency")
merplot<-merplot + theme_classic()
merplot<-merplot+ theme(plot.title=element_text(size=12,face="bold", hjust=0.5))
merplot<-merplot+ ggtitle("Speeding Tickets in Merrimack County by Top 3 Towns")
merplot


# Creation of dataframe that filters out rows for speeding tickets in Rockingham County and groups by Town 
#Added a column that shows number of speeding tickets
R<-mydata%>%
  filter(county_name !="")%>%
  filter(county_name=="Rockingham County")%>%
  filter(violation=="Speeding")%>%
  filter(stop_outcome=="Ticket")%>%
  filter(!is.na(Weekday))%>%
  group_by(town1)%>%
  mutate(frequency=n())%>%
  arrange(desc(frequency))

#Bar Plot of Rockingham County data using dataframe R. Plotted the Top 3 Towns with Highest Number of Speeding Tickets by Day of Week
rplot<- ggplot(data=subset(R,town1 %in% c("GREENLAND","HAMPTON", "SALEM")),aes(x=town1, fill=Weekday))+geom_bar()
rplot<- rplot + geom_text(stat = 'count', aes(label=..count..),position=position_stack(vjust=0.5), size=3) +xlab("Town") +ylab("Frequency")
rplot<-rplot+ggtitle("Speeding Tickets in Rockingham County by Top 3 Towns")
rplot<-rplot + theme_classic()
rplot<-rplot+ theme(plot.title=element_text(size=12,face="bold", hjust=0.5))
rplot

Merrimack
Rockingham

