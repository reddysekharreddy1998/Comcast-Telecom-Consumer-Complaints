#Importing necessary packages
library(dplyr)
library(ggplot2)
library(lubridatdata)

#Importing Comcast Datase
comcast_data<- read.csv(file.choose(),header = TRUE)

#Manipulating column names
library(stringi)
names(comcast_data)<- stri_replace_all(regex =  "\\.",replacement = "",str =names(comcast_data))
View(comcast_data)

na_vector <- is.na(comcast_data)
length(na_vector[na_vector==T])

#Processing Date
comcast_data$Date<- dmy(comcast_data$Date)
View(comcast_data)

#Now we need to get the complaints on a daily level basis and plot a trend chart for it.

dlb<-comcast_data %>% group_by(Date) %>% summarize(NumOfComplaints=n())
#Plotting for daily granularity level
ggplot(data = dlb,aes(as.POSIXct(Date),NumOfComplaints))+
  geom_line()+
  geom_point(size = 1)+
  scale_x_datetime(breaks = "1 weeks",date_labels = "%d/%m")+
  labs(title = "Daily Ticket Count",x= "Days",y ="No. of Tickets")+
  theme(axis.text.x = element_text(angle = 75),
        plot.title = element_text(hjust = 0.5))

#Now we need to get the complaints on a monthly level basis and plot a trend chart for it.
#Making month field
comcast_data$Month<-months(comcast_data$Date)
mlb<-comcast_data %>% group_by(Month =as.integer(month(Date))) %>% summarize(NumOfComplaints=n()) %>% arrange(desc(NumOfComplaints))
#Plotting for monthly granularity level
ggplot(data = mlb,aes(Month,NumOfComplaints,label = NumOfComplaints))+
  geom_line()+
  geom_point(size = 0.8)+
  geom_text()+
  scale_x_continuous(breaks = mlb$Month)+
  labs(title = "Monthly Ticket Count",x= "Months",y ="No. of Tickets")+
  theme(plot.title = element_text(hjust = 0.5))

#Now we need to make a frequency table basis the complaint types.

# Complaint Type Processing
network_tickets<- contains(comcast_data$CustomerComplaint,match = 'network',ignore.case = T)
internet_tickets<- contains(comcast_data$CustomerComplaint,match = 'internet',ignore.case = T)
billing_tickets<- contains(comcast_data$CustomerComplaint,match = 'bill',ignore.case = T)
email_tickets<- contains(comcast_data$CustomerComplaint,match = 'email',ignore.case = T)
charges_ticket<- contains(comcast_data$CustomerComplaint,match = 'charge',ignore.case = T)
comcast_data$ComplaintType[internet_tickets]<- "Internet"
comcast_data$ComplaintType[network_tickets]<- "Network"
comcast_data$ComplaintType[billing_tickets]<- "Billing"
comcast_data$ComplaintType[email_tickets]<- "Email"
comcast_data$ComplaintType[charges_ticket]<- "Charges"
comcast_data$ComplaintType[-c(internet_tickets,network_tickets,
                              billing_tickets,charges_ticket,email_tickets)]<- "Others"
table(comcast_data$ComplaintType)

#Now we need to make a new categorical variable for Complaint Status.

open_complaints<-(comcast_data$Status == 'Open' | comcast_data$Status == 'Pending')
closed_complaints<-(comcast_data$Status == 'Closed' | comcast_data$Status == 'Solved')
comcast_data$ComplaintStatus[open_complaints]<-'Open'
comcast_data$ComplaintStatus[closed_complaints]<-'Closed'

#Now we need to plot state wise status of complaints in a stacked bar chart.

stack<-table(comcast_data$ComplaintStatus,comcast_data$State)
stack
comcast_data<- group_by(comcast_data,State,ComplaintStatus)
chart_data<- summarise(comcast_data,Count = n())
#Plotting on stacked bar chart
ggplot(as.data.frame(chart_data) ,mapping = aes(State,Count))+
  geom_col(aes(fill = ComplaintStatus),width = 0.95)+
  theme(axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        title = element_text(size = 16,colour = "#0073C2FF"),
        plot.title = element_text(hjust =  0.5))+
  labs(title = "Ticket Status Stacked Bar Chart ",
       x = "States",y = "No of Tickets",
       fill= "Status")

#Now we need to see which state has maximum unresolved complaints

comcast_data %>% filter(ComplaintStatus=='Open') %>% group_by(State) %>% summarize(NumOfComplaints=n()) %>% arrange(desc(NumOfComplaints))

#Now we want to see the percentage of resolved complaints.

tot<-comcast_data %>% group_by(ComplaintStatus) %>% summarize(NumOfComplaints=n())
tot
slices<-tot$NumOfComplaints
pct<-round((slices/sum(slices)*100),2)
lbls<-paste(tot$ComplaintStatus," ",pct,"%",sep="")
#Plotting pie chart
pie(slices,labels=lbls)



int<-comcast_data %>% filter(ReceivedVia=='Internet',ComplaintStatus=='Closed') %>% group_by(ReceivedVia,ComplaintStatus) %>% summarize(NumOfComplaints=n()) 
ccc<-comcast_data %>% filter(ReceivedVia=='Customer Care Call',ComplaintStatus=='Closed') %>% group_by(ReceivedVia,ComplaintStatus) %>% summarize(NumOfComplaints=n()) 
#Percentage of resolved internet Complaints
intpct<-round(int$NumOfComplaints/sum(tot$NumOfComplaints)*100,2)
intpct
#Percentage of resolved Customer Care Call Complaints
cccpct<-round(ccc$NumOfComplaints/sum(tot$NumOfComplaints)*100,2)
cccpct


