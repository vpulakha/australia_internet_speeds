# Install the packages required to generate the Infographic

library(data.table)
library(ggplot2)
library(htmltab)
library(stringr)
library(ggrepel)
library(ggflags) # Note: ggflags is not available in CRAN.  So, please install from github devtools::install_github('rensa/ggflags')
library(countrycode)

#Download the data from the Wikipedia page
internet_speeds <- as.data.table(htmltab("https://en.wikipedia.org/wiki/List_of_countries_by_Internet_connection_speeds",1))

# Rename the columns
names(internet_speeds) <- c("Rank", "Country", "ookla", "m-lab", "net.io")

#Get rid of leading spaces in the Country Column
internet_speeds$Country <- str_trim(internet_speeds$Country)

# Order the data by Rank
internet_speeds <- internet_speeds[order(as.numeric(internet_speeds$Rank)), ]
internet_speeds$Country <- factor(internet_speeds$Country, levels = internet_speeds$Country)  # to retain the order in plot


#subset for specific countries
internet_speeds_subset <- internet_speeds[Country %in% c("Australia", "United States", "Canada", "New Zealand", 
                                                         "Japan", "China", "Singapore", "Germany", "South Korea", "United Kingdom")]

internet_speeds_subset$Country <- gsub("Australia", "AU", internet_speeds_subset$Country)
internet_speeds_subset$Country <- gsub("Canada", "CA", internet_speeds_subset$Country)
internet_speeds_subset$Country <- gsub("Japan", "JP", internet_speeds_subset$Country)
internet_speeds_subset$Country <- gsub("New Zealand", "NZ", internet_speeds_subset$Country)
internet_speeds_subset$Country <- gsub("United States", "US", internet_speeds_subset$Country)
internet_speeds_subset$Country <- gsub("China", "CN", internet_speeds_subset$Country)
internet_speeds_subset$Country <- gsub("Singapore", "SG", internet_speeds_subset$Country)
internet_speeds_subset$Country <- gsub("Germany", "DE", internet_speeds_subset$Country)
internet_speeds_subset$Country <- gsub("South Korea", "KR", internet_speeds_subset$Country)
internet_speeds_subset$Country <- gsub("United Kingdom", "GB", internet_speeds_subset$Country)

# Convert Country to lowercase
internet_speeds_subset$code <- tolower(internet_speeds_subset$Country)

# Order the data by Rank
internet_speeds_subset <- internet_speeds_subset[order(as.numeric(internet_speeds_subset$Rank)), ]
internet_speeds_subset$Country <- factor(internet_speeds_subset$Country, levels = internet_speeds_subset$Country)  # to retain the order in plot.

# Plot the data
p1 <- ggplot(internet_speeds_subset, aes(x=Country, y=as.numeric(ookla), fill=Country, country=code, size=5 ))+
  geom_bar(stat='identity', show.legend = FALSE)+ 
  scale_fill_manual(values = c("AU" = "red",
                               "CA" = "#1b98e0",
                               "NZ" = "#1b98e0",
                               "JP" = "#1b98e0",
                               "US" = "#1b98e0", 
                               "CN" = "#1b98e0",
                               "SG" = "#1b98e0",
                               "DE" = "#1b98e0",
                               "KR" = "#1b98e0",
                               "GB" = "#1b98e0")) + 
  geom_label_repel(data=internet_speeds_subset, aes(label=Rank))+
  geom_flag()+
  scale_country()+scale_size(range = c(0, 15))+scale_y_continuous(limits = c(0, 250), breaks = c(0, 25,50,75,100,125,150,175,200, 225, 250))+
  labs(subtitle="Internet Download Speeds - ranked by Speedtest.net data for April 2021", 
       y="Average Download Speed (Mbits/s)", 
       x="Country", 
       title=paste0("Australia's Fixed Broadband Internet Speeds - Ranked #",  internet_speeds_subset[Country == "AU"]$Rank , " in the World"), 
       caption="Source: Wikipedia - https://en.wikipedia.org/wiki/List_of_countries_by_Internet_connection_speeds")+
     theme(plot.title = element_text(face="bold"))+ theme(legend.position="none")+theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent'))

# Order the data by Rank
internet_speeds <- internet_speeds[order(as.numeric(internet_speeds$Rank)), ]
internet_speeds$Country <- factor(internet_speeds$Country, levels = internet_speeds$Country)  # to retain the order in plot


# Plot the inset graph

p2 <- ggplot(internet_speeds, aes(Country, as.numeric(ookla)))+geom_jitter(col="#1b98e0")+theme_light()+
  theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent')
  )+ theme(axis.title.x=element_blank(),
           axis.text.x=element_blank(),
           axis.ticks.x=element_blank())+
  labs( y="Average Download Speed (Mbits/s)", 
          subtitle="Broadband Internet Connection Speed by Country")+theme(axis.text=element_text(size=6),
                                                axis.title=element_text(size=5,face="bold"))+
  geom_point(data=internet_speeds[Country=="Australia"], 
aes(x=Country,y=as.numeric(ookla)), 
color='red',
size=2)+geom_label_repel(data=internet_speeds[Country=="Australia"], aes(label=Country))
  


# Plot the both the Main graph and the inset graph
p1 + annotation_custom(ggplotGrob(p2), xmin = 5, xmax = 10, 
                       ymin = 200, ymax = 260)


