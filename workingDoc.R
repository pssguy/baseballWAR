## just start on some baseball stuff

## cumulative war this year

# http://www.baseball-reference.com/teams/BOS/2015-roster.shtml

## maybe other than scrape Lahman does not have 2014 data yet

# library("XML2R") #Imports: RCurl, plyr - think about

library(XML)
library(dplyr)

u = "http://www.baseball-reference.com/teams/BOS/2015-roster.shtml"

tables = readHTMLTable(u, stringsAsFactors=FALSE)
names(tables) # [1] "40man"       "appearances"

active <- tables[[1]]
glimpse(active)

# from this table really only want to know if active, name andposition



active <- active[,c(1,3,5)]
colnames(active) <- c("Rank","Name","Category")
active$Rank <- as.integer(active$Rank)

active$active <- "T"

active[active$Rank>25,]$active <- "F"

## then want current year to get salary

current  <- tables[[2]]
glimpse(current)

colnames(current)

current <- current[,c(1,2,28)] # 27 players used to date

## for link should look at 

library(rvest)

page <- html("http://www.baseball-reference.com/teams/BOS/2015-roster.shtml")
#str(page)
class(u)
links <-html_nodes(page, "#appearances a") %>%
  html_attr("href") 

df<-current %>% 
  left_join(active)


df$link <- links

#http://www.baseball-reference.com/players/b/breslcr01.shtml

i <- 1
for (i in 1:nrow(df)) {
playerUrl <- paste0("http://www.baseball-reference.com",df$link[i])


#playerUrl <- "http://www.baseball-reference.com/players/b/breslcr01.shtml"
#playerUrl <- "http://www.baseball-reference.com/players/o/ortizda01.shtml"

tables = readHTMLTable(playerUrl , stringsAsFactors=FALSE)
#names(tables) # 


## get WAR by year

pitching <- tables["pitching_value"]$pitching_value
if (!is.null(pitching)) {
pitching <- pitching %>% 
  select(Year,Age,WAR)
glimpse(pitching)
pitching$Year <- str_sub(pitching$Year,1,4)
pitching$Year <- as.integer(pitching$Year)
pitching$Age <- as.integer(pitching$Age)
pitching$WAR <- as.numeric(pitching$WAR)

## need to group by year,

# remove NAs

pitching <-pitching[!is.na(pitching$Year),]

pitching <-pitching %>% 
  group_by(Year,Age) %>% 
  mutate(WAR=sum(WAR)) %>% 
  unique(.)

all <- data.frame(Year=c(min(pitching$Year):max(pitching$Year)))
all <- data.frame(Year=c(min(pitching$Year):max(pitching$Year)),Age=c(min(pitching$Age):max(pitching$Age)))

pitching <- pitching %>% 
  right_join(all) %>%
  mutate(WAR = ifelse(is.na(WAR),0,WAR))

#pitching[is.na(pitching$WAR),]$WAR <- 0

# pitching <- pitching %>% 
#   arrange(Year) %>% 
#   mutate(cumWAR=cumsum(.$WAR))

pitching$cumWAR <-cumsum(pitching$WAR)

pitching <- data.frame(pitching) ## otherwise cumWAR has attributes and ggvis layer_lines does not work
} 
## do cf with hitting



batting <- tables["batting_value"]$batting_value
if (!is.null(batting)) {
batting <- batting %>% 
  select(Year,Age,WAR)
glimpse(batting)
library(stringr)
batting$Year <- str_sub(batting$Year,1,4) # removes all star data
batting$Year <- as.integer(batting$Year)
batting$Age <- as.integer(batting$Age)
batting$WAR <- as.numeric(batting$WAR)

## need to group by year,

# remove NAs

batting <-batting[!is.na(batting$Year),]

batting <-batting %>% 
  group_by(Year,Age) %>% 
  mutate(WAR=sum(WAR)) %>% 
  unique(.)

all <- data.frame(Year=c(min(batting$Year):max(batting$Year)))
all <- data.frame(Year=c(min(batting$Year):max(batting$Year)),Age=c(min(batting$Age):max(batting$Age)))

batting <- batting %>% 
  right_join(all) %>%
  mutate(WAR = ifelse(is.na(WAR),0,WAR))

# batting[is.na(batting$WAR),]$WAR <- 0
# batting[is.na(batting$WAR),]$WAR <- 0

# batting <- batting %>% 
#   arrange(Year) %>% 
#   mutate(cumWAR=cumsum(.$WAR))

batting$cumWAR <-cumsum(batting$WAR)

batting <- data.frame(batting)
# zero as done althogh cumulatively is +0.2 in baseballl ref

}

if (!is.null(batting)&!is.null(pitching)) {
value <- batting %>% 
  rename(batWAR=WAR,batCumWAR=cumWAR) %>% 
  inner_join(pitching) %>% 
  mutate(val=batWAR+WAR,cumVal=batCumWAR+cumWAR)  %>% 
  rename(pitchWAR=WAR,pitchCumWAR=cumWAR)
} else if (!is.null(batting)){
  value <- batting %>% 
    rename(batWAR=WAR,batCumWAR=cumWAR) %>%
  mutate(pitchWAR=0,pitchCumWAR=0,val=batWAR,cumVal=batCumWAR)
} else {
  value <- pitching %>% 
    rename(pitchWAR=WAR,pitchCumWAR=cumWAR) %>%
    mutate(batWAR=0,batCumWAR=0,val=pitchWAR,cumVal=pitchCumWAR)
}

value$Name <- df$Name[i]
value$Salary <- df$Salary[i]
value$Category <- df$Category[i]
value$Active <- df$active[i]

if (i==1) {
  teamValues <- value 
  }  else {
  teamValues= rbind(teamValues,value) 
  }

}
teamValues$teamID <- "BOS"

names(teamValues)

library(ggvis)

teamValues %>% 
  group_by(Name) %>% 
  ggvis(~Year,~cumVal, stroke=~Name) %>% 
  layer_lines()


teamValues %>% 
  group_by(Name) %>% 
  ggvis(~Age,~cumVal, stroke=~Name) %>% 
  layer_lines()
library(readr)
write_csv(teamValues,"BOS2015.csv")

teamValues <- read_csv("BOS2015.csv")


## look at library dygraphs - though does not look great for multiple players

library(dygraphs)

teamValues %>% 
  group_by(Name)


or ggplot
http://rpubs.com/jalapic/gamebygame

library(ggplot2)

 ggplot(teamValues, aes(Age, cumVal, group=Name, color=Name)) +
  geom_line() etc.
  


# ggplot(mydf, aes(Gameno, CumGF, group=id, color=grp)) +
#   geom_line(aes(group=id, color=factor(grp))) +
#   geom_line(data=x2014, aes(Gameno, CumGF, group=id, color=factor(grp)), lwd=1.5) +
#   xlab("Game number") + ylab("Cumulative goals") +
#   scale_color_manual(values=c("gray80", "red")) +
#   ggtitle("Liverpool - Premier League goals by game") +
#   theme(
#     plot.title = element_text(hjust=0,vjust=1, size=rel(1.7)),
#     panel.background = element_blank(),
#     panel.grid.major.y = element_line(color="gray65"),
#     panel.grid.major.x = element_line(color="gray65"),
#     panel.grid.minor = element_blank(),
#     plot.background  = element_blank(),
#     text = element_text(color="gray20", size=10),
#     axis.text = element_text(size=rel(1.0)),
#     axis.text.x = element_text(color="gray20",size=rel(1.5)),
#     axis.text.y = element_text(color="gray20", size=rel(1.5)),
#     axis.title.x = element_text(size=rel(1.5), vjust=0),
#     axis.title.y = element_text(size=rel(1.5), vjust=1),
#     axis.ticks.y = element_blank(),
#     axis.ticks.x = element_blank(),
#     legend.position = "none"
#   )

look at linking to tables
library(DT)
career <- teamValues %>% 
  group_by(Name) %>% 
  arrange(desc(Age)) %>% 
  slice(1) %>% 
  select(Name,Age,WAR=cumVal) %>% 
  datatable(rownames = checkboxRows(.), escape = -1,
            ,options= list(paging = FALSE, searching = FALSE,info=FALSE, order = list(list(3, 'desc')))) 
  
  
  # link to graph
# app = system.file('examples', 'DT-info', package = 'DT')
# shiny::runApp(app)
# and  https://yihui.shinyapps.io/DT-checkbox/ which has code

teamValues %>% 
  group_by(Name) %>% 
  ggvis(~Age,~cumVal, stroke=~Name) %>% 
  layer_lines()

