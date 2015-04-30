## updating data

## start with this year - can update daily and add to historical data
## problem is WAR not avail for players new this year eg http://www.baseball-reference.com/players/b/bradlar01.shtml

#teamYears <-read_html("http://www.baseball-reference.com/leagues/MLB/2015.shtml")
a <- print(Sys.time())
library(rvest)
library(XML)
library(dplyr)
library(readr)
library(stringr)
library(DT)

teamYears <-read_html("http://www.baseball-reference.com/leagues/MLB/2015.shtml")
teamId <-html_nodes(teamYears, "#teams_standard_batting tbody td:nth-child(1)") %>%
  html_text()
tms <- length(teamId)-1  # -1 removes the leagueav
# i <- 1
for (i in 1:tms) { 
 # for (i in 1:1) {
  print(i)

  u <-paste0("http://www.baseball-reference.com/teams/",teamId[i],"/2015-roster.shtml")
#http://www.baseball-reference.com/teams/WSN/2015.shtml
  #http://www.baseball-reference.com/teams/BOS/2015-roster.shtml
  tables = readHTMLTable(u, stringsAsFactors=FALSE)
  
  active <- tables[[1]]
  
  # from this table really only want to know if active, name andposition
  #glimpse(active)
  active <- active[,c(1,3,5)]
  colnames(active) <- c("Rank","Name","Category")
  active$Rank <- as.integer(active$Rank)
  
  active$active <- "T"
  # this may be diff for past data
  active[active$Rank>25,]$active <- "F"
  # current for salary and age (though salary is iffy)
  current  <- tables[[2]]
  current <- current[,c(1,2,28)]
  
  # use rvest for links
  page <- read_html(u)
  #str(page)
  #class(u)
  links <-html_nodes(page, "#appearances a") %>%
    html_attr("href") 
  
  playerIDs <- character()
  # prob should do as an apply
  for (k in 1:length(links)) {
  playerIDs[k] <- str_match(links[k],"/players/[a-z]/(.+).shtml")[1,2]
  }
  
  df<-current %>% 
    left_join(active) # currently joins by name, playeriD might be better
  df$link <- links
  df$playerID <- playerIDs
  
  # j <- 1
  for (j in 1:nrow(df)) {
    print(j)
  
    playerUrl <- paste0("http://www.baseball-reference.com",df$link[j])
    
    
    #playerUrl <- "http://www.baseball-reference.com/players/b/breslcr01.shtml"
    #playerUrl <- "http://www.baseball-reference.com/players/o/ortizda01.shtml"
    
    tables = readHTMLTable(playerUrl , stringsAsFactors=FALSE)
    #names(tables) # 
    ## cate for new players - use standard fielding - though may only have pitching or batting
    
    if (!is.null(tables["standard_fielding"]$standard_fielding)) {
    temp <-tail(tables["standard_fielding"]$standard_fielding,1)
    } else if (!is.null(tables["batting_standard"]$batting_standard)) {
               temp <-tail(tables["batting_standard"]$batting_standard,1)
  } else {
    temp <-tail(tables["pitching_standard"]$pitching_standard,1)
  }           
    year <- temp$Year
    age <- temp$Age
    
    
    ## get WAR by year
    
    pitching <- tables["pitching_value"]$pitching_value
    if (!is.null(pitching)) {
      pitching <- pitching %>% 
        select(Year,Age,WAR)
     # glimpse(pitching)
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
      
      #all <- data.frame(Year=c(min(pitching$Year):max(pitching$Year)))
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
     # glimpse(batting)
      #library(stringr)
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
      
      batting <- data.frame(batting) # sometimes is not same length as pitching eg prior to interlock
      # and cannot do inner_join
      # zero as done althogh cumulatively is +0.2 in baseballl ref
      
    }
    
    # cater for situation where pitching and batting have not each been done in final year
    print("preadjust")
    if (!is.null(batting)&!is.null(pitching)) {
    if (max(batting$Year)>max(pitching$Year)) {
      newRow <- data.frame(Year=max(batting$Year),Age=max(batting$Age),WAR=0,cumWAR=tail(pitching,1)$cumWAR)
      pitching <- rbind(pitching,newRow)
    } else if (max(batting$Year)<max(pitching$Year)) {
      newRow <- data.frame(Year=max(pitching$Year),Age=max(pitching$Age),WAR=0,cumWAR=tail(batting,1)$cumWAR)
      batting <- rbind(batting,newRow)     
          }
    }
  #  print(glimpse(batting))
  #  print(glimpse(pitching))
    print("entering combo")
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

    } else if (!is.null(pitching)) {
      value <- pitching %>% 
        rename(pitchWAR=WAR,pitchCumWAR=cumWAR) %>%
        mutate(batWAR=0,batCumWAR=0,val=pitchWAR,cumVal=pitchCumWAR)
    } else {
      value <- data.frame(Year=year,Age=age,batWAR=0,batCumWAR=0,pitchWAR=0,pitchCumWAR=0,val=0,cumVal=0)
     } # need to cater for never played before
   # }
    print("exiting combo")
    value$Name <- df$Name[j]
    value$Salary <- df$Salary[j] # thi will just be final salary at moment
    value$Category <- df$Category[j]
    value$Active <- df$active[j]
    value$PlayerID <- df$playerID[j]
    
    print(value)
    
    if (j==1) {
      teamValues <- value 
    }  else {
      teamValues= rbind(teamValues,value) 
    }
    
  }
 # j <- NULL # NB hack
  teamValues$teamID <- teamId[i]
  
  if (i==1) { ##NB may need to change if not 1
    allTeamValues <- teamValues 
  }  else {
    allTeamValues= rbind(allTeamValues,teamValues) 
  }
} 
  
write_csv(allTeamValues,"war2015latest.csv")
b <- print(Sys.time())


### check c

# allTeamValues <- read_csv("war2015.csv") 
# allTeamValuesPt2 <- read_csv("war2015Pt2.csv") 
# allTeamValuesVer3 <- read_csv("war2015Ver3.csv") 
# unique(allTeamValuesVer3$teamID) # up to "MIN"
# ## need to add playerid eg miguel castro bjs has same as buehrle - maybe need to
# ## set value to zero
# 
# #glimpse(allTeamValues)
# #unique(allTeamValues$teamID)
# 
# library(DT)
# # allTeamValuesPt2 %>% 
# allTeamValues %>% 
#   filter(teamID=="NYM") %>% 
#   group_by(Name) %>% 
#   arrange(desc(Age)) %>% 
#   slice(1) %>% 
#   select(Name,Age,WAR=cumVal) %>% 
#   datatable(rownames = checkboxRows(.), escape = -1,
#             ,options= list(paging = FALSE, searching = FALSE,info=FALSE, order = list(list(3, 'desc')))) 
# 
# 
# ## issues (pitchers WAR eg buehrle had 0 for 2014 so joining them does not make sense)
# ## still getting some instances where star is not removed
# ## if new 