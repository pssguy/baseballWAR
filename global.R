library(shiny)
library(XML)
library(dplyr)
library(rvest)
library(ggvis)
library(readr)
library(stringr)
library(DT)


# read in example set
#teamValues <- read_csv("BOS2015.csv")
allTeamValues <- read_csv("war2015latest.csv")

teamChoice <- unique(allTeamValues$teamID)