library(shiny)
library(XML)
library(dplyr)
library(rvest)
library(ggvis)
library(readr)
library(stringr)


# read in example set
teamValues <- read_csv("BOS2015.csv")