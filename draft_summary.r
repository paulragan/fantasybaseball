library(XLConnect)
library(ggplot2)
library(dplyr)
setwd("C:\\Users\\Pragan\\Dropbox\\bb")
draft_details <- readWorksheetFromFile("2018_draft.xlsx", sheet = 1, startRow = 1, endCol = 4)

ggplot(draft_details, aes(x = Pick_Num, y = Price)) + geom_point() + facet_wrap(~ Manager)

ggplot(draft_details, aes(y = Price, x = Pick_Num)) + geom_point(aes(color = factor(Manager)))

dplyr::summarise(draft_details, avg=mean(Price)) %>%
  group_by(Manager)

draft_details %>%
  group_by(Manager) %>%
  summarise(avg=median(Pick_Num))