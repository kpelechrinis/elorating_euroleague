################################################################## 
### Calculating the Elo Rating                                 ###
# Euroleague 2016-17                                             #
# Author: Konstantinos Pelechrinis                               #
# Date: 04/23/2017                                               #
# Input files: euroleague1617.csv (basketball-reference.com)     #
################################################################## 

games <- read.csv("euroleague1617.csv")
teams <- unique(games$Visitor.Neutral)
k = 20
home.edge = 0.03
# This vector will keep track of the current Elo rating for the teams 
elo.rtg <- c()
for (i in 1:length(teams)){
  elo.rtg[as.character(teams[i])] = 1500
}

# The following data frame will keep track of all intermediate Elo ratings for the teams 
elo.all <- data.frame(Team=c(),Rating=c())
j = 1

for (i in 1:dim(games)[1]){
  # Estimate the expected score for each team
  vl = elo.rtg[as.character(games[i,]$Visitor.Neutral)][[1]]
  hl = elo.rtg[as.character(games[i,]$Home.Neutral)][[1]]
  elo.all[j,1]= as.character(games[i,]$Visitor.Neutral)
  elo.all[j,2] = vl
  j = j+1
  elo.all[j,1] = as.character(games[i,]$Home.Neutral)
  elo.all[j,2] = hl
  #Visiting
  Ev = max(1/(1+10^((hl-vl)/400)) - home.edge,0)
  #Home
  Eh = min(1/(1+10^((vl-hl)/400)) + home.edge,1)
  # Find the actual Elo score based on the outcome
  if (games[i,]$PTS < games[i,]$PTS.1){
    Sv = 0
    Sh = 1
  }else{
    Sv = 1
    Sh = 0
  }
  #Update ratings
  elo.rtg[as.character(games[i,]$Visitor.Neutral)][[1]] = elo.rtg[as.character(games[i,]$Visitor.Neutral)][[1]] + (k*(Sv-Ev))
  elo.rtg[as.character(games[i,]$Home.Neutral)][[1]] = elo.rtg[as.character(games[i,]$Home.Neutral)][[1]] + (k*(Sh-Eh))
}