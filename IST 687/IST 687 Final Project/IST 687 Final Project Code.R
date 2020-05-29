#IST687 - Applied Data Science
#Project R Code
#Team A2: Tanbir Biryajh, Ning Shi, Rashad Davis, Graham Latsa

#Environment config 

# if you did not install the following packages, please do so 

# if (!requireNamespace("BiocManager")) 

#   install.packages("BiocManager") 

# BiocManager::install() 

# BiocManager::available() 

# use the following format to install lib for R 3.5.1 

# the default install.package() function may not work for the latest R version 

# BiocManager::install('genefilter') 



# install.packages('plyr') 

# install.packages('jsonlite') 

# install.packages('genefilter') 

# install.packages('modeest') 

# install.packages('ggplot2') 

# install.packages('plotrix') 

# install.packages('car') 



# import library dependency 

# for any business question, please import the lib here 

library("plyr") 

library("jsonlite") 

library("modeest") 

library("ggplot2") 

library(plotrix) 

library(car) 


#Import Data 

# set work directory 

getwd() 

setwd('../data/league-of-legends/') 



# set the data source 

GAMES <- 'games.csv' 

# CHAM_INFO_1 <- 'champion_info.json' 

CHAM_INFO_2 <- 'champion_info_2.json' 

SUM_SPELL <- 'summoner_spell_info.json' 



# load csv and json 

games_df = read.csv(GAMES)  

# cham_info_1_list = fromJSON(CHAM_INFO_1) # type, version, data 

champ_info_2_list = fromJSON(CHAM_INFO_2) # type, version, data 

# cham_data_1_list = cham_info_1_list$data # title, id, key, name 

champ_data_2_list = champ_info_2_list$data # tag, title, id, key, name 

sum_spell_info_list = fromJSON(SUM_SPELL)$data # 'id', 'summonerLevel', 'name', key': 'SummonerHeal', 'description' 



# convert champ list to data frame 

champ_prep <- function(champ_list){ 
  
  
  
  champ_df <-as.data.frame(do.call("cbind", champ_list)) 
  
  champ_df <- data.frame(r1=names(champ_df), t(champ_df)) 
  
  champ_df$r1 <- NULL 
  
  row.names(champ_df) <- NULL 
  
  colnames(champ_df) <- c('ChampTag', 'ChampTitle', 'ChampID', 'ChampionKey', 'ChampionName') 
  
  
  
  return(champ_df) 
  
} 



champ_df <- champ_prep(champ_data_2_list[-1]) 



# convert sum spell info list to data frame 



sum_spell_prep <- function(sum_spell_list){ 
  
  
  
  sum_spell_df <- data.frame(matrix(unlist(sum_spell_info_list), nrow=length(sum_spell_info_list), byrow=T), stringsAsFactors = FALSE) 
  
  colnames(sum_spell_df) <- c("SummonerID","SummonerLevel","SummonerName","SummonerKey","SummonerDescription") 
  
  
  
  return(sum_spell_df) 
  
} 



sum_spell_df <- sum_spell_prep(sum_spell_info_list) 

#Dataset description 

#Business Questions 

#2.    Who is the champion with the highest win rate? Lowest win rate? 

#Two important functions:  

#Create a function that takes in a champion ID and returns the number of games they were present in.  

gamesCount = function(x){  
  
  totalGamesVector= c(games_df$t1_champ1id[games_df$t1_champ1id==x],   
                      
                      games_df$t1_champ2id[games_df$t1_champ2id==x],  
                      
                      games_df$t1_champ3id[games_df$t1_champ3id==x],   
                      
                      games_df$t1_champ4id[games_df$t1_champ4id==x],  
                      
                      games_df$t1_champ5id[games_df$t1_champ5id==x],  
                      
                      games_df$t2_champ1id[games_df$t2_champ1id==x],   
                      
                      games_df$t2_champ2id[games_df$t2_champ2id==x],  
                      
                      games_df$t2_champ3id[games_df$t2_champ3id==x],   
                      
                      games_df$t2_champ4id[games_df$t2_champ4id==x],  
                      
                      games_df$t2_champ5id[games_df$t2_champ5id==x])  
  
  totalGameCount = length(totalGamesVector)  
  
  return(totalGameCount)  
  
}  

#Create a function that takes in a champion ID and returns the win rate for that champion.  

#We will use the gamesCount function to help with this calculation.  

winRate = function(x) {  
  
  totalGames = gamesCount(x)  
  
  totalWinsVector = c(games_df$t1_champ1id[games_df$winner==1 & games_df$t1_champ1id == x],   
                      
                      games_df$t1_champ2id[games_df$winner==1 & games_df$t1_champ2id == x],  
                      
                      games_df$t1_champ3id[games_df$winner==1 & games_df$t1_champ3id == x],   
                      
                      games_df$t1_champ4id[games_df$winner==1 & games_df$t1_champ4id == x],  
                      
                      games_df$t1_champ5id[games_df$winner==1 & games_df$t1_champ5id == x],  
                      
                      games_df$t2_champ1id[games_df$winner==2 & games_df$t2_champ1id == x],   
                      
                      games_df$t2_champ2id[games_df$winner==2 & games_df$t2_champ2id == x],  
                      
                      games_df$t2_champ3id[games_df$winner==2 & games_df$t2_champ3id == x],   
                      
                      games_df$t2_champ4id[games_df$winner==2 & games_df$t2_champ4id == x],  
                      
                      games_df$t2_champ5id[games_df$winner==2 & games_df$t2_champ5id == x])  
  
  totalWins = length(totalWinsVector)  
  
  totalWinRate = (totalWins / totalGames)*100  
  
  return(totalWinRate)  
  
}  



# Win rate per champion 

max(unlist(champ_df$ChampID)) 

min(unlist(champ_df$ChampID)) 

#Add winrate to champ dataframe 

for (x in 1:516){ 
  
  champ_df$winrate[champ_df$ChampID==x] <- winRate(x) 
  
} 



#Highest Win Rate 

champ_df$ChampionName[which.max(champ_df$winrate)]  

#Lowest Win rate 

champ_df$ChampionName[which.min(champ_df$winrate)] 



#Win rate plot 

library("ggplot2") 

#Add Champion Names as rownames 

row.names(champ_df) <- champ_df$ChampionName 

row.names(champ_df) 



#Create a dataframe with the top 5 champions 

top5 <- head(champ_df[order(-(champ_df$winrate)), ], 5)   



ggplot(top5, aes(x=row.names(top5), y=winrate))  + geom_col(color="black", fill="springgreen") + ggtitle("Bar Chart: Highest Win Rate Champions") + theme(plot.title = element_text(hjust = 0.5)) + ylab("Win Rate (%)") + xlab("Champion Name") + theme(axis.text.y = element_text(size = 3) ) + geom_text(aes(label=top5$winrate), vjust=-0.3, size=3.5)  



#Create a dataframe with the bottom 5 champions 

bot5 <- head(champ_df[order((champ_df$winrate)), ], 5)   



ggplot(bot5, aes(x=row.names(bot5), y=winrate))  + geom_col(color="black", fill="springgreen") + ggtitle("Bar Chart: Lowest Win Rate Champions") + theme(plot.title = element_text(hjust = 0.5)) + ylab("Win Rate (%)") + xlab("Champion Name") + theme(axis.text.y = element_text(size = 3) ) + geom_text(aes(label=bot5$winrate), vjust=-0.3, size=3.5) 



#Overall Plot 

ggplot(champ_df, aes(x=row.names(champ_df), y=winrate))  + geom_col(color="black", fill="pink") + coord_flip() + ggtitle("Bar Chart: Win Rate per Champion") + theme(plot.title = element_text(hjust = 0.5)) + ylab("Win Rate (%)") + xlab("Champion Name") + theme(axis.text.y = element_text(size = 3) ) 



#Split the plot between above and below average win rates        

aboveaveragechamps <- champ_df[champ_df$winrate > mean(champ_df$winrate),] 

belowaveragechamps <- champ_df[champ_df$winrate < mean(champ_df$winrate),] 

ggplot(aboveaveragechamps, aes(x=row.names(aboveaveragechamps), y=winrate))  + geom_col(color="black", fill="pink") + coord_flip() + ggtitle("Bar Chart: Win Rate per Above Average Champion") + theme(plot.title = element_text(hjust = 0.5)) + ylab("Win Rate (%)") + xlab("Champion Name") + theme(axis.text.y = element_text(hjust=1, size = 6.5) ) 

ggplot(belowaveragechamps, aes(x=row.names(belowaveragechamps), y=winrate))  + geom_col(color="black", fill="pink") + coord_flip() + ggtitle("Bar Chart: Win Rate per Below Average Champion") + theme(plot.title = element_text(hjust = 0.5)) + ylab("Win Rate (%)") + xlab("Champion Name") + theme(axis.text.y = element_text(hjust=1, size = 6.5) ) 



#top 25 and bottom 25 champs 

quantile(champ_df$winrate, c(0.25, 0.75)) 

top25champs <- champ_df[champ_df$winrate > quantile(champ_df$winrate, 0.75),] 

bottom25champs <- champ_df[champ_df$winrate < quantile(champ_df$winrate, 0.25),] 

ggplot(top25champs, aes(x=row.names(top25champs), y=winrate))  + geom_col(color="black", fill="pink") + coord_flip() + ggtitle("Bar Chart: Win Rate for Top 25% Champion") + theme(plot.title = element_text(hjust = 0.5)) + ylab("Win Rate (%)") + xlab("Champion Name") + theme(axis.text.y = element_text(hjust=1, size =8)) 

ggplot(bottom25champs, aes(x=row.names(bottom25champs), y=winrate))  + geom_col(color="black", fill="pink") + coord_flip() + ggtitle("Bar Chart: Win Rate Bottom 25% Champion") + theme(plot.title = element_text(hjust = 0.5)) + ylab("Win Rate (%)") + xlab("Champion Name") + theme(axis.text.y = element_text(hjust=1, size = 8) ) 


#3.    What does the highest win rate champion lose to the most? 

#**Take the instances where the champion with the highest  

#win rate lost to a set of champions and tally those losses  

#or put those champions into a vector** 



#Champ ID with highest win rate 

highwinratechampid <- as.numeric(unlist(champ_df$ChampID[which.max(champ_df$winrate)])) 



#Generate a list Team 2 champions who won a game against a team with at least one high win rate champion  

T1bestchampcounter <-  
  
  as.vector(rbind(games_df$t2_champ1id[games_df$winner==2 & (games_df$t1_champ1id==highwinratechampid | games_df$t1_champ2id==highwinratechampid| games_df$t1_champ3id==highwinratechampid| games_df$t1_champ4id==highwinratechampid| games_df$t1_champ5id==highwinratechampid)], 
                  
                  games_df$t2_champ2id[games_df$winner==2 & (games_df$t1_champ1id==highwinratechampid | games_df$t1_champ2id==highwinratechampid| games_df$t1_champ3id==highwinratechampid| games_df$t1_champ4id==highwinratechampid| games_df$t1_champ5id==highwinratechampid)], 
                  
                  games_df$t2_champ3id[games_df$winner==2 & (games_df$t1_champ1id==highwinratechampid | games_df$t1_champ2id==highwinratechampid| games_df$t1_champ3id==highwinratechampid| games_df$t1_champ4id==highwinratechampid| games_df$t1_champ5id==highwinratechampid)], 
                  
                  games_df$t2_champ4id[games_df$winner==2 & (games_df$t1_champ1id==highwinratechampid | games_df$t1_champ2id==highwinratechampid| games_df$t1_champ3id==highwinratechampid| games_df$t1_champ4id==highwinratechampid| games_df$t1_champ5id==highwinratechampid)], 
                  
                  games_df$t2_champ5id[games_df$winner==2 & (games_df$t1_champ1id==highwinratechampid | games_df$t1_champ2id==highwinratechampid| games_df$t1_champ3id==highwinratechampid| games_df$t1_champ4id==highwinratechampid| games_df$t1_champ5id==highwinratechampid)])) 



#Team 2's Best Counter to the Highest Win Rate Champion         

champ_df$ChampionName[champ_df$ChampID==mfv(T1bestchampcounter)] 



#Generate a list Team 1 champions who won a game against a team with at least one high win rate champion 

T2bestchampcounter <-  
  
  as.vector(rbind(games_df$t1_champ1id[games_df$winner==1 & (games_df$t2_champ1id==highwinratechampid | games_df$t2_champ2id==highwinratechampid| games_df$t2_champ3id==highwinratechampid| games_df$t2_champ4id==highwinratechampid| games_df$t2_champ5id==highwinratechampid)], 
                  
                  games_df$t1_champ2id[games_df$winner==1 & (games_df$t2_champ1id==highwinratechampid | games_df$t2_champ2id==highwinratechampid| games_df$t2_champ3id==highwinratechampid| games_df$t2_champ4id==highwinratechampid| games_df$t2_champ5id==highwinratechampid)], 
                  
                  games_df$t1_champ3id[games_df$winner==1 & (games_df$t2_champ1id==highwinratechampid | games_df$t2_champ2id==highwinratechampid| games_df$t2_champ3id==highwinratechampid| games_df$t2_champ4id==highwinratechampid| games_df$t2_champ5id==highwinratechampid)], 
                  
                  games_df$t1_champ4id[games_df$winner==1 & (games_df$t2_champ1id==highwinratechampid | games_df$t2_champ2id==highwinratechampid| games_df$t2_champ3id==highwinratechampid| games_df$t2_champ4id==highwinratechampid| games_df$t2_champ5id==highwinratechampid)], 
                  
                  games_df$t1_champ5id[games_df$winner==1 & (games_df$t2_champ1id==highwinratechampid | games_df$t2_champ2id==highwinratechampid| games_df$t2_champ3id==highwinratechampid| games_df$t2_champ4id==highwinratechampid| games_df$t2_champ5id==highwinratechampid)])) 



#Team 1's Best Counter to Team 1's Highest Win Rate Champion         

champ_df$ChampionName[champ_df$ChampID==mfv(T2bestchampcounter)] 



#The best counter overall 

champ_df$ChampionName[champ_df$ChampID==mfv(c(T1bestchampcounter, T2bestchampcounter))] 



#Generate the histogram to view the counter ranking against Janna 

counter_rank1_df <- count(T1bestchampcounter) 

counter_rank1_df$team <- rep(1, dim(counter_rank1_df)[1]) 

counter_rank2_df <- count(T2bestchampcounter) 

counter_rank2_df$team <- rep(2, dim(counter_rank2_df)[1]) 

counter_rank_df <- rbind(counter_rank1_df, counter_rank2_df) 

colnames(counter_rank_df) <- c('id', 'freq', 'team') 

counter_rank_df$name <- unlist(champ_df$ChampionName[match(counter_rank_df$id, champ_df$ChampID)]) 

row.names(counter_rank_df) <- NULL 



top_5_ids <- count(c(T1bestchampcounter, T2bestchampcounter)) 

top_5_ids <- top_5_ids[order(-top_5_ids$freq),][1:5,] 



top5_1_df <- counter_rank_df[which(counter_rank_df$team==1),][match(top_5_ids$x, counter_rank_df$id),] 

top5_2_df <- counter_rank_df[which(counter_rank_df$team==2),][match(top_5_ids$x, counter_rank_df$id),] 



top_5_df <- rbind(top5_1_df, top5_2_df) 

top_5_df$team[top_5_df$team==1] <- 'team_1' 

top_5_df$team[top_5_df$team==2] <- 'team_2' 



ggplot(data=top_5_df, aes(x=reorder(name, -freq), y=freq, fill=team)) +   
  
  geom_bar(stat="identity", width=0.8, position=position_dodge()) +  
  
  geom_text(aes(label=freq), hjust=2, color="white", 
            
            position = position_dodge(0.5), size=4) +  
  
  ggtitle("Top Five Best Counter") +  
  
  xlab("Champ_Name") + ylab("Count_Time") + 
  
  coord_flip() 


#4.    What are the top two summoner spells for the champion with the most wins? 

#Take the champion with the highest win rate and record all the summoner spells used for that champion in a vector. 


bestChampSpell1 = c( 
  
  games_df$t1_champ1_sum1[games_df$t1_champ1id == bestChamp], 
  
  games_df$t1_champ2_sum1[games_df$t1_champ2id == bestChamp], 
  
  games_df$t1_champ3_sum1[games_df$t1_champ3id == bestChamp], 
  
  games_df$t1_champ4_sum1[games_df$t1_champ4id == bestChamp], 
  
  games_df$t1_champ5_sum1[games_df$t1_champ5id == bestChamp], 
  
  games_df$t2_champ1_sum1[games_df$t2_champ1id == bestChamp], 
  
  games_df$t2_champ2_sum1[games_df$t2_champ2id == bestChamp], 
  
  games_df$t2_champ3_sum1[games_df$t2_champ3id == bestChamp], 
  
  games_df$t2_champ4_sum1[games_df$t2_champ4id == bestChamp], 
  
  games_df$t2_champ5_sum1[games_df$t2_champ5id == bestChamp] 
  
) 


#Do the same for spell number 2. 


bestChampSpell2 = c( 
  
  games_df$t1_champ1_sum2[games_df$t1_champ1id == bestChamp], 
  
  games_df$t1_champ2_sum2[games_df$t1_champ2id == bestChamp], 
  
  games_df$t1_champ3_sum2[games_df$t1_champ3id == bestChamp], 
  
  games_df$t1_champ4_sum2[games_df$t1_champ4id == bestChamp], 
  
  games_df$t1_champ5_sum2[games_df$t1_champ5id == bestChamp], 
  
  games_df$t2_champ1_sum2[games_df$t2_champ1id == bestChamp], 
  
  games_df$t2_champ2_sum2[games_df$t2_champ2id == bestChamp], 
  
  games_df$t2_champ3_sum2[games_df$t2_champ3id == bestChamp], 
  
  games_df$t2_champ4_sum2[games_df$t2_champ4id == bestChamp], 
  
  games_df$t2_champ5_sum2[games_df$t2_champ5id == bestChamp] 
  
) 


#Mode of summoner spell 1 vector and summoner spell 2 vector will return the most frequently taken summoner spells. 

mfv(bestChampSpell1) 

#ID is 4 for Flash. 

mfv(bestChampSpell2) 

#ID is 3 for Exhaust. 

#This is okay, but summoner spell number is really just the position of the spell on one of two keys.  

#sum1 could be ID 4 and sum2 could be ID 3, or vice versa, and they would be the exact same spells. 

#We need to combine the summoner spells into one big vector and find the requency. 

bestChampSpells = c(bestChampSpell1, bestChampSpell2) 

#Now we can find the frequency. In fact, let's create a function that we can use later to find the frequency of a vector of summoner spells. 

#This function can only be used on the entire listing of summoner spells within both columns. This will not produce accurate results for a single column of summoner spells. 

#This is fine, however, as a single columns is useless to our calculation. 



sumSpellFreq = function(v){ 
  
  SpellID = unique(v) 
  
  counts = tabulate(match(v, SpellID)) 
  
  percentUsed = (counts/length(v))*2*100 #multiply this value by 2 here since there are 2 summoner spells per game. 
  
  df1 = data.frame(SpellID, counts, percentUsed) 
  
  df = merge(df1, summonerlist, by = "SpellID") 
  
  return(data.frame(SpellName = df$SpellName, SpellCount = df$counts, PercentTaken = paste(round(df$percentUsed, 2),"%"))) 
  
} 


JannaSpells = sumSpellFreq(bestChampSpells) 

JannaSpells 

#Now we can see that Flash is taken 99.85% of the time and exhaust is taken 93% of the time. 


#Using the vector creation from above, we can create a function that will receive a champ ID and return a vector that contains all the summoner spells used by a champion. 

#We can use this function to help with other questions. 

sumSpellList = function(x){ 
  
  c( 
    
    games_df$t1_champ1_sum1[games_df$t1_champ1id == x], 
    
    games_df$t1_champ2_sum1[games_df$t1_champ2id == x], 
    
    games_df$t1_champ3_sum1[games_df$t1_champ3id == x], 
    
    games_df$t1_champ4_sum1[games_df$t1_champ4id == x], 
    
    games_df$t1_champ5_sum1[games_df$t1_champ5id == x], 
    
    games_df$t2_champ1_sum1[games_df$t2_champ1id == x], 
    
    games_df$t2_champ2_sum1[games_df$t2_champ2id == x], 
    
    games_df$t2_champ3_sum1[games_df$t2_champ3id == x], 
    
    games_df$t2_champ4_sum1[games_df$t2_champ4id == x], 
    
    games_df$t2_champ5_sum1[games_df$t2_champ5id == x], 
    
    games_df$t1_champ1_sum2[games_df$t1_champ1id == x], 
    
    games_df$t1_champ2_sum2[games_df$t1_champ2id == x], 
    
    games_df$t1_champ3_sum2[games_df$t1_champ3id == x], 
    
    games_df$t1_champ4_sum2[games_df$t1_champ4id == x], 
    
    games_df$t1_champ5_sum2[games_df$t1_champ5id == x], 
    
    games_df$t2_champ1_sum2[games_df$t2_champ1id == x], 
    
    games_df$t2_champ2_sum2[games_df$t2_champ2id == x], 
    
    games_df$t2_champ3_sum2[games_df$t2_champ3id == x], 
    
    games_df$t2_champ4_sum2[games_df$t2_champ4id == x], 
    
    games_df$t2_champ5_sum2[games_df$t2_champ5id == x] 
    
  ) 
  
} 



#Graph of Janna's spell percentage. 



JannaSpellsPlot = grid.table(JannaSpells) 



JannaSpellsPlot  

#5.    What is the best composition of champions? 

# use ifelse function to collect the ids from the team that won the game 

win_champs <- data.frame(ifelse(games_df$winner == 1, games_df$t1_champ1id, games_df$t2_champ1id),  
                         
                         ifelse(games_df$winner == 1, games_df$t1_champ2id, games_df$t2_champ2id),  
                         
                         ifelse(games_df$winner == 1, games_df$t1_champ3id, games_df$t2_champ3id),  
                         
                         ifelse(games_df$winner == 1, games_df$t1_champ4id, games_df$t2_champ4id),  
                         
                         ifelse(games_df$winner == 1, games_df$t1_champ5id, games_df$t2_champ5id)) 

colnames(win_champs) <- c('champ1id', 'champ2id', 'champ3id', 'champ4id', 'champ5id') 



# takes around 2 mins to sum up the team ids as a list 

for (i in 1:dim(win_champs)[1]){ 
  
  champs_v <- c(win_champs$champ1id[i],  
                
                win_champs$champ2id[i],  
                
                win_champs$champ3id[i],  
                
                win_champs$champ4id[i],  
                
                win_champs$champ5id[i]) 
  
  champs_v <- sort(champs_v) 
  
  win_champs$team_ids[i] <- list(champs_v) 
  
  print(i/dim(win_champs)[1]) 
  
} 

# a beet way to count the occurrence is to conver the list to string 

for (i in 1:dim(win_champs)[1]){ 
  
  
  
  win_champs$str_team_ids[i] <- paste(win_champs$team_ids[i], sep=" ") 
  
  print(i/dim(win_champs)[1]) 
  
} 

# count the frequency for each team config in string data type 

freq <- count(win_champs, vars = 'str_team_ids') 



# match the frequency and string team ids 

win_champs$team_freq <- freq$freq[match(win_champs$str_team_ids, freq$str_team_ids)] 



# get teams which won the most times 

top_teams <- win_champs$team_ids[win_champs$team_freq == max(win_champs$team_freq)] 



# set up a function to map each id to its name 

id2name <- function(team_ids){ 
  
  name_lists <- champ_df$ChampionName[match(unlist(team_ids), champ_df$ChampID)] 
  
  return(c(names(unlist(name_lists)))) 
  
} 



# clean the result and save it to a data frame 

top_team_df <- data.frame() 

for (i in 1:length(top_teams)){ 
  
  ids = top_teams[i] 
  
  names = list(id2name(top_teams[i])) 
  
  top_team_df[i,1] <- list(ids) 
  
  top_team_df[i,2] <- list(names) 
  
} 

colnames(top_team_df) <- c('ids', 'names') 

View(top_team_df) 



# create a champs ranking graph among the top teams 

champ_rank_df <- count(unlist(top_team_df$names)) 

colnames(champ_rank_df) <- c('name', 'freq') 

top_10_cooperator <- champ_rank_df[order(-champ_rank_df$freq),][1:10,]  



ggplot(data=top_10_cooperator, aes(x=reorder(name, freq), y=freq, fill=name)) +   
  
  geom_bar(stat="identity", width=0.8, position=position_dodge()) +  
  
  geom_text(aes(label=freq), hjust=2, color="white", 
            
            position = position_dodge(0.5), size=4) +  
  
  ggtitle("Top 10 Cooperator") +  
  
  xlab("Champ_Name") + ylab("Win_Frequency") + 
  
  coord_flip() 

#Name list of each team 

c("Jax", "Twitch", "LeeSin", "Lux", "Syndra") 

c("Singed", "MonkeyKing", "Kayn", "Braum", "Jhin") 

c("Soraka", "Akali", "Lux", "Khazix", "Jhin") 

c("MasterYi", "JarvanIV", "Yorick", "Jinx", "Thresh") 

c("Urgot", "Tristana", "LeeSin", "Braum", "Zed") 

c("MissFortune", "Akali", "Leona", "Sejuani", "AurelionSol") 

c("Sivir", "Swain", "Leona", "Lux", "Khazix") 

c("Sivir", "Swain", "Leona", "Lux", "Khazix") 

c("Gragas", "Fiora", "Kayn", "Jhin", "Thresh") 

c("LeeSin", "Riven", "KogMaw", "Zed", "Nami") 

c("Urgot", "Tristana", "Sona", "LeeSin", "Cassiopeia") 

c("MasterYi", "Jax", "Talon", "Rakan", "Xayah") 

c("Jax", "Twitch", "LeeSin", "Lux", "Syndra") 

c("Tryndamere", "Twitch", "Orianna", "Sejuani", "Thresh") 

c("Caitlyn", "Maokai", "Akali", "Zed", "Ornn") 

c("Tristana", "Blitzcrank", "Renekton", "KogMaw", "Kindred") 

c("Gragas", "Fiora", "Kayn", "Jhin", "Thresh") 

c("Tristana", "Blitzcrank", "Renekton", "KogMaw", "Kindred") 

c("Urgot", "Tristana", "LeeSin", "Braum", "Zed") 

c("Soraka", "Akali", "Lux", "Khazix", "Jhin") 

c("Teemo", "Shaco", "Janna", "Brand", "Varus") 

c("Orianna", "MonkeyKing", "Brand", "Jhin", "RekSai") 

c("Teemo", "Shaco", "Janna", "Brand", "Varus") 

c("Rammus", "Riven", "Ahri", "Rakan", "Xayah") 

c("LeeSin", "Riven", "KogMaw", "Zed", "Nami") 

c("Soraka", "Akali", "Lux", "Khazix", "Jhin") 

c("Orianna", "MonkeyKing", "Brand", "Jhin", "RekSai") 

c("Anivia", "Caitlyn", "Leona", "Fiora", "Ornn") 

c("Urgot", "Tristana", "Sona", "LeeSin", "Cassiopeia") 

c("Morgana", "Malphite", "Hecarim", "Yasuo", "Jinx") 

c("Morgana", "Malphite", "Hecarim", "Yasuo", "Jinx") 

c("MasterYi", "Jax", "Talon", "Rakan", "Xayah") 

c("Caitlyn", "Maokai", "Akali", "Zed", "Ornn") 

c("LeeSin", "Riven", "KogMaw", "Zed", "Nami") 

c("DrMundo", "Orianna", "Kayn", "Rakan", "Xayah") 

c("Morgana", "Malphite", "Hecarim", "Yasuo", "Jinx") 

c("Gangplank", "Sejuani", "Velkoz", "Rakan", "Xayah") 

c("Gragas", "Fiora", "Kayn", "Jhin", "Thresh") 

c("Tryndamere", "Twitch", "Orianna", "Sejuani", "Thresh") 

c("Teemo", "Shaco", "Janna", "Brand", "Varus") 

c("DrMundo", "Orianna", "Kayn", "Rakan", "Xayah") 

c("DrMundo", "Orianna", "Kayn", "Rakan", "Xayah") 

c("Urgot", "Tristana", "Sona", "LeeSin", "Cassiopeia") 

c("MissFortune", "Taric", "Lissandra", "Kayn", "RekSai") 

c("MissFortune", "Akali", "Leona", "Sejuani", "AurelionSol") 

c("MissFortune", "Akali", "Leona", "Sejuani", "AurelionSol") 

c("Anivia", "Caitlyn", "Leona", "Fiora", "Ornn") 

c("Rammus", "Riven", "Ahri", "Rakan", "Xayah") 

c("Anivia", "Caitlyn", "Leona", "Fiora", "Ornn") 

c("MasterYi", "JarvanIV", "Yorick", "Jinx", "Thresh") 

c("MasterYi", "JarvanIV", "Yorick", "Jinx", "Thresh") 

c("MasterYi", "Jax", "Talon", "Rakan", "Xayah") 

c("Caitlyn", "Maokai", "Akali", "Zed", "Ornn") 

c("Rammus", "Riven", "Ahri", "Rakan", "Xayah") 

c("Gangplank", "Sejuani", "Velkoz", "Rakan", "Xayah") 

c("Singed", "MonkeyKing", "Kayn", "Braum", "Jhin") 

c("Urgot", "Tristana", "LeeSin", "Braum", "Zed") 

c("Tristana", "Blitzcrank", "Renekton", "KogMaw", "Kindred") 

c("MissFortune", "Taric", "Lissandra", "Kayn", "RekSai") 

c("Tryndamere", "Twitch", "Orianna", "Sejuani", "Thresh") 

c("Gangplank", "Sejuani", "Velkoz", "Rakan", "Xayah") 

c("MissFortune", "Taric", "Lissandra", "Kayn", "RekSai") 

c("Sivir", "Swain", "Leona", "Lux", "Khazix") 

c("Singed", "MonkeyKing", "Kayn", "Braum", "Jhin") 

c("Jax", "Twitch", "LeeSin", "Lux", "Syndra") 

c("Orianna", "MonkeyKing", "Brand", "Jhin", "RekSai") 


#6.    What is the most banned champion? What is their win rate? 

#Create a vector of all the banned champion. 

#Note that both sides can have the same bans, so a champion can be banned by both sides in a match. 

allBans <- c(games_df$t1_ban1, 
             
             games_df$t1_ban2, 
             
             games_df$t1_ban3, 
             
             games_df$t1_ban4, 
             
             games_df$t1_ban5, 
             
             games_df$t2_ban1, 
             
             games_df$t2_ban2, 
             
             games_df$t2_ban3, 
             
             games_df$t2_ban4, 
             
             games_df$t2_ban5) 



#Find the mode of the banned champions. This will be the most banned champion. 



mfv(allBans) 



#The most banned champion is ID 157, which is Yasuo. 



#Win rate of most banned champion.  



winRate(157) #50.15% win rate. 



#Calculate ban rate. Create a function that takes in a champion ID and returns a ban rate. 



banRate = function (ID){ 
  
  length(allBans[allBans == ID])/nrow(games_df)*100 
  
} 



#Observe the ban rate of the most banned champion. 



banRate(157) 



#Create a data frame with ban rates for all champs. 

#Use lapply with the banRate function to return the ban rates for the champions. 

#At the same time, we can unlist this result and create a data frame that matches the champion ID. 



champBanRateList = data.frame(champList, unlist(lapply(champList, banRate))) 



#Let's clean up the column names. We'll be using this data frame a bunch. 



colnames(champBanRateList) = c("ChampID", "BanRate") 



#Merge champion win rate list with champ list. 



champBanRateList = merge(champBanRateList, champlist, by = "ChampID") 



champBanRateList = data.frame(ChampID = champBanRateList$ChampID, ChampName = champBanRateList$ChampionName, BanRate = champBanRateList$BanRate) 



#Find the highest win rate. Use that index to find the champion ID. 



champBanRateList[which.max(champBanRateList[,3]),] 



#The champion with the highest ban rate is ID 157, Yasuo, with a 64.12% ban rate. 



champBanRateList[which.min(champBanRateList[,3]),] 



#The champion with the lowest ban rate is ID 72, Skarner, with a 0.14% ban rate. 



#DataFrame with champs, win rate, and ban rate. 



ChampInfo = merge(champBanRateList, champWinRateList, by = "ChampID") 

ChampInfo = data.frame(ChampID = ChampInfo$ChampID, ChampName = ChampInfo$ChampName.x, WinRate = round(ChampInfo$WinRate, 2), BanRate = round(ChampInfo$BanRate, 2)) 



#Plot the ban rate. 

BanBar = ggplot(ChampInfo, aes(x = ChampName, y = BanRate)) + geom_col(color = "black", fill = "blue") + coord_flip() +  ggtitle("Bar Chart: Ban Rate for Champions") + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.text.y = element_text(hjust=0, size =3)) 

BanBar 



#Ban Rate for Top 10 Champions with win rate. 

Champinfo2 = ChampInfo[order(-ChampInfo$BanRate),] 

Champinfo2 = Champinfo2[1:10,] 

Champinfo2 = melt(Champinfo2, id.vars = "ChampName", measure.vars = c("WinRate", "BanRate")) 

BanBar2 = ggplot(Champinfo2, aes(x = ChampName, y = value, fill = variable, label = paste0(value,"%"))) + geom_bar(stat = "identity") + coord_flip() + ggtitle("Bar Chart: Top 10 Banned Champs with Win Rate") + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.text.y = element_text(face = "bold", hjust=0, size =9))  + geom_text(size = 4, position = position_stack(vjust = 0.5), color = "white") + scale_fill_manual(values=c("dark green", "dark red")) 

BanBar2 

#7.    Which objective kill count is the best predictor of winner? 



#Scale the objectives and create a new dat frame with the information. 



compareObjectives <- data.frame(games_df$winner, 
                                
                                scale(games_df$t1_towerKills, center = FALSE, scale = TRUE),  
                                
                                scale(games_df$t1_inhibitorKills, center = FALSE, scale = TRUE), 
                                
                                scale(games_df$t1_baronKills, center = FALSE, scale = TRUE),  
                                
                                scale(games_df$t1_dragonKills, center = FALSE, scale = TRUE), 
                                
                                scale(games_df$t1_riftHeraldKills, center = FALSE, scale = TRUE), 
                                
                                scale(games_df$t2_towerKills, center = FALSE, scale = TRUE),  
                                
                                scale(games_df$t2_inhibitorKills, center = FALSE, scale = TRUE), 
                                
                                scale(games_df$t2_baronKills, center = FALSE, scale = TRUE), 
                                
                                scale(games_df$t2_dragonKills, center = FALSE, scale = TRUE), 
                                
                                scale(games_df$t2_riftHeraldKills, center = FALSE, scale = TRUE) 
                                
) 



#Rename the columns with easily accessible names. 



colnames(compareObjectives) <-  c("winner", "t1_towerKills",  
                                  
                                  "t1_inhibitorKills", "t1_baronKills",  
                                  
                                  "t1_dragonKills", "t1_riftHeraldKills", 
                                  
                                  "t2_towerKills",  
                                  
                                  "t2_inhibitorKills", "t2_baronKills",  
                                  
                                  "t2_dragonKills", "t2_riftHeraldKills") 



#Plot to get an example of the data. 



plot(compareObjectives$t1_towerKills, compareObjectives$winner) 





#Looks like linear regression will not work here. Use logit or probit. 



#Set winner outcome to binary to perform logit analysis. Blue becomes 0, Red becomes 1. 



compareObjectives$winner = ifelse (compareObjectives$winner == 2, 1, 0) 



#Call the logit and probits models into variables. 



ObjLog = glm(formula = winner ~ ., family = binomial(logit), data = compareObjectives) 



#Look at sumaries of the data. 



summary(ObjLog) 



#For both models, tower kills were the most important factors and dragon kills did not matter. However, we need to perform a VIF to see if any factors have multicollinearity. 



vif(ObjLog) 



#Drop dragons. 



ObjLog2 = glm(formula = winner ~ t1_towerKills + t1_inhibitorKills + t1_baronKills + t1_riftHeraldKills + t2_towerKills + t2_inhibitorKills +t2_baronKills + t2_riftHeraldKills  , family = binomial(logit), data = compareObjectives) 



summary(ObjLog2) 

vif(ObjLog2) 





#Use the logit model to predict the winner. The type is response to ensure values between 1 and 0. Round the output to get values between 1 and 0. 



compareObjectives$predWinner = round(predict(ObjLog2, type = "resp")) 



#Percentage Correct 



perc_log <- length(which(compareObjectives$winner==compareObjectives$predWinner))/dim(compareObjectives)[1] 

perc_log 





#Confusion matrix using the logit model. 



#Create a table for the actual and predicted value counts. 



bq6results <- table(Actual = compareObjectives$winner, Pred = compareObjectives$predWinner) 



bq6plot = qplot(1:2, 1:2) +   theme_void() + annotation_custom(grob = tableGrob(bq6results)) + 
  
  annotate("text", x = 1.515, y = 1.65, label = "Prediction", fontface = 2) + annotate("text", x = 1.38, y = 1.57, label = "Actual", fontface = 2) + 
  
  annotate("text", x = 1.515, y = 1.35, label = "Percentage Correct - 95.97%", colour = "dark green", fontface = 2) 



bq6plot 



#Graph the prediction based on the analysis. 



ObjLogPlot = ggplot(compareObjectives, aes(x = t2_towerKills, y = winner)) + stat_smooth(method="glm", method.args=list(family="binomial"), se=TRUE) 

ObjLogPlot 





#Add a histogram to the plot. 

logi.hist.plot(compareObjectives$t2_towerKills,compareObjectives$winner,boxp=FALSE,type="hist",col="gray") 

# 8 Which side has a higher win rate? 

# draw pie chart for team win rate 

team_win_count <- c(length(games_df$winner[games_df$winner==1]), length(games_df$winner[games_df$winner==2])) 

team_win_rate <- team_win_count / length(row.names(games_df)) 

team_label <- c('Team_One', 'Team_Two') 

percent <- function(x, digits = 2, format = "f", ...) { 
  
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%") 
  
} 

precent_team_win_rate <- c(percent(team_win_rate[1]), percent(team_win_rate[2])) 

team_win_rate_str <- c(toString(precent_team_win_rate[1]), toString(precent_team_win_rate[2])) 

chart_label <- c() 

for (i in 1:2){ 
  
  chart_label[i] <- paste(c(team_label[i], team_win_rate_str[i]), collapse=" - ") 
  
} 

pie3D(team_win_rate, labels=chart_label, col=c('blue', 'red'), explode=0.1, 
      
      main="Pie Chart of Team Win Rate") 


#9.    Which first to indicator is the best predictor of winner? 

#Which champion is present in games with the highest percentage of that indicator? 



#Make a dataframe for winners and first to indicators. 



compareFirst <- data.frame(games_df$winner, 
                           
                           games_df$firstBlood,  
                           
                           games_df$firstTower, 
                           
                           games_df$firstInhibitor,  
                           
                           games_df$firstBaron, 
                           
                           games_df$firstDragon, 
                           
                           games_df$firstRiftHerald 
                           
) 


#Rename the columns. 


colnames(compareFirst) = c("winner", "firstBlood", "firstTower", "firstInhibitor", "firstBaron", "firstDragon", "firstRiftHerald") 


#We will be using logit/probit again, so we need to set the proper result values. 


compareFirst$winner = ifelse (compareFirst$winner == 2, 1, 0) 


#Run logit analysis on these features. 

firstLM = lm(formula = winner ~ ., data = compareFirst) 

summary(firstLM) 



#Check for multicollinearity. 

vif(firstLM) 



#First inhibitor is the predictor with the largest coefficient value. Rift herald appears to not be significant at all. 

#Remove rift herald and rerun the regression. 



secondLM = lm(formula = winner ~ firstBlood + firstTower + firstInhibitor + firstBaron + firstDragon, data = compareFirst) 

summary(secondLM) 



#Try to predict the winner given the first to. 



compareFirst$predWinner = round(predict(secondLM, type = "resp")) 



#Percentage Correct 



perc_log2 <- length(which(compareFirst$winner==compareFirst$predWinner))/dim(compareFirst)[1] 

perc_log2 





#Confusion matrix using the logit model. 



#Create a table for the actual and predicted value counts. 



bq8results <- table(Actual = compareFirst$winner, Pred = compareFirst$predWinner) 



bq8plot = qplot(1:2, 1:2) +   theme_void() + annotation_custom(grob = tableGrob(bq8results)) + 
  
  annotate("text", x = 1.515, y = 1.65, label = "Prediction", fontface = 2) + annotate("text", x = 1.38, y = 1.57, label = "Actual", fontface = 2) + 
  
  annotate("text", x = 1.515, y = 1.35, label = "Percentage Correct - 82.96%", colour = "dark green", fontface = 2) 



bq8plot 


#10.    Which champion (or team composition) is in games with the most objectives?  

#Compile objectives for team 1 

games_df$T1CompiledObjectives[games_df$winner==1] <- ( 
  
  
  
  scale(games_df$t1_towerKills[games_df$winner==1], center = FALSE, scale = TRUE) +  
    
    scale(games_df$t1_inhibitorKills[games_df$winner==1], center = FALSE, scale = TRUE) + 
    
    scale(games_df$t1_baronKills[games_df$winner==1], center = FALSE, scale = TRUE) + 
    
    scale(games_df$t1_dragonKills[games_df$winner==1], center = FALSE, scale = TRUE) + 
    
    scale(games_df$t1_riftHeraldKills[games_df$winner==1], center = FALSE, scale = TRUE) ) 



#Return the champ with max compiled objectives for team 1 

champ_df$ChampionName[champ_df$ChampID==games_df$t1_champ1id[which.max(games_df$T1CompiledObjectives)]] 

champ_df$ChampionName[champ_df$ChampID==games_df$t1_champ2id[which.max(games_df$T1CompiledObjectives)]] 

champ_df$ChampionName[champ_df$ChampID==games_df$t1_champ3id[which.max(games_df$T1CompiledObjectives)]] 

champ_df$ChampionName[champ_df$ChampID==games_df$t1_champ4id[which.max(games_df$T1CompiledObjectives)]] 

champ_df$ChampionName[champ_df$ChampID==games_df$t1_champ5id[which.max(games_df$T1CompiledObjectives)]] 



#Compile objectives for team 2 

games_df$T2CompiledObjectives[games_df$winner==2] <- ( 
  
  
  
  scale(games_df$t2_towerKills[games_df$winner==2], center = FALSE, scale = TRUE) + 
    
    scale(games_df$t2_inhibitorKills[games_df$winner==2], center = FALSE, scale = TRUE) + 
    
    scale(games_df$t2_baronKills[games_df$winner==2], center = FALSE, scale = TRUE) + 
    
    scale(games_df$t2_dragonKills[games_df$winner==2], center = FALSE, scale = TRUE) + 
    
    scale(games_df$t2_riftHeraldKills[games_df$winner==2], center = FALSE, scale = TRUE) ) 



#Return the champ with max compiled objectives for team 2 

champ_df$ChampionName[champ_df$ChampID==games_df$t2_champ1id[which.max(games_df$T2CompiledObjectives)]] 

champ_df$ChampionName[champ_df$ChampID==games_df$t2_champ2id[which.max(games_df$T2CompiledObjectives)]] 

champ_df$ChampionName[champ_df$ChampID==games_df$t2_champ3id[which.max(games_df$T2CompiledObjectives)]] 

champ_df$ChampionName[champ_df$ChampID==games_df$t2_champ4id[which.max(games_df$T2CompiledObjectives)]] 

champ_df$ChampionName[champ_df$ChampID==games_df$t2_champ5id[which.max(games_df$T2CompiledObjectives)]] 



#Generate a new table with the top teams with most objectives for Team 1 

t1co <- head(games_df[order(-(games_df$T1CompiledObjectives)), ], 10)  



#Add the name of champion to the table 



#t1 c1 

t1con <- t1co$t1_champ1id 



for (x in t1con) { 
  
  
  
  t1co$t1_champ1name[t1co$t1_champ1id==x] <- unlist(champ_df$ChampionName[champ_df$ChampID==x]) 
  
  
  
}   



#c1 c2 



t1con <- t1co$t1_champ2id 



for (x in t1con) { 
  
  
  
  t1co$t1_champ2name[t1co$t1_champ2id==x] <- unlist(champ_df$ChampionName[champ_df$ChampID==x]) 
  
  
  
}   



#t1 c3 

t1con <- t1co$t1_champ3id 



for (x in t1con) { 
  
  
  
  t1co$t1_champ3name[t1co$t1_champ3id==x] <- unlist(champ_df$ChampionName[champ_df$ChampID==x]) 
  
  
  
}   



#t1 c4 

t1con <- t1co$t1_champ4id 



for (x in t1con) { 
  
  
  
  t1co$t1_champ4name[t1co$t1_champ4id==x] <- unlist(champ_df$ChampionName[champ_df$ChampID==x]) 
  
  
  
}   



#t1 c5 

t1con <- t1co$t1_champ5id 



for (x in t1con) { 
  
  
  
  t1co$t1_champ5name[t1co$t1_champ5id==x] <- unlist(champ_df$ChampionName[champ_df$ChampID==x]) 
  
  
  
}   



rownames(t1co) <- paste(t1co$t1_champ1name, t1co$t1_champ2name, t1co$t1_champ3name, t1co$t1_champ4name,t1co$t1_champ5name, sep="\n")  



#Generate a plot 

ggplot(t1co, aes(x=row.names(t1co), y=T1CompiledObjectives))+ geom_col(color="black", fill="skyblue") + ggtitle("Bar Chart: Top 10 Team 1 Compositions with Most Objective Kills") + theme(plot.title = element_text(hjust = 0.5)) + ylab("Objective Total") + xlab("Team Composition") + theme(axis.text.y = element_text(hjust=1, size = 6.5) )  





#Generate a similar plot for Team 2 



#Generate a new table with the top teams with most objectives for Team 2 

t2co <- head(games_df[order(-(games_df$T2CompiledObjectives)), ], 10)  



#Add the name of champion to the table 



#t2 c1 

t2con <- t2co$t2_champ1id 



for (x in t2con) { 
  
  
  
  t2co$t2_champ1name[t2co$t2_champ1id==x] <- unlist(champ_df$ChampionName[champ_df$ChampID==x]) 
  
  
  
}   



#c1 c2 



t2con <- t2co$t2_champ2id 



for (x in t2con) { 
  
  
  
  t2co$t2_champ2name[t2co$t2_champ2id==x] <- unlist(champ_df$ChampionName[champ_df$ChampID==x]) 
  
  
  
}   



#t2 c3 

t2con <- t2co$t2_champ3id 



for (x in t2con) { 
  
  
  
  t2co$t2_champ3name[t2co$t2_champ3id==x] <- unlist(champ_df$ChampionName[champ_df$ChampID==x]) 
  
  
  
}   



#t2 c4 

t2con <- t2co$t2_champ4id 



for (x in t2con) { 
  
  
  
  t2co$t2_champ4name[t2co$t2_champ4id==x] <- unlist(champ_df$ChampionName[champ_df$ChampID==x]) 
  
  
  
}   



#t2 c5 

t2con <- t2co$t2_champ5id 



for (x in t2con) { 
  
  
  
  t2co$t2_champ5name[t2co$t2_champ5id==x] <- unlist(champ_df$ChampionName[champ_df$ChampID==x]) 
  
  
  
}   



rownames(t2co) <- paste(t2co$t2_champ1name, t2co$t2_champ2name, t2co$t2_champ3name, t2co$t2_champ4name,t2co$t2_champ5name, sep="\n")  



#Generate a plot 

ggplot(t2co, aes(x=row.names(t2co), y=T2CompiledObjectives))+ geom_col(color="black", fill="darkred") + ggtitle("Bar Chart: Top 10 Team 2 Compositions with Most Objective Kills") + theme(plot.title = element_text(hjust = 0.5)) + ylab("Objective Total") + xlab("Team Composition") + theme(axis.text.y = element_text(hjust=1, size = 6.5) )  



#11 Summoner Spell Win Rates 



#Possible spells list. Exclude all spells that are not in summoner's rift and Clarity. 



spellList = summonerlist$SpellID[c(1:7,9,10)] 



#Number of times where a spell was chosen by a player. 



spellTotal = function(x){ 
  
  totalGames = c(games_df$t1_champ1_sum1[games_df$t1_champ1_sum1==x],   
                 
                 games_df$t1_champ2_sum1[games_df$t1_champ2_sum1==x],  
                 
                 games_df$t1_champ3_sum1[games_df$t1_champ3_sum1==x],   
                 
                 games_df$t1_champ4_sum1[games_df$t1_champ4_sum1==x],  
                 
                 games_df$t1_champ5_sum1[games_df$t1_champ5_sum1==x],  
                 
                 games_df$t2_champ1_sum1[games_df$t2_champ1_sum1==x],   
                 
                 games_df$t2_champ2_sum1[games_df$t2_champ2_sum1==x],  
                 
                 games_df$t2_champ3_sum1[games_df$t2_champ3_sum1==x],   
                 
                 games_df$t2_champ4_sum1[games_df$t2_champ4_sum1==x],  
                 
                 games_df$t2_champ5_sum1[games_df$t2_champ5_sum1==x], 
                 
                 games_df$t1_champ1_sum2[games_df$t1_champ1_sum2==x],   
                 
                 games_df$t1_champ2_sum2[games_df$t1_champ2_sum2==x],  
                 
                 games_df$t1_champ3_sum2[games_df$t1_champ3_sum2==x],   
                 
                 games_df$t1_champ4_sum2[games_df$t1_champ4_sum2==x],  
                 
                 games_df$t1_champ5_sum2[games_df$t1_champ5_sum2==x],  
                 
                 games_df$t2_champ1_sum2[games_df$t2_champ1_sum2==x],   
                 
                 games_df$t2_champ2_sum2[games_df$t2_champ2_sum2==x],  
                 
                 games_df$t2_champ3_sum2[games_df$t2_champ3_sum2==x],   
                 
                 games_df$t2_champ4_sum2[games_df$t2_champ4_sum2==x],  
                 
                 games_df$t2_champ5_sum2[games_df$t2_champ5_sum2==x] 
                 
  )  
  
  return(length(totalGames)) 
  
} 



#Percentage of time summoner taken by users. 10 Possoble players, 51490 possible instances of players. 



spellPercent = function (x){ 
  
  totalGames = c(games_df$t1_champ1_sum1[games_df$t1_champ1_sum1==x],   
                 
                 games_df$t1_champ2_sum1[games_df$t1_champ2_sum1==x],  
                 
                 games_df$t1_champ3_sum1[games_df$t1_champ3_sum1==x],   
                 
                 games_df$t1_champ4_sum1[games_df$t1_champ4_sum1==x],  
                 
                 games_df$t1_champ5_sum1[games_df$t1_champ5_sum1==x],  
                 
                 games_df$t2_champ1_sum1[games_df$t2_champ1_sum1==x],   
                 
                 games_df$t2_champ2_sum1[games_df$t2_champ2_sum1==x],  
                 
                 games_df$t2_champ3_sum1[games_df$t2_champ3_sum1==x],   
                 
                 games_df$t2_champ4_sum1[games_df$t2_champ4_sum1==x],  
                 
                 games_df$t2_champ5_sum1[games_df$t2_champ5_sum1==x], 
                 
                 games_df$t1_champ1_sum2[games_df$t1_champ1_sum2==x],   
                 
                 games_df$t1_champ2_sum2[games_df$t1_champ2_sum2==x],  
                 
                 games_df$t1_champ3_sum2[games_df$t1_champ3_sum2==x],   
                 
                 games_df$t1_champ4_sum2[games_df$t1_champ4_sum2==x],  
                 
                 games_df$t1_champ5_sum2[games_df$t1_champ5_sum2==x],  
                 
                 games_df$t2_champ1_sum2[games_df$t2_champ1_sum2==x],   
                 
                 games_df$t2_champ2_sum2[games_df$t2_champ2_sum2==x],  
                 
                 games_df$t2_champ3_sum2[games_df$t2_champ3_sum2==x],   
                 
                 games_df$t2_champ4_sum2[games_df$t2_champ4_sum2==x],  
                 
                 games_df$t2_champ5_sum2[games_df$t2_champ5_sum2==x] 
                 
  ) 
  
  
  
  percentage = length(totalGames)/(51490*10)*100 
  
  return(percentage) 
  
} 



#All percentages of posible summoner spell choices. 



SpellFreqTable = data.frame(SpellID = spellList,SpellFreq = unlist(lapply(spellList, spellPercent))) 

SpellFreqTable = merge(SpellFreqTable, summonerlist, by = "SpellID") 

SpellFreqTable = SpellFreqTable[,c(1,4,2)] 

SpellFreqTable$SpellFreq = paste(round(SpellFreqTable$SpellFreq,2), "%") 

SpellFreqTable 



#Plot the summoner spell frequency table. 


grid.table(SpellFreqTable) 


#Summoner Win Rate by Summoner Spell. 


SpellChampWin = function(c, s) { 
  
  #Number of times a summoner spell, s, appeared in a game with champion, c. 
  
  
  
  totaltimestaken = c( 
    
    games_df$t1_champ1_sum1[games_df$t1_champ1id == c & games_df$t1_champ1_sum1 == s], 
    
    games_df$t1_champ2_sum1[games_df$t1_champ2id == c & games_df$t1_champ2_sum1 == s], 
    
    games_df$t1_champ3_sum1[games_df$t1_champ3id == c & games_df$t1_champ3_sum1 == s], 
    
    games_df$t1_champ4_sum1[games_df$t1_champ4id == c & games_df$t1_champ4_sum1 == s], 
    
    games_df$t1_champ5_sum1[games_df$t1_champ5id == c & games_df$t1_champ5_sum1 == s], 
    
    games_df$t2_champ1_sum1[games_df$t2_champ1id == c & games_df$t2_champ1_sum1 == s], 
    
    games_df$t2_champ2_sum1[games_df$t2_champ2id == c & games_df$t2_champ2_sum1 == s], 
    
    games_df$t2_champ3_sum1[games_df$t2_champ3id == c & games_df$t2_champ3_sum1 == s], 
    
    games_df$t2_champ4_sum1[games_df$t2_champ4id == c & games_df$t2_champ4_sum1 == s], 
    
    games_df$t2_champ5_sum1[games_df$t2_champ5id == c & games_df$t2_champ5_sum1 == s], 
    
    games_df$t1_champ1_sum2[games_df$t1_champ1id == c & games_df$t1_champ1_sum1 == s], 
    
    games_df$t1_champ2_sum2[games_df$t1_champ2id == c & games_df$t1_champ2_sum1 == s], 
    
    games_df$t1_champ3_sum2[games_df$t1_champ3id == c & games_df$t1_champ3_sum1 == s], 
    
    games_df$t1_champ4_sum2[games_df$t1_champ4id == c & games_df$t1_champ4_sum1 == s], 
    
    games_df$t1_champ5_sum2[games_df$t1_champ5id == c & games_df$t1_champ5_sum1 == s], 
    
    games_df$t2_champ1_sum2[games_df$t2_champ1id == c & games_df$t1_champ1_sum1 == s], 
    
    games_df$t2_champ2_sum2[games_df$t2_champ2id == c & games_df$t2_champ2_sum1 == s], 
    
    games_df$t2_champ3_sum2[games_df$t2_champ3id == c & games_df$t3_champ3_sum1 == s], 
    
    games_df$t2_champ4_sum2[games_df$t2_champ4id == c & games_df$t4_champ4_sum1 == s], 
    
    games_df$t2_champ5_sum2[games_df$t2_champ5id == c & games_df$t5_champ5_sum1 == s] 
    
  ) 
  
  
  
  #Number of times a summoner spell, s, appeared in game with champion, c, and the champion won. 
  
  
  
  totaltimeswon = c( 
    
    games_df$t1_champ1_sum1[games_df$t1_champ1id == c & games_df$t1_champ1_sum1 == s & games_df$winner == 1], 
    
    games_df$t1_champ2_sum1[games_df$t1_champ2id == c & games_df$t1_champ2_sum1 == s & games_df$winner == 1], 
    
    games_df$t1_champ3_sum1[games_df$t1_champ3id == c & games_df$t1_champ3_sum1 == s & games_df$winner == 1], 
    
    games_df$t1_champ4_sum1[games_df$t1_champ4id == c & games_df$t1_champ4_sum1 == s & games_df$winner == 1], 
    
    games_df$t1_champ5_sum1[games_df$t1_champ5id == c & games_df$t1_champ5_sum1 == s & games_df$winner == 1], 
    
    games_df$t2_champ1_sum1[games_df$t2_champ1id == c & games_df$t2_champ1_sum1 == s & games_df$winner == 2], 
    
    games_df$t2_champ2_sum1[games_df$t2_champ2id == c & games_df$t2_champ2_sum1 == s & games_df$winner == 2], 
    
    games_df$t2_champ3_sum1[games_df$t2_champ3id == c & games_df$t2_champ3_sum1 == s & games_df$winner == 2], 
    
    games_df$t2_champ4_sum1[games_df$t2_champ4id == c & games_df$t2_champ4_sum1 == s & games_df$winner == 2], 
    
    games_df$t2_champ5_sum1[games_df$t2_champ5id == c & games_df$t2_champ5_sum1 == s & games_df$winner == 2], 
    
    games_df$t1_champ1_sum2[games_df$t1_champ1id == c & games_df$t1_champ1_sum1 == s & games_df$winner == 1], 
    
    games_df$t1_champ2_sum2[games_df$t1_champ2id == c & games_df$t1_champ2_sum1 == s & games_df$winner == 1], 
    
    games_df$t1_champ3_sum2[games_df$t1_champ3id == c & games_df$t1_champ3_sum1 == s & games_df$winner == 1], 
    
    games_df$t1_champ4_sum2[games_df$t1_champ4id == c & games_df$t1_champ4_sum1 == s & games_df$winner == 1], 
    
    games_df$t1_champ5_sum2[games_df$t1_champ5id == c & games_df$t1_champ5_sum1 == s & games_df$winner == 1], 
    
    games_df$t2_champ1_sum2[games_df$t2_champ1id == c & games_df$t1_champ1_sum1 == s & games_df$winner == 2], 
    
    games_df$t2_champ2_sum2[games_df$t2_champ2id == c & games_df$t2_champ2_sum1 == s & games_df$winner == 2], 
    
    games_df$t2_champ3_sum2[games_df$t2_champ3id == c & games_df$t3_champ3_sum1 == s & games_df$winner == 2], 
    
    games_df$t2_champ4_sum2[games_df$t2_champ4id == c & games_df$t4_champ4_sum1 == s & games_df$winner == 2], 
    
    games_df$t2_champ5_sum2[games_df$t2_champ5id == c & games_df$t5_champ5_sum1 == s & games_df$winner == 2] 
    
  ) 
  
  
  
  #Find the percentage won by dividing the length of the times the champion won by the length of appearances. 
  
  percentwon = length(totaltimeswon)/length(totaltimestaken)*100 
  
  return(round(percentwon,2)) 
  
} 



#Create a function that makes a table of Summoner spell and win rate when given a Champion ID. 

ChampSpellFreq = function(x) { 
  
  
  
  #Prepare the champion name for pasting later. 
  
  champpick = champlist$ChampionName[champlist$ChampID == x] 
  
  
  
  #Create a dummy vector to store the SpellChampWin results. 
  
  pblock = NULL 
  
  
  
  #Find the win percentage of all the summoner spells in question through iteration. 
  
  for (s in 1:length(SpellFreqTable$SpellID)) 
    
  { 
    
    pblock = c(pblock, SpellChampWin(x, SpellFreqTable$SpellID[s])) 
    
    
    
  } 
  
  
  
  #Create a data frame with the spell names and the frequency taken. 
  
  df = data.frame(name = SpellFreqTable$SpellName, freq = paste(pblock, "%") ) 
  
  
  
  #Rename the columns. 
  
  colnames(df) = c(paste(champpick,"SpellName"), paste(champpick,"SpellFreq")) 
  
  return(df) 
  
} 



YasuoSpellWinRate = ChampSpellFreq(157) 

YasuoSpellWinRate 



JannaSpellWinRate = ChampSpellFreq(40) 

JannaSpellWinRate 

#Plot the tables. 



grid.table(YasuoSpells) 

grid.table(JannaSpellWinRate) 

#12.    Which side gets more objectives in matches?  

#Reference business question #9 



if (mean(games_df$T1CompiledObjectives, na.rm=TRUE) > mean(games_df$T2CompiledObjectives, na.rm=TRUE)) print("Team 1 gets more objectives") else print("Team 2 gets more objectives") 



#Generate a pie chart to compare the objectives 

a <- mean(games_df$T1CompiledObjectives, na.rm=TRUE)  

b <- mean(games_df$T2CompiledObjectives, na.rm=TRUE) 



slices <- c(a, b) 

lbls <- c("Team 1 Compiled Objectives", "Team 2 Compiled Objectives") 

pct <- round(slices/sum(slices)*100, digits=2) 

lbls <- paste(lbls, pct)   

lbls <- paste(lbls,"%",sep="")  

pie(slices, labels = lbls, main="Average Compiled Objectives for Team 1 and Team 2", col = c("blue", "red")) 



#12 



#Load the libraries   

library("kernlab")   

library("e1071")   

library("ggplot2")   

library("gridExtra") 



#Compile objectives for team 1 

games_df$allobjt1 <- ( 
  
  
  
  scale(games_df$t1_towerKills, center = FALSE, scale = TRUE) +  
    
    scale(games_df$t1_inhibitorKills, center = FALSE, scale = TRUE) + 
    
    scale(games_df$t1_baronKills, center = FALSE, scale = TRUE) + 
    
    scale(games_df$t1_dragonKills, center = FALSE, scale = TRUE) + 
    
    scale(games_df$t1_riftHeraldKills, center = FALSE, scale = TRUE) ) 



#Compile objectives for team 2 

games_df$allobjt2 <- ( 
  
  
  
  scale(games_df$t2_towerKills, center = FALSE, scale = TRUE) + 
    
    scale(games_df$t2_inhibitorKills, center = FALSE, scale = TRUE) + 
    
    scale(games_df$t2_baronKills, center = FALSE, scale = TRUE) + 
    
    scale(games_df$t2_dragonKills, center = FALSE, scale = TRUE) + 
    
    scale(games_df$t2_riftHeraldKills, center = FALSE, scale = TRUE) ) 





#Create a separate dataframe with the kill objective and scale them as appropriate 

bokt <- data.frame(games_df$winner,games_df$allobjt1,games_df$allobjt2) 





#Rename the columns for objective kill table 

colnames(bokt) <-  c("winner", "Team1CompiledObjectives", "Team2CompiledObjectives") 



#Convert 

# Team 1 == 1 

# Team 2 == 0 

bokt$winner <- ifelse(bokt$winner==2, 0, 1) 





#covert winner to factor  

bokt$winner <- as.factor(bokt$winner)  



#Create a random index by sampling all indices in our dataset  

random <- sample(1:dim(bokt)[1])  



#Take a small subset of the data to analyze. R stalls/crashes when using more than 5000 training data 

random <- random[1:7500] 

bokt <- bokt[random, ] 

#reset the rownames/index 

rownames(bokt) <- NULL 



#Re-create the random index based on our smaller dataset  

random <- sample(1:dim(bokt)[1])  



#Create a cut point to separate our dataset into 2/3 Train and 1/3 Test  

cutpoint <- floor(2*(dim(bokt)[1])/3)  

cutpoint  



#Create the train data  

traindata <- bokt[random[1:cutpoint], ]  



#Create the test data  

testdata <- bokt[random[(cutpoint+1):dim(bokt)[1]], ]  



#Build a ksvm model  

ksvmmodel <- ksvm(winner~., data=traindata, kernal="rbfdot", kpar="automatic", C=10, cross=10, prob.model=TRUE)  



str(ksvmmodel) 



#test the model  

ksvmpred <- predict(ksvmmodel, testdata)  



#generate a comparison table  

comptable <- data.frame(testdata$winner, ksvmpred)  



#rename the columns  

colnames(comptable) <- c("test", "pred")  



#percentage of winners that was correctly predicted  

percent <- (length(which(comptable$test==comptable$pred)))/(dim(comptable)[1])*100   

percent  



#plot the results of ksvm function  

#determine if prediction is "correct" or "wrong" for each case  

comptable$correct <- ifelse(comptable$test==comptable$pred,"correct","wrong")  



#create a new dataframe with the data for the plot  

plotdata <- data.frame(comptable$correct, testdata$Team1CompiledObjectives, testdata$Team2CompiledObjectives, testdata$winner, comptable$pred)  



#rename the columns  

colnames(plotdata) <- c("Correct", "Team1CompiledObjectives", "Team2CompiledObjectives", "Winner", "Predict")  



#create a plot for the ksvm results  

ksvmplot <- ggplot(data=plotdata, aes(x=Team1CompiledObjectives, y=Team2CompiledObjectives)) + geom_point(data=plotdata, aes(size=Correct, color=Winner, shape=Predict)) + ggtitle("Winner by Team 1&2 Compiled Objectives ksvm Plot")  

ksvmplot  



#ksvm Confusion Matrix 

ksvmresults <- table(test=comptable$test, pred=comptable$pred) 

print(ksvmresults) 



#build a model using naive bayes   

nb <- naiveBayes(winner~., data=traindata)  



#test the model  

nbpred <- predict(nb, testdata)  



#generate a comparison table  

nbcomptable <- data.frame(testdata$winner, nbpred)  



#rename the columns  

colnames(nbcomptable) <- c("test", "pred")  



#percentage of winners that was correctly predicted  

nbpercent <- (length(which(nbcomptable$test==nbcomptable$pred)))/(dim(nbcomptable)[1])*100   

nbpercent  



#plot the results of the nb function  

#determine if prediction is "correct" or "wrong" for each case  

nbcomptable$correct <- ifelse(nbcomptable$test==nbcomptable$pred,"correct","wrong")  



#create a new dataframe with the data for the plot  

nbplotdata <- data.frame(nbcomptable$correct, testdata$Team1CompiledObjectives, testdata$Team2CompiledObjectives, testdata$winner, nbcomptable$pred)  



#rename the columns  

colnames(nbplotdata) <- c("Correct", "Team1CompiledObjectives", "Team2CompiledObjectives", "Winner", "Predict")  



#create a plot for the nb results  

nbplot <- ggplot(data=nbplotdata, aes(x=Team1CompiledObjectives, y=Team2CompiledObjectives)) + geom_point(data=nbplotdata, aes(size=Correct, color=Winner, shape=Predict)) + ggtitle("Winner by Team 1&2 Compiled Objectives naiveBayes Plot")  

nbplot  



#naïve Bayes Confusion Matrix 

nbresults <- table(test=nbcomptable$test, pred=nbcomptable$pred) 

print(nbresults) 



#Show both plots in one window  

grid.arrange(ksvmplot, nbplot, nrow=2, top="Winnner by Team 1&2 Objective Differential by Comparison")  





#13.    How much is Rift Herald an indicator of win chance? 

#There can only be one Rift Herald taken per game 



# If Team 1 wins and they get the rift herald 

q <-length(games_df$gameId[games_df$winner==1 & games_df$t1_riftHeraldKills==1]) 



# If Team 2 wins and they get the rift herald 

w <- length(games_df$gameId[games_df$winner==2 & games_df$t2_riftHeraldKills==1]) 



# If Team 1 wins and they get no rift herald 

e <- length(games_df$gameId[games_df$winner==1 & games_df$t1_riftHeraldKills==0]) 



# If Team 2 wins and they get no rift herald 

r <- length(games_df$gameId[games_df$winner==2 & games_df$t2_riftHeraldKills==0]) 



#Pie Chart Comparing the different Win Rates 

aslices <- c(q,w,e,r) 

albls <- c("Team 1 Win Rate with Rift Herald", "Team 2 Win Rate with Rift Herald", "Team 1 Win Rate without Rift Herald", "Team 2 Win Rate without Rift Herald") 

apct <- round(aslices/sum(aslices)*100, digits=2) 

albls <- paste(albls, apct, sep="\n")  

albls <- paste(albls,"%",sep="")  

pie(aslices, labels = albls, main="Team 1 and Team 2 Win Percentages given Rift Herald Kill", col = c("skyblue", "darkred", "blue", "red")) 



#Another Pie Chart Comparing the different Win Rates 

aaslices <- c(q+w, e+r) 

aalbls <- c("Win Percentage with Rift Herald", "Win Percentage without Rift Herald") 

aapct <- round(aaslices/sum(aaslices)*100, digits=2) 

aalbls <- paste(aalbls, aapct, sep="\n")  

aalbls <- paste(aalbls,"%",sep="")  

pie(aaslices, labels = aalbls, main="Win Percentages given Rift Herald Kill", col = c("blue", "red")) 



#14.    Create a function that takes a champion name as an input and returns a win rate for that champion and the top champion they lose to. 

library("modeest") 



nemesis <- function(cn){ 
  
  #Convert name into champion ID 
  
  y <- (champ_df$ChampID[champ_df$ChampionName==cn])[1] 
  
  
  
  n <- c(games_df$t1_champ1id[games_df$winner==2 & games_df$t1_champ1id!=y],   
         
         games_df$t1_champ2id[games_df$winner==2 & games_df$t1_champ2id!=y],  
         
         games_df$t1_champ3id[games_df$winner==2 & games_df$t1_champ3id!=y],   
         
         games_df$t1_champ4id[games_df$winner==2 & games_df$t1_champ4id!=y],  
         
         games_df$t1_champ5id[games_df$winner==2 & games_df$t1_champ5id!=y],  
         
         games_df$t2_champ1id[games_df$winner==1 & games_df$t2_champ1id!=y],   
         
         games_df$t2_champ2id[games_df$winner==1 & games_df$t2_champ2id!=y],  
         
         games_df$t2_champ3id[games_df$winner==1 & games_df$t2_champ3id!=y],   
         
         games_df$t2_champ4id[games_df$winner==1 & games_df$t2_champ4id!=y],  
         
         games_df$t2_champ5id[games_df$winner==1 & games_df$t2_champ5id!=y])  
  
  n <- mfv(n) 
  
  n <- champ_df$ChampionName[champ_df$ChampID==n] 
  
  
  
  return(paste(n, "is the nemesis of", cn)) 
  
} 



#Test 

nemesis("Tristana") 

nemesis("Annie") 

nemesis("Ahri") 

nemesis("Jinx") 



#15.    Create a function that takes a champion name as an input and returns the summoner spell combination with the highest win rate. 

library("modeest") 



champspellwincombo <- function(cn){ 
  
  #Convert name into champion ID 
  
  y <- (champ_df$ChampID[champ_df$ChampionName==cn])[1] 
  
  
  
  #Summoner slot 1 
  
  sum1wins<- c(games_df$t1_champ1_sum1[games_df$winner==1 & games_df$t1_champ1id==y],   
               
               games_df$t1_champ2_sum1[games_df$winner==1 & games_df$t1_champ2id==y],  
               
               games_df$t1_champ3_sum1[games_df$winner==1 & games_df$t1_champ3id==y],   
               
               games_df$t1_champ4_sum1[games_df$winner==1 & games_df$t1_champ4id==y],  
               
               games_df$t1_champ5_sum1[games_df$winner==1 & games_df$t1_champ5id==y],  
               
               games_df$t2_champ1_sum1[games_df$winner==2 & games_df$t2_champ1id==y],   
               
               games_df$t2_champ2_sum1[games_df$winner==2 & games_df$t2_champ2id==y],  
               
               games_df$t2_champ3_sum1[games_df$winner==2 & games_df$t2_champ3id==y],   
               
               games_df$t2_champ4_sum1[games_df$winner==2 & games_df$t2_champ4id==y],  
               
               games_df$t2_champ5_sum1[games_df$winner==2 & games_df$t2_champ5id==y])  
  
  sum1wins <- mfv(sum1wins) 
  
  sum1wins <- sum_spell_df$SummonerName[sum_spell_df$SummonerID==sum1wins] 
  
  
  
  #Summoner Slot 2 
  
  sum2wins<- c(games_df$t1_champ1_sum2[games_df$winner==1 & games_df$t1_champ1id==y],   
               
               games_df$t1_champ2_sum2[games_df$winner==1 & games_df$t1_champ2id==y],  
               
               games_df$t1_champ3_sum2[games_df$winner==1 & games_df$t1_champ3id==y],   
               
               games_df$t1_champ4_sum2[games_df$winner==1 & games_df$t1_champ4id==y],  
               
               games_df$t1_champ5_sum2[games_df$winner==1 & games_df$t1_champ5id==y],  
               
               games_df$t2_champ1_sum2[games_df$winner==2 & games_df$t2_champ1id==y],   
               
               games_df$t2_champ2_sum2[games_df$winner==2 & games_df$t2_champ2id==y],  
               
               games_df$t2_champ3_sum2[games_df$winner==2 & games_df$t2_champ3id==y],   
               
               games_df$t2_champ4_sum2[games_df$winner==2 & games_df$t2_champ4id==y],  
               
               games_df$t2_champ5_sum2[games_df$winner==2 & games_df$t2_champ5id==y])  
  
  sum2wins <- mfv(sum2wins) 
  
  sum2wins <- sum_spell_df$SummonerName[sum_spell_df$SummonerID==sum2wins] 
  
  return(paste(cn, "wins the most with", sum1wins, "and", sum2wins)) 
  
} 



#Test 

champspellwincombo("Tristana") 

champspellwincombo("Ahri") 

champspellwincombo("Jinx") 

champspellwincombo("Caitlyn") 



#16.    Create a function that takes a champion name as an input and returns the champion they win the most with. 

library("modeest") 



champpartner <- function(cn){ 
  
  #Convert name into champion ID 
  
  y <- (champ_df$ChampID[champ_df$ChampionName==cn])[1] 
  
  
  
  duo <- c(games_df$t1_champ1id[games_df$winner==1 & games_df$t1_champ1id!=y],   
           
           games_df$t1_champ2id[games_df$winner==1 & games_df$t1_champ2id!=y],  
           
           games_df$t1_champ3id[games_df$winner==1 & games_df$t1_champ3id!=y],   
           
           games_df$t1_champ4id[games_df$winner==1 & games_df$t1_champ4id!=y],  
           
           games_df$t1_champ5id[games_df$winner==1 & games_df$t1_champ5id!=y],  
           
           games_df$t2_champ1id[games_df$winner==2 & games_df$t2_champ1id!=y],   
           
           games_df$t2_champ2id[games_df$winner==2 & games_df$t2_champ2id!=y],  
           
           games_df$t2_champ3id[games_df$winner==2 & games_df$t2_champ3id!=y],   
           
           games_df$t2_champ4id[games_df$winner==2 & games_df$t2_champ4id!=y],  
           
           games_df$t2_champ5id[games_df$winner==2 & games_df$t2_champ5id!=y])  
  
  duo <- mfv(duo) 
  
  duo <- champ_df$ChampionName[champ_df$ChampID==duo] 
  
  
  
  return(paste(cn, "wins the most with", duo)) 
  
} 



#Test 

champpartner("Tristana") 

champpartner("Ahri") 

champpartner("Jinx") 

champpartner("Caitlyn") 