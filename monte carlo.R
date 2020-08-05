library(tidyverse) 

spinFunc <- function(tokens, maxTokens){
  #HHCO has a spinner with 7 positions.
  spinner <- seq(1, 7, 1)
  spin <- sample(spinner, 1)
  if(spin == 1) #1 apple
  {tokens <- tokens - 1}
  else if(spin == 2) #2 apples
  {tokens <- tokens - 2}
  else if(spin == 3) #3 apples
  {tokens <- tokens - 3}
  else if(spin == 4) #4 apples
  {tokens <- tokens - 4}
  else if(spin == 5) #bird
  {tokens <- tokens + 2}
  else if(spin == 6) #dog
  {tokens <- tokens + 2}
  else #basket
  {tokens <- maxTokens}
  
  #Adjust to make sure that the final number of tokens is between the minimum and maximum limits
  if(tokens < 0) 
  {tokens <- 0}
  if(tokens > maxTokens) 
  {tokens <- maxTokens}
  
  return(tokens)
}

play_hhco_func <- function(numGames, players){
  
  startTokens <- 10 #The game starts with 10 cherries in each tree
  #initialize
  finalData <- data.frame(green=numeric(), purple=numeric(), red=numeric(), 
                            round=numeric(), game=numeric(), winner=character())
  
  #play games
  for(i in 1:numGames){
    gameData <- data.frame(green=startTokens, purple=startTokens, red=startTokens, round=0, game=i)
    counter <-0
    tempGreen <- startTokens
    tempPurple <- startTokens
    tempRed <- startTokens
    
    while(tempGreen > 0 & tempPurple > 0 & tempRed > 0){ #play a game until someone gets to 0 tokens
      counter <- counter + 1
      tempTokens <- gameData[counter,]
      
      tempTokens$green <- spinFunc(tempTokens$green, startTokens)
      if(players>=2){
        tempTokens$purple <- spinFunc(tempTokens$purple, startTokens)
      } 
      if(players>=3){
        tempTokens$red <- spinFunc(tempTokens$red, startTokens)
      }
      
      tempTokens$round <- counter
      gameData <- rbind(gameData, tempTokens)
      
      tempGreen <- tempTokens$green
      tempPurple <- tempTokens$purple
      tempRed <- tempTokens$red
      
    }
    
    gameLength <- length(gameData$green) #get the length of the data frame
    if(gameData$green[gameLength]==0){
      gameData$winner <- "green"}
    else if(gameData$purple[gameLength]==0){
      gameData$winner <- "purple"}
    else {
      gameData$winner <- "red"
    }
    finalData <- bind_rows(finalData, gameData)
  }
  
  finalDataTidy <- data.frame(tokens=c(finalData$green, finalData$purple, finalData$red),
                              color=c(rep("green", length(finalData$green)),
                                      rep("purple", length(finalData$purple)),
                                      rep("red", length(finalData$red))),
                              round=rep(finalData$round, 3),
                              game=rep(finalData$game, 3),
                              winner=rep(finalData$winner, 3))
  
  finalDataTidy2 <- finalDataTidy[finalDataTidy$tokens==0,] #save only the rounds when someone won
  finalDataTidy2$num_players <- players
  
  return(finalDataTidy2)
  
}

numGames <- 1000

#simulate 1, 2, and 3 player games
finalData_1 <- play_hhco_func(numGames, 1)
finalData_2 <- play_hhco_func(numGames, 2)
finalData_3 <- play_hhco_func(numGames, 3)

#bind it all together
allData <- bind_rows(finalData_1, finalData_2, finalData_3)

#Automatically generate subtitle based on number of games 
subTitle <- sprintf("%s Simulated Games of Hi Ho! Cherry-O", format(numGames, big.mark   = ","))

#plot
ggplot(allData, aes(x=round, linetype=as.factor(num_players)))+
  stat_ecdf(geom = "smooth", color="black")+
  theme_bw()+
  xlab("Game Rounds")+
  ylab("Cumulative Probability")+
  theme(axis.text=element_text(size=10),
        axis.title = element_text(size=12))+
  labs(title="How Long Will This Game Last?", subtitle = subTitle, 
       linetype="Number of\nPlayers")