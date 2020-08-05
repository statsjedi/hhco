library(tidyverse)
library(readxl)
library(matrixcalc) #for matrix.power()
library(gt)
library(webshot) #for gtsave

#===============================================================
#model the game as an absorbing Markov chain

hhco_markov <- read_excel("hhco_markov.xlsx")

initial_state <- c(1, rep(0,10))

hhco_markov <- as.matrix(hhco_markov) #convert from data frame to matrix

rounds <- 100 #number of game rounds

results_matrix <- matrix(nrow = 11, ncol = rounds) #make matrix of results

for(i in 1:rounds){ #calculate result matrix
  results_matrix[,i] <- matrix.power(hhco_markov, i) %*% initial_state
}

#the above calculated results for a 1 player game. Let's extend to 2 and 3 player games
cumulative_results1 <- tibble(round=seq(1,rounds,1), p=1-results_matrix[11,])
cumulative_results1$player1 <- 1-cumulative_results1$p #this is the same as results_matrix[11,]
cumulative_results1$player2 <- 1-cumulative_results1$p^2
cumulative_results1$player3 <- 1-cumulative_results1$p^3

#calculate the number of rounds to finish for certain probabilities
cumulative_results_breaks <- c(0.1, 0.25, 0.5, 0.75, 0.9, 0.99) #probability levels
cumulative_results2 <- tibble(x=cumulative_results_breaks,
                              y1=approx(x=cumulative_results1$player1, y=cumulative_results1$round, 
                                        xout=cumulative_results_breaks)$y,
                              y2=approx(x=cumulative_results1$player2, y=cumulative_results1$round, 
                                        xout=cumulative_results_breaks)$y,
                              y3=approx(x=cumulative_results1$player3, y=cumulative_results1$round, 
                                        xout=cumulative_results_breaks)$y)

#make a nice looking table of probability calculations
t.probs <- cumulative_results2 %>% 
  mutate(y1=ceiling(y1), y2=ceiling(y2), y3=ceiling(y3)) %>% 
  rename(`Probability`=x, `1-Player Game`=y1, `2-Player Game`=y2, `3-Player Game`=y3) %>% 
  gt() %>% 
  tab_header(title="Probabilities of Finishing \"Hi Ho! Cherry-O\" within n Rounds")

t.probs

#tidy cumulative_results for ggplot
cumulative_results1b <- 
  cumulative_results1 %>% 
  select(-p) %>% 
  pivot_longer(-round) %>% 
  mutate(players=0) %>% 
  mutate(players=replace(players, name=="player1", 1),
         players=replace(players, name=="player2", 2),
         players=replace(players, name=="player3", 3)) 

#plot probabilities
p.cumulative <- cumulative_results1b %>% 
  filter(round <= 60) %>% 
  ggplot(aes(x=round, y=value, group=players, linetype=as_factor(players)))+
  geom_line(size=1)+
  theme_bw()+
  xlab("Game Length (Rounds)")+
  ylab("Probability of Finishing")+
  labs(title="Modeling \"Hi Ho! Cherry-O\" as an Absorbing Markov Chain", linetype="Number of\nPlayers")+
  theme(plot.title = element_text(size = 12, family="Arial Narrow", face="bold"),
        axis.title = element_text(size = 10, family="Arial Narrow"), 
        axis.text = element_text(size = 10, family="Arial Narrow"),
        legend.text = element_text(size = 10, family="Arial Narrow"), 
        legend.title = element_text(size = 10, family="Arial Narrow"))+
  scale_y_continuous(breaks=seq(0, 1, .2))+
  scale_x_continuous(breaks=seq(0, 60, 10))

p.cumulative
  
ggsave("hhco.png", p.cumulative, width=7, height=5, units="in")  
gtsave(t.probs, "t_probs.png")
