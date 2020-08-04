library(tidyverse)
library(tidygraph)
library(ggraph)
library(readxl)
library(matrixcalc) #for matrix.power()
library(patchwork)

edges <- read_excel("hhco_markov.xlsx", sheet = "network2")
nodes <- tibble(node_key=seq(1, 11, 1), label=seq(0, 10, 1), color=c(rep("white", 10), "black"))

edges$from <- edges$from + 1  #can't have 0
edges$to <- edges$to + 1 #can't have 0

routes_tidy <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)

routes_tidy <- 
  routes_tidy %>% 
  mutate(centrality = centrality_authority())

#This lets us get the probability legend titles to use in the other plot
p.network_color <- ggraph(routes_tidy, layout = "focus", focus=11) + 
  geom_edge_fan(aes(color = as.factor(prob)), 
                arrow = arrow(length = unit(4, 'mm')), 
                end_cap = circle(5, 'mm')) + 
  geom_node_point(aes(color=cut_interval(centrality, 5)), shape=21, size=8, stroke=4, fill="white") +
  geom_node_text(aes(label = label), color="black") +
  labs(edge_color= "Probability", color="Centrality", title="Network Analysis") +
  scale_edge_color_brewer(palette = "Set1", labels=c("1/7", "2/7", "3/7", "4/7"))+
  scale_color_viridis_d()+
  theme_graph()+
  theme(legend.text = element_text(size = 14), legend.title = element_text(size = 16))

p.network_color

p.network2 <- ggraph(routes_tidy, layout = "focus", focus=11) + 
  geom_edge_fan(aes(edge_width = prob), color="#737373",
                arrow = arrow(length = unit(4, 'mm')), 
                end_cap = circle(5, 'mm')) + 
  geom_edge_loop(aes(edge_width = prob),color="#737373",
                 arrow = arrow(length = unit(4, 'mm')), 
                 end_cap = circle(5, 'mm')) + 
  geom_node_point(aes(color=cut_interval(centrality, 5)), shape=21, size=8, stroke=4, fill="white") +
  geom_node_text(aes(label = label), color="black") +
  labs(edge_width = "Probability", color="Centrality", title="Network Analysis") +
  scale_edge_width(range=c(0, 3))+ #set thickness
  scale_color_viridis_d()+
  theme_graph()+
  theme(legend.text = element_text(size = 14), legend.title = element_text(size = 16))

p.network2

#===============================================================

hhco_markov <- read_excel("hhco_markov.xlsx")
hhco_markov <- hhco_markov[,1:11]

initial_state <- c(1, rep(0,10))

hhco_markov <- as.matrix(hhco_markov) #convert from data frame to matrix

rounds <- 100 #number of game rounds

results_matrix <- matrix(nrow = 11, ncol = rounds) #make matrix of results

for(i in 1:rounds){ #calculate result matrix
  results_matrix[,i] <- matrix.power(hhco_markov, i) %*% initial_state
}

cum_results <- data.frame(round=seq(1,rounds,1), cum_p=results_matrix[11,])
cum_results_breaks <- approx(x=cum_results$cum_p, y=cum_results$round, xout=c(0.1, 0.25, 0.5, 0.75, 0.9, 0.99))
cum_results2 <- data.frame(round=cum_results_breaks$y, cum_p=cum_results_breaks$x)
cum_results2$labels <- sprintf("%.0f%% chance of\nending within %i rounds", cum_results2$cum_p*100, 
                               round(cum_results2$round,0))
cum_results2
#plot cumulative probabilities
p.cum <- 
  ggplot(cum_results, aes(x=round, y=cum_p))+
  geom_line(size=1)+
  theme_classic()+
  geom_point(data=cum_results2, aes(x=round, y=cum_p), size=5, fill="red", shape=21)+
  geom_text(data=cum_results2, aes(x=round, y=cum_p, label=labels),
            hjust=0, nudge_y = -0.07, nudge_x = 1, size=4)+
  xlab("Number of Rounds")+
  ylab("Probability")+
  labs(title="Cumulative Probability Plot")+
  theme(plot.title = element_text(size = 18, family="Arial Narrow", face="bold"),
        axis.title = element_text(size = 14), axis.text = element_text(size = 14))

p.cum

#===============================================================

p.patchwork <- p.network2 / p.cum +
  plot_annotation(title = "Modeling \"Hi Ho! Cherry-O\" as an Absorbing Markov Chain",
                  theme = theme(plot.title = element_text(size = 24)))+
  plot_layout(heights = c(1.2, 1))

p.patchwork

ggsave("hhco.png", p.patchwork, width=10, height=10.5, units="in")  

p.patchwork.color <- p.network_color / p.cum +
  plot_annotation(title = "Modeling \"Hi Ho! Cherry-O\" as an Absorbing Markov Chain",
                  theme = theme(plot.title = element_text(size = 24)))+
  plot_layout(heights = c(1.2, 1))

ggsave("hhco_color_dummy.png", p.patchwork.color, width=10, height=10.5, units="in")  
