library(tidyverse)
library(tidygraph)
library(ggraph)
library(readxl)

#make edges and nodes
edges <- read_excel("hhco_markov.xlsx", sheet = "network")
nodes <- tibble(node_key=seq(1, 11, 1), label=seq(0, 10, 1), color=c(rep("white", 10), "black"))

#edges can't have 0 in the list
edges$from <- edges$from + 1  
edges$to <- edges$to + 1

#make network object
routes_tidy <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)

#calculate centrality
routes_tidy <- 
  routes_tidy %>% 
  mutate(centrality = centrality_authority())

#This lets us get the fractional probability legend titles to use in the other plot
p.network_color <- ggraph(routes_tidy, layout = "focus", focus=11) + 
  geom_edge_fan(aes(color = as.factor(probability)), 
                arrow = arrow(length = unit(4, 'mm')), 
                end_cap = circle(5, 'mm')) + 
  geom_node_point(aes(color=cut_interval(centrality, 5)), shape=21, size=5, stroke=3, fill="white") +
  geom_node_text(aes(label = label), color="black", size=3) +
  labs(edge_color= "Probability", color="Centrality", 
       title="Network Analysis of \"Hi Ho! Cherry-O\"") +
  scale_edge_color_brewer(palette = "Set1", labels=c("1/7", "2/7", "3/7", "4/7"))+
  scale_color_viridis_d()+
  theme_graph()+
  theme(legend.text = element_text(size = 10), legend.title = element_text(size = 10),
        plot.title = element_text(size = 12))

p.network_color

#the network plot
p.network <- ggraph(routes_tidy, layout = "focus", focus=11) + 
  geom_edge_fan(aes(edge_width = probability), color="#737373",
                arrow = arrow(length = unit(4, 'mm')), 
                end_cap = circle(5, 'mm')) + 
  geom_edge_loop(aes(edge_width = probability),color="#737373",
                 arrow = arrow(length = unit(4, 'mm')), 
                 end_cap = circle(5, 'mm')) + 
  geom_node_point(aes(color=cut_interval(centrality, 5)), shape=21, size=5, stroke=3, fill="white") +
  geom_node_text(aes(label = label), color="black", size=3) +
  labs(edge_width = "Probability", color="Centrality", 
       title="Network Analysis of \"Hi Ho! Cherry-O\"") +
  scale_edge_width(range=c(0.2, 3))+ #set thickness
  scale_color_viridis_d()+
  theme_graph()+
  theme(legend.text = element_text(size = 10), legend.title = element_text(size = 10),
        plot.title = element_text(size = 12))

p.network

ggsave("hhco_network_color.png", p.network_color, width=7, height=5, units="in")  
ggsave("hhco_network.png", p.network, width=7, height=5, units="in")  
