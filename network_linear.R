source("network.R")

p.network_linear <- ggraph(routes_tidy, layout = "linear") + 
  geom_edge_arc(aes(edge_width = probability), color="#737373",
                arrow = arrow(length = unit(4, 'mm')), 
                end_cap = circle(5, 'mm')) + 
  geom_edge_loop(aes(edge_width = probability),color="#737373",
                 arrow = arrow(length = unit(4, 'mm')), 
                 end_cap = circle(5, 'mm')) + 
  geom_node_point(aes(color=cut_interval(centrality, 5)), shape=21, size=8, stroke=4, fill="white") +
  geom_node_text(aes(label = label), color="black") +
  labs(edge_width = "Probability", color="Centrality", 
       title="Network Analysis of \"Hi Ho! Cherry-O\"") +
  scale_edge_width(range=c(0.2, 3))+ #set thickness
  scale_color_viridis_d()+
  theme_graph()+
  theme(legend.text = element_text(size = 14), legend.title = element_text(size = 16))

p.network_linear

p.network_linear_color <- ggraph(routes_tidy, layout = "linear") + 
  geom_edge_arc(aes(color = as.factor(probability)), 
                arrow = arrow(length = unit(4, 'mm')), 
                end_cap = circle(5, 'mm')) + 
  geom_edge_loop(aes(color = as.factor(probability)), 
                 arrow = arrow(length = unit(4, 'mm')), 
                 end_cap = circle(5, 'mm')) + 
  geom_node_point(aes(color=cut_interval(centrality, 5)), shape=21, size=8, stroke=4, fill="white") +
  geom_node_text(aes(label = label), color="black") +
  labs(edge_color = "Probability", color="Centrality", 
       title="Network Analysis of \"Hi Ho! Cherry-O\"") +
  scale_edge_color_brewer(palette = "Set1", labels=c("1/7", "2/7", "3/7", "4/7"))+
  scale_color_viridis_d()+
  theme_graph()+
  theme(legend.text = element_text(size = 14), legend.title = element_text(size = 16))

p.network_linear_color

ggsave("hhco_network_linear_color.png", p.network_linear_color, width=9, height=5, units="in")  
ggsave("hhco_network_linear.png", p.network_linear, width=9, height=5, units="in")  
