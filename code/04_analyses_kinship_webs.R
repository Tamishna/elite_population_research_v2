## Top wealth and its historical origins: Identifying entrenched fortunes by linking rich lists over 100 years
## Authors: Daria Tisch and Emma Ischinsky
## Task: Analyses, Part II of II (kinship webs)
## 2023-06-29



#### 0. Organisation ####

# Set working directory
Sys.info()['nodename']
work_dir = ifelse(Sys.info()['nodename']=="P2010", 
                  "C:/Users/ti/Local/seafile/main/---projects---/mm2019/replication_package",
                  "D:/Seafile/main/---projects---/mm2019/replication_package")
setwd(work_dir)
getwd()


# Packages
pkgs <- c(
  "tidyverse",
  "readxl",
  "writexl",
  "ggplot2",
  "igraph",
  "ggraph",
  "ggiraph"
) 

## Install uninstalled packages
lapply(pkgs[!(pkgs %in% installed.packages())], install.packages)

## Load all packages to library
lapply(pkgs, library, character.only = TRUE)


# Clean environment
rm(list = ls())



#### 1. Generate network graph ####

# Load data
load(file = "data_prepared/mm2019_igraph.Rdata")
edges2019 = edges
graph2019 = graph
nodes2019 = nodes
remove(edges, graph, nodes)


load(file = "data_prepared/dynastic_igraph.Rdata")
edges1913 = edges
graph1913 = graph
nodes1913 = nodes
remove(edges, graph, nodes)


graph_combine = igraph::union(graph1913, graph2019, byname = "auto")
#nodes
nodes = igraph::as_data_frame(graph_combine, what = c("vertices"))
nodes = rownames_to_column(nodes, var = "node")
nodes = nodes %>%
  mutate(nodeLabel = ifelse(is.na(nodeLabel_1), nodeLabel_2, nodeLabel_1),
         group_1 = ifelse(is.na(group_1), "family", group_1),
         group_2 = ifelse(is.na(group_2), "family", group_2),
         group = ifelse(group_1=="reich 1912","rich 1912",
                        ifelse(group_2=="rich 2019","rich 2019", "family")),
         label = ifelse(group=="rich 1912"| group=="rich 2019", nodeLabel, ""),
         lvl = ifelse(group == "rich 1912", 1,
                      ifelse(group == "family", 1,
                             ifelse(group == "rich 2019", 2, NA)))
  ) %>%
  filter(startsWith(node, "Q"))  %>%
  select(node, label, nodeLabel, group, lvl)
nodes = unique(nodes)


# identify entrenched nodes

df = read_excel("data/mm2019.xlsx") %>%
  select(id_fam, full_name, rich1913)
df_wiki = read_excel("data/wikidata2019.xlsx") %>%
  select(id_fam, name, id_wikidata)
df = left_join(df, df_wiki, by = c("full_name" = "name")) %>%
  filter(rich1913==1,
         !is.na(id_wikidata)) %>%
  select(id_wikidata)

nodes = nodes %>%
  mutate(group = ifelse(node %in% df$id_wikidata , "rich 2019, entrenched", group))
table(nodes$group)


#edges
edges = igraph::as_data_frame(graph_combine, what = c("edges"))
edges =edges %>%
  mutate(type = ifelse(is.na(type_1), type_2, type_1)) %>%
  filter(startsWith(to, "Q"))  %>%
  select(from, to, type)

table(edges$type, useNA = "always")
# Dont differentiate by parenthood (mother or father)
edges = edges %>%
  mutate(
    type = ifelse(type == "has_sibling", "has_relative", type),
    from2 = ifelse(type == "has_father" | type == "has_mother", to, from),
    to2 = ifelse(type == "has_father" | type == "has_mother", from, to),
    type2 = ifelse(type == "has_father" | type == "has_mother", "has_child", type)
  ) %>%
  select(from2, to2,type2) %>%
  rename(from = from2,
         to = to2,
         type = type2)
table(edges$type, useNA = "always")
edges = edges %>%
  mutate(weight = ifelse(type == "has_child", 100,10),
         weight = ifelse(type == "has_spouse", 50 ,weight))
edges = unique(edges)
graph = graph_from_data_frame(edges, directed = TRUE, vertices = nodes)


remove(edges1913, edges2019, graph1913, graph2019, nodes1913, nodes2019, graph_combine)



#### 2. Network analyses ####


##### 2.1 Kinship networks of the German super-rich (Figure 4) #####

# select only graph components which include at least one of the rich 2019
component_list  = decompose(graph, mode = c("weak"), max.comps = NA, min.vertices = 0)

graphs_with_richies = c()
table(nodes$group)
richies2019 = nodes %>% filter(group=="rich 2019" | group =="rich 2019, entrenched")

for (x in 1:length(component_list)) {
  print(x)
  liste_true = richies2019$node %in% V(component_list[[x]])$name
  y = ifelse("TRUE" %in% liste_true, x, "NA")
  graphs_with_richies = append(graphs_with_richies, y)
} 

graphs_with_richies = graphs_with_richies[graphs_with_richies != "NA"]


g2 = component_list[[1]]

for (x in graphs_with_richies[2:length(graphs_with_richies)]) {
  print(x)
  y = as.numeric(x)
  print(y)
  g2 = union(g2, component_list[[y]], byname = "auto") 
}


nodes2 = as_data_frame(g2, what = c( "vertices")) %>% select(name) %>%
  left_join(nodes, by=c("name" = "node"))

nodes2 = nodes2 %>%
  mutate(index = row_number(),
         node = name)


edges2 = as_data_frame(g2, what = c( "edges"))%>% select(from, to) %>% unique() %>%
  left_join(edges %>% unique(), by=c("from" = "from","to" = "to"))


# Numbers for the paper
table(nodes2$group)
# family             rich 1912             rich 2019 rich 2019, entrenched 
# 20118                    76                   167                    26 

# Because it takes some time to generate the layout, we save and load it
#graphi_layout <- create_layout(g2, "stress")
#save(graphi_layout, file = "data/graphi_layout.RData")
load(file = "data/graphi_layout.Rdata")



all_all = ggraph(graphi_layout)+
  geom_edge_link0(alpha = 0.4) +  #arrow = arrow(length = unit(1, 'mm'), ends = "last",  type = "closed"),aes(color = type )
  geom_node_point( col = "white") +
  geom_node_point(aes(fill = nodes2$group, size = nodes2$group, alpha = nodes2$group  ), shape = 21) +
  theme_graph()+
  scale_fill_manual(values=c("gray97","gold","darkred", "blue"),
                    name="Nodes: Individuals",
                    labels=c("family member", "rich 1912", "rich 2019", "rich 2019 (entrenched)")) +
  scale_size_manual(values= c(0.5,2,2,2)) +
  scale_alpha_manual(values= c(0.1,1,1,1)) +
  guides(size = "none", alpha = "none")   +
  theme(legend.text=element_text(size=12, family = "serif"),
        legend.title=element_text(size=12, family = "serif"))

all_all

ggsave(all_all, filename = "graphs/figure_04.jpeg",  width = 9, height = 6, units = "in", type = "cairo")
ggsave(all_all, filename = "graphs/figure_04.eps",  width = 9, height = 6, units = "in", device=cairo_ps)


svg(filename = "graphs/figure_04.svg",
    width = 10, height = 7, pointsize = 16)
all_all
dev.off()


##### 2.2 Largest kinship network (Figure 5) #####

g2 = decompose(graph, mode = c("weak"), max.comps = NA, min.vertices = 0)

lg_nodes = as_data_frame(g2[[1]], what = c( "vertices"))

lg_nodes = lg_nodes %>%
  mutate(index = row_number())

lg_edges = as_data_frame(g2[[1]], what = c( "edges"))

richies = lg_nodes %>%
  filter(group!="family")

lg = as.undirected(
  g2[[1]],
  mode = c("collapse")
)

sp = shortest_paths(
  lg,
  from = V(lg)[name=="Q18649186"],
  to = richies$name,
  out = "both",
  weights = NULL
)


subi = induced_subgraph(g2[[1]], unlist(sp$vpath) )

subi_nodes = as_data_frame(subi, what = c( "vertices"))
subi_nodes = subi_nodes %>%
  mutate(index = row_number(),
         label2 = ifelse(group != "family", name, ""),
         label2 = ifelse(group == "rich 2019", nodeLabel, label2),
         label3 = ifelse(group == "rich 2019" | group == "rich 2019, entrenched", label, ""))

subi_edges = as_data_frame(subi, what = c( "edges"))

subi_layout <- create_layout(subi, "stress")
lgraph3 = ggraph(subi_layout) +
  geom_edge_link0(aes(color = type ), edge_width = 5/.pt, alpha = 0.7 ) +
  geom_point_interactive(aes(x = x, y = y,
                             tooltip = paste0(subi_nodes$nodeLabel),
                             color = subi_nodes$group,
                             onclick=paste0('window.open("', "http://www.wikidata.org/entity/", name , '")')),
                         size = 20/.pt) +
  theme_graph()+
  scale_color_manual(values=c("gray","gold","darkred", "blue"),
                     name = "Nodes: Individuals",
                     labels= c("family member", "rich in 1912", "rich in 2019")) +
  scale_edge_color_manual(values=c("black", "blue", "pink"),
                          name = "Edges: family relationship",
                          labels=c("child-parent", "relatives", "spouses")) +
  guides(size = "none")   +
  theme(legend.text=element_text(size=26), legend.title=element_text(size=26),
        legend.position = c(0.08, 0.85), text = element_text(family = "serif"))  

# Interactive graph (html with links to Wikidata)
girafe(ggobj = lgraph3, width_svg = 25, height_svg = 25,
       options = list(opts_sizing(rescale = TRUE),
                      opts_hover_inv(css = "opacity:0.1;"),
                      opts_hover(css = "fill:red;")))

lg_widget <- girafe(ggobj = lgraph3, width_svg = 25, height_svg = 25,
                    options = list(opts_sizing(rescale = TRUE),
                                   opts_hover_inv(css = "opacity:0.1;"),
                                   opts_hover(css = "fill:red;")))
lg_widget


htmlwidgets::saveWidget(lg_widget, "graphs/figure_05.html")


subi_layout <- create_layout(subi, "stress")

lgraph =ggraph(subi_layout) +
  geom_edge_link0(aes(linetype = type ), edge_width = 0.7/.pt, alpha = 0.7 ) + #edge_colour = "grey66",
  geom_node_point(aes(fill = group, size = group),  shape = 21, alpha = 0.6)+
  geom_node_label(aes(filter= (group == "rich 2019" | group == "rich 2019, entrenched"), label = subi_nodes$label3, size = group ),
                  family = "serif" , size = 9/.pt ,
                  fontface = "bold" ,
                  #nudge_x = 2,
                  label.size= 0,
                  alpha = 1,
                  label.padding=.01,
                  repel = TRUE) + #
  theme_graph()+
  scale_size_manual(values=c(3/.pt,5.5/.pt,7/.pt,7/.pt)) +
  scale_fill_manual(values=c("gray","gold","darkred", "blue"),
                    name = "Nodes: Individuals",
                    labels= c("family member", "rich 1912", "rich 2019", "rich 2019 (entrenched)")) +
  scale_edge_linetype_manual(values=c("solid", "longdash", "dotted"),
                             name = "Edges: family relationship",
                             labels=c("child-parent", "relatives", "spouses")) +
  guides(size = "none")   +
  theme(legend.text=element_text(size=12), legend.title=element_text(size=12),
        text = element_text(family = "serif"))  
lgraph
ggsave(lgraph, filename = "graphs/figure_05.jpeg",
       width = 9, height = 6, units = "in",
       bg = "white", type = "cairo")

ggsave(lgraph, filename = "graphs/figure_05.eps",
       width = 9, height = 6, units = "in",
       bg = "white", device=cairo_ps, fallback_resolution = 600)


svg(filename = "graphs/figure_05.svg",
    width = 10, height = 7, pointsize = 16)
lgraph
dev.off()

# Numbers for paper
table(lg_nodes$group)
# family             rich 1912             rich 2019 rich 2019, entrenched 
# 19468                    46                     8                     7 

# The end