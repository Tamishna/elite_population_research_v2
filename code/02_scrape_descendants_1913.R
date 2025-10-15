## Top wealth and its historical origins: Identifying entrenched fortunes by linking rich lists over 100 years
## Authors: Daria Tisch and Emma Ischinsky
## Task: Generate graph with all descendants of 1913 rich list members who have connection to 2019 list 
##      --> Query family members (SPARQL query, Wikidata API) and generate igraph object
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

# Packages
pkgs <- c(
  "readxl",
  "writexl",
  "tidyverse",
  "WikidataR",
  "igraph"
)


## Install uninstalled packages
lapply(pkgs[!(pkgs %in% installed.packages())], install.packages)

## Load all packages to library
lapply(pkgs, library, character.only = TRUE)


# Clean environment
rm(list = ls())





#### 1. Load and prepare data ####

# Individuals on rich list 1913
df_1913 = read_excel("data/dynastic_1913_openrefined.xlsx")
df_1913 = df_1913[!is.na(df_1913$id_fam),]

df_1913 = df_1913 %>%
  rename(id_gnd ="GND ID") %>%
  mutate(wiki_id_husband = replace(wiki_id_husband, wiki_id_husband == "0", NA),
         id_relative = replace(id_relative, id_relative == "0", NA),
         id_gnd = ifelse(is.na(id_gnd),GND_2, id_gnd),
         id_wikidata = ifelse(is.na(id_wikidata),wiki_id_husband, id_wikidata),
         id_wikidata = ifelse(is.na(id_wikidata),id_relative, id_wikidata)
  ) %>%
  select(id_fam, name, id_wikidata)

# Check missing values (id_wikidata)
sum(!is.na(df_1913$id_wikidata))
sum(is.na(df_1913$id_wikidata))



#### 2. SPAQRL query ####
colnames <- c("item", "itemLabel")
df_wiki <- data.frame(matrix(ncol = length(colnames), nrow = 1))
colnames(df_wiki) <- colnames


df1 =df_1913 %>% drop_na(id_wikidata) %>% select(id_wikidata)
df1 = unique(df1)
df1 = df1 %>% filter(!(id_wikidata %in% c("Q62553","Q60504","Q58010","Q66424","Q2677","Q215902","Q57569","Q1674265","Q63679","Q1635114")))
wiki_list = as.list(df1$id_wikidata)
remove(df1)




###### 2.1 Get all family members ######

sparql_query01 = "SELECT ?item ?itemLabel
WHERE{
  wd:"

sparql_query02 = "  (wdt:P1038|^wdt:P1038 | wdt:P3373|^wdt:P3373 | wdt:P40|^wdt:P40 | wdt:P26|^wdt:P26 | wdt:P22|^wdt:P22| wdt:P25|^wdt:P25 |  (wdt:P26/(wdt:P22|wdt:P25|wdt:P40)))? / 
             (wdt:P1038|^wdt:P1038 | wdt:P3373|^wdt:P3373 | wdt:P40|^wdt:P40 | wdt:P26|^wdt:P26 | wdt:P22|^wdt:P22| wdt:P25|^wdt:P25 |  (wdt:P26/(wdt:P22|wdt:P25|wdt:P40)))? / 
             (wdt:P1038|^wdt:P1038 | wdt:P3373|^wdt:P3373 | wdt:P40|^wdt:P40 | wdt:P26|^wdt:P26 | wdt:P22|^wdt:P22| wdt:P25|^wdt:P25 |  (wdt:P26/(wdt:P22|wdt:P25|wdt:P40)))? / 
             (wdt:P1038|^wdt:P1038 | wdt:P3373|^wdt:P3373 | wdt:P40|^wdt:P40 | wdt:P26|^wdt:P26 | wdt:P22|^wdt:P22| wdt:P25|^wdt:P25 |  (wdt:P26/(wdt:P22|wdt:P25|wdt:P40)))?  ?item. 
  SERVICE wikibase:label { bd:serviceParam wikibase:language 'de,en'. } 
}
ORDER BY DESC(?item)
"

sparql_query02_2 = "  (wdt:P1038|^wdt:P1038 | wdt:P3373|^wdt:P3373 | wdt:P40|^wdt:P40 | wdt:P26|^wdt:P26 | wdt:P22|^wdt:P22| wdt:P25|^wdt:P25 |  (wdt:P26/(wdt:P22|wdt:P25|wdt:P40)))? / 
             (wdt:P1038|^wdt:P1038 | wdt:P3373|^wdt:P3373 | wdt:P40|^wdt:P40 | wdt:P26|^wdt:P26 | wdt:P22|^wdt:P22| wdt:P25|^wdt:P25 |  (wdt:P26/(wdt:P22|wdt:P25|wdt:P40)))? / 
             (wdt:P1038|^wdt:P1038 | wdt:P3373|^wdt:P3373 | wdt:P40|^wdt:P40 | wdt:P26|^wdt:P26 | wdt:P22|^wdt:P22| wdt:P25|^wdt:P25 |  (wdt:P26/(wdt:P22|wdt:P25|wdt:P40)))?  ?item. 
  SERVICE wikibase:label { bd:serviceParam wikibase:language 'de,en'. } 
}
ORDER BY DESC(?item)
"


if (!file.exists("data_prepared/fam_members_richies1913_2019.RData")) {
  for (x in wiki_list) {
    print(x)
    # now we write the sparql query
    sparql_query <- paste0(sparql_query01, x, sparql_query02)
    df_single=tryCatch({query_wikidata(sparql_query, format = c("tibble"))},
                       error = function(e){  data.frame(item = "NA", itemLabel = x) })
    df_single =  df_single %>%
      mutate(across(everything(), as.character))
    df_wiki = bind_rows(df_wiki, df_single)
    df_wiki = unique(df_wiki)
  }
  save(df_wiki, file = "data_prepared/fam_members_richies1913_2019.RData")
} else{
  load("data_prepared/fam_members_richies1913_2019.RData")
}



# Error handling: for a view there was a time out error, so we shorten the SPARQL query for those:
wiki_list2 = c("Q62553","Q60504","Q58010","Q66424","Q2677","Q215902","Q57569","Q1674265","Q63679","Q1635114")


if (!file.exists("data_prepared/fam_members_richies1913_2019_2.RData")) {
  for (x in wiki_list2) {
    print(x)
    # now we write the sparql query
    sparql_query <- paste0(sparql_query01, x, sparql_query02_2)
    df_single=tryCatch({query_wikidata(sparql_query, format = c("simple"))},
                       error = function(e){  data.frame(item = "NA", itemLabel = x) })
    df_single =  df_single %>%
      mutate(across(everything(), as.character))
    df_wiki = bind_rows(df_wiki, df_single)
    df_wiki = unique(df_wiki)
  }
  save(df_wiki, file = "data_prepared/fam_members_richies1913_2019_2.RData")
} else{
  load("data_prepared/fam_members_richies1913_2019_2.RData")
}


df_wiki = unique(df_wiki)
# 10210 individuals

remove(wiki_list, wiki_list2, sparql_query01, sparql_query02, sparql_query02_2)




###### 2.2 Get info of all family members ######

df_wiki_info <- data.frame(matrix(ncol = length(colnames), nrow = 1))
colnames(df_wiki_info) <- colnames

wikilist_info = df_wiki %>% filter(!is.na(item)) # %>%  subset( !(id_wikidata %in% c("Q56150413"))) 
wikilist_info = as.list(wikilist_info$item)

sparql_query03 = "SELECT ?item ?itemLabel ?father ?fatherLabel ?mother ?motherLabel ?sibling ?siblingLabel ?relative ?relativeLabel ?child ?spouse ?spouseLabel ?childLabel
?genderLabel  (YEAR(?birth_date) as ?birth_year) (YEAR(?death_date) as ?death_year) 
WHERE{
  wd:"

sparql_query04 = "   wdt:? ?item. # 
  OPTIONAL {?item wdt:P1038 ?relative .}
  OPTIONAL {?item wdt:P40 ?child .}
  OPTIONAL {?item wdt:P26 ?spouse .}
  OPTIONAL {?item wdt:P22 ?father .}
  OPTIONAL {?item wdt:P25 ?mother .}
  OPTIONAL {?item wdt:P3373 ?sibling .}
  OPTIONAL {?item wdt:P21 ?gender .}
  OPTIONAL {?item wdt:P569 ?birth_date .}
  OPTIONAL {?item wdt:P570 ?death_date .}
  SERVICE wikibase:label { bd:serviceParam wikibase:language 'de,en'. } 
}
ORDER BY DESC(?item)
"
if (!file.exists("data_prepared/richies1913_2019_with_info.RData")) {
  for (i in 1:length(wikilist_info)) {
    x = wikilist_info[[i]]
    print(x)
    print(i)
    # now we write the sparql query
    sparql_query <- paste0(sparql_query03, x, sparql_query04)
    df_single=query_wikidata(sparql_query, format = c("tibble"))
    df_single =  df_single %>%
      mutate(across(everything(), as.character))
    df_wiki_info = bind_rows(df_wiki_info, df_single)
  }
  save(df_wiki_info, file = "data_prepared/richies1913_2019_with_info.RData")
} else{
  load("data_prepared/richies1913_2019_with_info.RData")
}

df_wiki_info = unique(df_wiki_info)

df_wiki = df_wiki %>%
  mutate(node = item) %>%
  filter(!is.na(item))
df_wiki_info  = df_wiki_info %>%
  mutate(node = item) %>%
  filter(!is.na(item))






#### 3. Generating network graph ####

#* Nodes: Individuals
#* Edges: family relationship: father, mother, spouse, relative, child


# generate relationship lists (edge list)
df_spouse = df_wiki_info %>%
  select(item, spouse, itemLabel, spouseLabel)  %>%
  filter(!is.na(spouse)) %>%
  mutate(from = item,
         to = spouse,
         type = "has_spouse"
  ) %>%
  relocate(from, to, type)
# drop duplicates
df_spouse = unique(df_spouse) # 7410 spouse relationships (both directions)

df_mother = df_wiki_info %>%
  select(item, mother, itemLabel, motherLabel)  %>%
  filter(!is.na(mother)) %>%
  mutate(from = item,
         to = mother,
         type = "has_mother"
  ) %>%
  relocate(from, to, type)
# drop duplicates
df_mother = unique(df_mother) # 8044 mothers



df_father = df_wiki_info %>%
  select(item, father, itemLabel, fatherLabel)  %>%
  filter(!is.na(father)) %>%
  mutate(from = item,
         to = father,
         type = "has_father"
  ) %>%
  relocate(from, to, type)
# drop duplicates
df_father = unique(df_father) # 8686 fathers



df_siblings = df_wiki_info %>%
  select(item, sibling, itemLabel, siblingLabel)  %>%
  filter(!is.na(sibling)) %>%
  mutate(from = item,
         to = sibling,
         type = "has_sibling"
  ) %>%
  relocate(from, to, type)
# drop duplicates
df_siblings = unique(df_siblings) # 12810 siblings


df_relative = df_wiki_info %>%
  select(item, relative, itemLabel, relativeLabel)  %>%
  filter(!is.na(relative)) %>%
  mutate(from =item,
         to = relative, 
         type = "has_relative"
  ) %>%
  relocate(from, to, type)
# drop duplicates
df_relative = unique(df_relative) # 549 relatives


df_child = df_wiki_info %>%
  select(item, child, itemLabel, childLabel)  %>%
  filter(!is.na(child)) %>%
  mutate(from = item,
         to = child, 
         type = "has_child"
  ) %>%
  relocate(from, to, type)
# drop duplicates
df_child = unique(df_child) # 20517 child



edges = bind_rows(df_spouse, df_father, df_mother, df_siblings, df_relative, df_child) %>%
  relocate(from, to, type) %>%
  mutate(fromLabel = itemLabel,
         toLabel = ifelse(type == "has_mother", motherLabel, 
                          ifelse(type == "has_spouse", spouseLabel,
                                 ifelse(type == "has_father", fatherLabel, 
                                        ifelse(type == "has_sibling", siblingLabel, 
                                               ifelse(type == "has_relative", relativeLabel,
                                                      ifelse(type == "has_child", childLabel,NA))))))
  )
table(edges$type, useNA = "always")





# generate node list (vertices list)
nodes <- unique(data.frame(node = c(edges[,"from"], edges[,"to"]), 
                           nodeLabel =  c(edges[,"fromLabel"], edges[,"toLabel"])))
nodes = nodes %>%
  mutate(id = row_number()-1)
#18649 nodes

nodes["nodeLabel2"] = paste(nodes$nodeLabel, nodes$id)


nodes["group"] = ifelse(nodes$node %in% df_1913$id_wikidata, "reich 1912", "descendant")
table(nodes$group)
table(nodes$node %in% df_1913$id_wikidata) # 123 rich individuals with at least one relative in Wikidata

nodes = nodes %>%
  mutate( label1913 = ifelse(group == "reich 1912", nodeLabel, ""),
  )


edges$value <- 1 

# drop single duplicates
nodes$dup = duplicated(nodes$node)
nodes = nodes[!duplicated(nodes$node), ] 
nodes = nodes %>%
  mutate(index = row_number())

edges$dup = duplicated(edges[c("from","to","type")])
edges = edges[!duplicated(edges[c("from","to","type")]), ] 

## generate igraph object
graph <- graph_from_data_frame(edges, directed=TRUE, vertices = nodes)
save(graph,nodes, edges, file = "data_prepared/dynastic_igraph.Rdata")

# The end