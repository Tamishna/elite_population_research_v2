## Top wealth and its historical origins: Identifying entrenched fortunes by linking rich lists over 100 years
## Authors: Daria Tisch and Emma Ischinsky
## Task: Generate graph with ancestors of 2019 rich list members
##      --> Query family members (SPARQL query, Wikidata API) and generate igraph object
## 2023-06-29



#### 0. Organisation ####

# Set working directory

# Print the machine's node name (used right below to pick a working directory)
Sys.info()['nodename']

work_dir = ifelse(Sys.info()['nodename']=="P2010", 
                  "C:/Users/ti/Local/seafile/main/---projects---/mm2019/replication_package",
                  "D:/Seafile/main/---projects---/mm2019/replication_package")
setwd(work_dir)
getwd()  # sanity check: shows the active working directory


# Packages used:
# - readxl / writexl: read & write Excel files
# - tidyverse: data wrangling (dplyr, tidyr, etc.)
# - WikidataR: run SPARQL queries against Wikidata
# - stringr: string helpers (e.g., startsWith patterns)
# - igraph: build the family graph (nodes = people, edges = relationships)

pkgs <- c(
  "readxl",
  "writexl",
  "tidyverse",
  "WikidataR",
  "stringr",
  "igraph"
)


## Install uninstalled packages
lapply(pkgs[!(pkgs %in% installed.packages())], install.packages)

# Load all packages into the session
lapply(pkgs, library, character.only = TRUE)

# Clean environment
rm(list = ls())





#### 1. Load and prepare data ####

# Individuals on rich list 2019
# Load the 2019 rich-list data that includes Wikidata IDs (if available).
df = read_excel("data/wikidata2019.xlsx")

# how many rows have a valid Wikidata entity (Q-IDs)? 439
df_test = df  %>% filter(!is.na(id_wikidata)) %>%
  select(id_wikidata, id_fam, name) %>%
  distinct(id_wikidata, .keep_all = TRUE)

# Count distinct families represented among those with Wikidata IDs
length(unique(df_test$id_fam)) # 394 families

remove(df_test)







##### 2. SPAQRL query #####
# Goal of section 2:
#   (1) Given the 2019 people with known Wikidata IDs, fetch their family NETWORK neighborhood
#       (parents/children/spouses/relatives/siblings) up to ~4 steps away.
#   (2) Then, for ALL discovered people, fetch detailed relationship triples to build edges.


# Small, named two-column tibble to hold the first wave of results:
# 'item'      -> Wikidata Q-ID for a related person
# 'itemLabel' -> Human-readable label for that Q-ID
colnames <- c("item", "itemLabel")
df_wiki <- data.frame(matrix(ncol = length(colnames), nrow = 1))
colnames(df_wiki) <- colnames


# 'wiki_list' is a 1-column tibble; we'll loop over it and query each Q-ID.
wiki_list = unique(df %>% select(id_wikidata) %>% filter(!is.na(id_wikidata)))





###### 2.1 Get all family members ######
# We will construct a SPARQL query in two parts and paste in the Q-ID in the middle.
# The path expression walks through common family relations (P-properties):
#  - P1038: relative
#  - P3373: sibling
#  - P40: child
#  - P26: spouse
#  - P22: father
#  - P25: mother
# The ( ... )? /( ... )? /( ... )? /( ... )? sequence means "up to 4 hops" in any of those relations.



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

# We cache the results to disk so repeated runs don't hammer Wikidata or waste time.
# If the cache file exists, we just load it; otherwise we loop through each seed Q-ID.
if (!file.exists("data_prepared/fam_members_richies2019.RData")) {
  for (i in 1:length(wiki_list$id_wikidata)) {
    print(i)
    x = wiki_list$id_wikidata[[i]]
    print(x)
    # now we write the sparql query
    sparql_query <- paste0(sparql_query01, x, sparql_query02)
    
    # Run the query. If the endpoint times out or errors, return a minimal row noting the Q-ID.
    df_single=tryCatch({query_wikidata(sparql_query, format = c("tibble"))},
                       error = function(e){  data.frame(item = "NA", itemLabel = x) })
    
    # Coerce everything to character to avoid type clashes on rbind
    df_single =  df_single %>%
      mutate(across(everything(), as.character))
    df_wiki = bind_rows(df_wiki, df_single)
    df_wiki = unique(df_wiki)
  }
  save(df_wiki, file = "data_prepared/fam_members_richies2019.RData")
} else{
  load("data_prepared/fam_members_richies2019.RData")
}


# Keep only valid Wikidata Q-IDs (drop NA or blank or non-Q rows)
df_wiki = df_wiki %>%
  filter(stringr::str_starts(item, 'Q'))

# 3282 individuals

remove(wiki_list, sparql_query01, sparql_query02)




###### 2.2 Get info of all family members 
#Get info (relationship triples) for all discovered people 
# Goal: For every 'item' (Q-ID) we found above, pull detailed links:
# father, mother, sibling, relative, child, spouse, gender, birth/death year.
# We'll use these to construct the edge list.


# Fresh holder for the detailed rows we’re about to fetch
df_wiki_info <- data.frame(matrix(ncol = length(colnames), nrow = 1))
colnames(df_wiki_info) <- colnames

# Build a simple list of Q-IDs to iterate over
wikilist_info = df_wiki %>% filter(!is.na(item))
wikilist_info = as.list(wikilist_info$item)

# Two-part SPARQL again; we fill the Q-ID in between.
# IMPORTANT: 'wdt:?' is a hack used in the original code: it means "any property"
# to bind ?item, then OPTIONAL lines pull specific properties we care about.
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

# Same caching pattern as above
if (!file.exists("data_prepared/rich2019_with_info.RData")) {
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
    df_wiki_info = unique(df_wiki_info)
  }
  save(df_wiki_info, file = "data_prepared/rich2019_with_info.RData")
} else{
  load("data_prepared/rich2019_with_info.RData")
}








# Final light cleanup: add a 'node' column mirroring 'item' (igraph likes 'name'/'node' keys),
# and drop non-IDs
df_wiki = df_wiki %>%
  mutate(node = item) %>%
  filter(!is.na(item))
df_wiki_info  = df_wiki_info %>%
  mutate(node = item) %>%
  filter(!is.na(item))


remove(sparql_query03, sparql_query04, colnames, wikilist_info)



#### 3. Generating network graph ####
# Concept:
#   - Each unique person (Q-ID) becomes a node.
#   - Each family relationship becomes a directed edge between two nodes.
#     (Direction chosen here is "from source person" -> "related person".)


# generate relationship lists (edge list)
# SPOUSE edges (bidirectional relationships are frequently duplicated in Wikidata)
df_spouse = df_wiki_info %>%
  select(item, spouse, itemLabel, spouseLabel)  %>%
  filter(!is.na(spouse)) %>%
  mutate(from = item,
         to = spouse,
         type = "has_spouse"
  ) %>%
  relocate(from, to, type)
# drop duplicates
df_spouse = unique(df_spouse) # 2079 spouse relationships (both directions)

df_mother = df_wiki_info %>%
  select(item, mother, itemLabel, motherLabel)  %>%
  filter(!is.na(mother)) %>%
  mutate(from = item,
         to = mother,
         type = "has_mother"
  ) %>%
  relocate(from, to, type)
# drop duplicates
df_mother = unique(df_mother) # 1987 mothers



df_father = df_wiki_info %>%
  select(item, father, itemLabel, fatherLabel)  %>%
  filter(!is.na(father)) %>%
  mutate(from = item,
         to = father,
         type = "has_father"
  ) %>%
  relocate(from, to, type)
# drop duplicates
df_father = unique(df_father) # 2283 fathers



df_siblings = df_wiki_info %>%
  select(item, sibling, itemLabel, siblingLabel)  %>%
  filter(!is.na(sibling)) %>%
  mutate(from = item,
         to = sibling,
         type = "has_sibling"
  ) %>%
  relocate(from, to, type)
# drop duplicates
df_siblings = unique(df_siblings) # 3418 siblings


df_relative = df_wiki_info %>%
  select(item, relative, itemLabel, relativeLabel)  %>%
  filter(!is.na(relative)) %>%
  mutate(from =item,
         to = relative, 
         type = "has_relative"
  ) %>%
  relocate(from, to, type)
# drop duplicates
df_relative = unique(df_relative) # 363 relatives


df_child = df_wiki_info %>%
  select(item, child, itemLabel, childLabel)  %>%
  filter(!is.na(child)) %>%
  mutate(from = item,
         to = child, 
         type = "has_child"
  ) %>%
  relocate(from, to, type)
# drop duplicates
df_child = unique(df_child) # 5379 child


#Combine all edge tables and attach readable labels
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


# Stack all 'from' and 'to' endpoints and de-duplicate to get the full node set.
# node     -> Q-ID
# nodeLabel-> readable label from Wikidata
# generate node list (vertices list)
nodes <- unique(data.frame(node = c(edges[,"from"], edges[,"to"]), 
                           nodeLabel =  c(edges[,"fromLabel"], edges[,"toLabel"])))

# Give each node an integer ID 
nodes = nodes %>%
  mutate(id = row_number()-1)
#6074 nodes

#nodeLabel2 can be useful when exporting/visualizing (label + unique numeric ID)
nodes["nodeLabel2"] = paste(nodes$nodeLabel, nodes$id)


# Tag which nodes are in the 2019 seed list vs. ancestors discovered through traversal.
# If a node's Q-ID is in df$id_wikidata, it's a "rich 2019" seed; otherwise it's an "ancestor".
nodes["group"] = ifelse(nodes$node %in% df$id_wikidata, "rich 2019", "ancestor")

# Sanity checks: how many in each group?
table(nodes$group, useNA = "always")
table(nodes$node %in% df$id_wikidata) # 193 rich individuals with at least one relative in Wikidata

# Convenience label used later in visuals: only label the "rich 2019" nodes
nodes = nodes %>%
  mutate(nodeLabel3 = ifelse(group == "rich 2019", nodeLabel, ""),
  )

# Edge weight placeholder (uniform = 1 here; adjust if you want different strengths)
edges$value <- 1 

# drop single duplicates by Q-ID
nodes$dup = duplicated(nodes$node)
nodes = nodes[!duplicated(nodes$node), ] 
nodes = nodes %>%
  mutate(index = row_number())

# Remove duplicate edges by (from, to, type)
edges$dup = duplicated(edges[c("from","to","type")])
edges = edges[!duplicated(edges[c("from","to","type")]), ] 

## generate igraph object
graph <- graph_from_data_frame(edges, directed=TRUE, vertices = nodes)
save(graph,nodes, edges, file = "data_prepared/mm2019_igraph.Rdata")


# The end