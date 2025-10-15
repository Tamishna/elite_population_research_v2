

```r
#### 0. Organisation ####

# Set working directory

# Print the machine's node name (used right below to pick a working directory)
Sys.info()['nodename']
```

```
##                                 nodename 
## "vl965-172-31-125-49.wireless.umass.edu"
```

```r
work_dir = "/Users/Tamishna/Desktop/Elite Population Research/V2"
setwd(work_dir)
getwd()  # sanity check: shows the active working directory
```

```
## [1] "/Users/Tamishna/Desktop/Elite Population Research/V2"
```

```r
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
```

```
## list()
```

```r
# Load all packages into the session
lapply(pkgs, library, character.only = TRUE)
```

```
## [[1]]
##  [1] "igraph"    "WikidataR" "lubridate" "forcats"   "stringr"   "dplyr"     "purrr"     "readr"     "tidyr"     "tibble"    "ggplot2"   "tidyverse" "writexl"  
## [14] "readxl"    "stats"     "graphics"  "grDevices" "utils"     "datasets"  "methods"   "base"     
## 
## [[2]]
##  [1] "igraph"    "WikidataR" "lubridate" "forcats"   "stringr"   "dplyr"     "purrr"     "readr"     "tidyr"     "tibble"    "ggplot2"   "tidyverse" "writexl"  
## [14] "readxl"    "stats"     "graphics"  "grDevices" "utils"     "datasets"  "methods"   "base"     
## 
## [[3]]
##  [1] "igraph"    "WikidataR" "lubridate" "forcats"   "stringr"   "dplyr"     "purrr"     "readr"     "tidyr"     "tibble"    "ggplot2"   "tidyverse" "writexl"  
## [14] "readxl"    "stats"     "graphics"  "grDevices" "utils"     "datasets"  "methods"   "base"     
## 
## [[4]]
##  [1] "igraph"    "WikidataR" "lubridate" "forcats"   "stringr"   "dplyr"     "purrr"     "readr"     "tidyr"     "tibble"    "ggplot2"   "tidyverse" "writexl"  
## [14] "readxl"    "stats"     "graphics"  "grDevices" "utils"     "datasets"  "methods"   "base"     
## 
## [[5]]
##  [1] "igraph"    "WikidataR" "lubridate" "forcats"   "stringr"   "dplyr"     "purrr"     "readr"     "tidyr"     "tibble"    "ggplot2"   "tidyverse" "writexl"  
## [14] "readxl"    "stats"     "graphics"  "grDevices" "utils"     "datasets"  "methods"   "base"     
## 
## [[6]]
##  [1] "igraph"    "WikidataR" "lubridate" "forcats"   "stringr"   "dplyr"     "purrr"     "readr"     "tidyr"     "tibble"    "ggplot2"   "tidyverse" "writexl"  
## [14] "readxl"    "stats"     "graphics"  "grDevices" "utils"     "datasets"  "methods"   "base"
```

```r
# Clean environment
rm(list = ls())





#### 1. Load and prepare data ####

# Individuals on rich list 2019
# Load the 2019 rich-list data that includes Wikidata IDs (if available).
df = read_excel("/Users/Tamishna/Desktop/Elite Population Research/V2/Data/wikidata2019 copy.xlsx")


# how many rows have a valid Wikidata entity (Q-IDs)? 439
df_test = df  %>% filter(!is.na(id_wikidata)) %>%
  select(id_wikidata, id_fam, name) %>%
  distinct(id_wikidata, .keep_all = TRUE)

# Count distinct families represented among those with Wikidata IDs
length(unique(df_test$id_fam)) # 394 families
```

```
## [1] 394
```

```r
remove(df_test)


##### 2. Ancestor-only traversal (parents P22/P25) #####
```

```r
# Config
```

```r
dir.create("data_prepared", showWarnings = FALSE, recursive = TRUE)
dir.create("cache/parents", showWarnings = FALSE, recursive = TRUE)

MAX_DEPTH   <- 20        # safety cap on generations (tune as needed)
SEED_LIMIT  <- 25        # set to a small number (e.g., 25) for a pilot run; NA = all
BASE_SLEEP  <- 0.25      # polite delay between requests (seconds)
RETRY_TRIES <- 3
BACKOFF     <- 1.7

# Build seed vector of 2019 Q-IDs (skip NA)
seeds <- df %>%
  filter(!is.na(id_wikidata)) %>%
  distinct(id_wikidata) %>%
  pull(id_wikidata)

if (!is.na(SEED_LIMIT)) {
  seeds <- head(seeds, SEED_LIMIT)
}

length(seeds)  # sanity check; expect ~439 for full run
```

```
## [1] 25
```

```r
# Helpers: caching + safe query
```

```r
parent_cache_file <- function(qid) file.path("cache/parents", paste0(qid, ".rds"))

safe_query <- function(sparql, tries = RETRY_TRIES, sleep = BASE_SLEEP, backoff = BACKOFF) {
  attempt <- 1
  repeat {
    out <- tryCatch(
      { WikidataR::query_wikidata(sparql, format = "tibble") },
      error = function(e) e
    )
    if (!inherits(out, "error")) return(out)
    if (attempt >= tries) stop(out)
    Sys.sleep(sleep)
    sleep <- sleep * backoff
    attempt <- attempt + 1
  }
}

# Return tibble: one row with child + optional father/mother and labels/years.
# Uses SERVICE labels and pulls birth/death years for item and parents.
wd_get_parents <- function(qid, use_cache = TRUE) {
  cf <- parent_cache_file(qid)
  if (use_cache && file.exists(cf)) {
    return(readRDS(cf))
  }
  
  sparql <- paste0(
    "SELECT ?item ?itemLabel ?father ?fatherLabel ?mother ?motherLabel ",
    "(YEAR(?item_birth) as ?item_birth_year) (YEAR(?item_death) as ?item_death_year) ",
    "(YEAR(?father_birth) as ?father_birth_year) (YEAR(?father_death) as ?father_death_year) ",
    "(YEAR(?mother_birth) as ?mother_birth_year) (YEAR(?mother_death) as ?mother_death_year) ",
    "WHERE { ",
    "  BIND(wd:", qid, " AS ?item) ",
    "  OPTIONAL { ?item wdt:P569 ?item_birth . } ",
    "  OPTIONAL { ?item wdt:P570 ?item_death . } ",
    "  OPTIONAL { ?item wdt:P22 ?father . ",
    "             OPTIONAL { ?father wdt:P569 ?father_birth . } ",
    "             OPTIONAL { ?father wdt:P570 ?father_death . } ",
    "  } ",
    "  OPTIONAL { ?item wdt:P25 ?mother . ",
    "             OPTIONAL { ?mother wdt:P569 ?mother_birth . } ",
    "             OPTIONAL { ?mother wdt:P570 ?mother_death . } ",
    "  } ",
    "  SERVICE wikibase:label { bd:serviceParam wikibase:language 'de,en'. } ",
    "}"
  )
  
  Sys.sleep(BASE_SLEEP)  # be polite
  res <- safe_query(sparql)
  
  # always coerce to character (avoid type issues) and ensure one row
  res <- res %>% mutate(across(everything(), as.character))
  if (nrow(res) == 0) {
    res <- tibble::tibble(
      item = qid, itemLabel = NA_character_,
      father = NA_character_, fatherLabel = NA_character_,
      mother = NA_character_, motherLabel = NA_character_,
      item_birth_year = NA_character_, item_death_year = NA_character_,
      father_birth_year = NA_character_, father_death_year = NA_character_,
      mother_birth_year = NA_character_, mother_death_year = NA_character_
    )
  }
  
  saveRDS(res, cf)
  res
}
```

```r
# BFS over parents only: child -> (father|mother)
```

```r
ancestors_for_seed <- function(seed_qid, max_depth = MAX_DEPTH) {
  visited <- new.env(hash = TRUE, parent = emptyenv())
  labels  <- list()  # list of (qid -> best label) we encounter
  
  # record a label helper
  set_label <- function(qid, lbl) {
    if (isTRUE(nchar(lbl) > 0)) labels[[qid]] <<- lbl
  }
  
  # edge store (list of tibbles we rbind at the end)
  edge_buf <- list()
  
  # frontier = data.frame(qid, depth)
  frontier <- data.frame(qid = seed_qid, depth = 0, stringsAsFactors = FALSE)
  visited[[seed_qid]] <- TRUE
  
  total_edges <- 0L
  
  while (nrow(frontier) > 0) {
    # process one depth level together
    this_depth <- frontier$depth[1]
    if (this_depth >= max_depth) break
    
    next_frontier <- list()
    
    for (i in seq_len(nrow(frontier))) {
      child_qid <- frontier$qid[i]
      d         <- frontier$depth[i]
      
      # fetch parents + labels
      info <- wd_get_parents(child_qid)
      
      # store child label if available
      set_label(child_qid, info$itemLabel[1])
      
      # father edge
      if (!is.na(info$father[1]) && stringr::str_starts(info$father[1], "Q")) {
        father_qid <- info$father[1]
        set_label(father_qid, info$fatherLabel[1])
        
        edge_buf[[length(edge_buf) + 1]] <- tibble::tibble(
          from = child_qid,
          to   = father_qid,
          type = "has_father",
          fromLabel = info$itemLabel[1],
          toLabel   = info$fatherLabel[1]
        )
        total_edges <- total_edges + 1L
        
        if (is.null(visited[[father_qid]])) {
          visited[[father_qid]] <- TRUE
          next_frontier[[length(next_frontier) + 1]] <- data.frame(
            qid = father_qid, depth = d + 1, stringsAsFactors = FALSE
          )
        }
      }
      
      # mother edge
      if (!is.na(info$mother[1]) && stringr::str_starts(info$mother[1], "Q")) {
        mother_qid <- info$mother[1]
        set_label(mother_qid, info$motherLabel[1])
        
        edge_buf[[length(edge_buf) + 1]] <- tibble::tibble(
          from = child_qid,
          to   = mother_qid,
          type = "has_mother",
          fromLabel = info$itemLabel[1],
          toLabel   = info$motherLabel[1]
        )
        total_edges <- total_edges + 1L
        
        if (is.null(visited[[mother_qid]])) {
          visited[[mother_qid]] <- TRUE
          next_frontier[[length(next_frontier) + 1]] <- data.frame(
            qid = mother_qid, depth = d + 1, stringsAsFactors = FALSE
          )
        }
      }
    }
    
    # Combine next frontier
    if (length(next_frontier) > 0) {
      frontier <- do.call(rbind, next_frontier)
    } else {
      frontier <- frontier[0, , drop = FALSE]  # empty
    }
    
    cat(sprintf("  depth %d → next frontier: %d nodes | edges so far: %d\n",
                this_depth + 1, nrow(frontier), total_edges))
  }
  
  # Combine edges; dedupe
  edges <- if (length(edge_buf) > 0) dplyr::bind_rows(edge_buf) else
    tibble::tibble(from = character(), to = character(),
                   type = character(), fromLabel = character(), toLabel = character())
  edges <- unique(edges)
  
  # Build nodes from edges + labels map
  node_ids <- unique(c(edges$from, edges$to))
  node_labels <- vapply(node_ids, function(q) labels[[q]] %||% NA_character_, FUN.VALUE = character(1))
  
  nodes <- tibble::tibble(
    node = node_ids,
    nodeLabel = node_labels
  )
  
  list(edges = edges, nodes = nodes)
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || is.na(x)) y else x
```

```r
# Run ancestry for all seeds; accumulate and save
```

```r
all_edges <- list()
all_nodes <- list()

cat(sprintf("Running parent-only ancestry on %d seed(s)\n", length(seeds)))
```

```
## Running parent-only ancestry on 25 seed(s)
```

```r
for (si in seq_along(seeds)) {
  seed <- seeds[[si]]
  cat(sprintf("[%d/%d] seed = %s\n", si, length(seeds), seed))
  res <- tryCatch(
    { ancestors_for_seed(seed, max_depth = MAX_DEPTH) },
    error = function(e) {
      message("  !! Error on seed ", seed, ": ", conditionMessage(e))
      return(NULL)
    }
  )
  if (!is.null(res)) {
    all_edges[[length(all_edges) + 1]] <- res$edges
    # tag the seed itself for later grouping
    res$nodes$is_seed <- res$nodes$node %in% seed
    all_nodes[[length(all_nodes) + 1]] <- res$nodes
  }
}
```

```
## [1/25] seed = Q2140188
##   depth 1 → next frontier: 0 nodes | edges so far: 0
## [2/25] seed = Q78308
##   depth 1 → next frontier: 0 nodes | edges so far: 0
## [3/25] seed = Q75900
##   depth 1 → next frontier: 2 nodes | edges so far: 2
##   depth 2 → next frontier: 4 nodes | edges so far: 6
##   depth 3 → next frontier: 2 nodes | edges so far: 8
##   depth 4 → next frontier: 0 nodes | edges so far: 8
## [4/25] seed = Q289590
##   depth 1 → next frontier: 2 nodes | edges so far: 2
##   depth 2 → next frontier: 4 nodes | edges so far: 6
##   depth 3 → next frontier: 2 nodes | edges so far: 8
##   depth 4 → next frontier: 0 nodes | edges so far: 8
## [5/25] seed = Q1665415
##   depth 1 → next frontier: 0 nodes | edges so far: 0
## [6/25] seed = Q28317048
##   depth 1 → next frontier: 1 nodes | edges so far: 1
##   depth 2 → next frontier: 1 nodes | edges so far: 2
##   depth 3 → next frontier: 0 nodes | edges so far: 2
## [7/25] seed = Q18704846
##   depth 1 → next frontier: 2 nodes | edges so far: 2
##   depth 2 → next frontier: 0 nodes | edges so far: 2
## [8/25] seed = Q29640766
##   depth 1 → next frontier: 0 nodes | edges so far: 0
## [9/25] seed = Q19519918
##   depth 1 → next frontier: 0 nodes | edges so far: 0
## [10/25] seed = Q95188
##   depth 1 → next frontier: 0 nodes | edges so far: 0
## [11/25] seed = Q17574882
##   depth 1 → next frontier: 2 nodes | edges so far: 2
##   depth 2 → next frontier: 0 nodes | edges so far: 2
## [12/25] seed = Q86208
##   depth 1 → next frontier: 0 nodes | edges so far: 0
## [13/25] seed = Q97002
##   depth 1 → next frontier: 1 nodes | edges so far: 1
##   depth 2 → next frontier: 2 nodes | edges so far: 3
##   depth 3 → next frontier: 0 nodes | edges so far: 3
## [14/25] seed = Q78713
##   depth 1 → next frontier: 2 nodes | edges so far: 2
##   depth 2 → next frontier: 2 nodes | edges so far: 4
##   depth 3 → next frontier: 0 nodes | edges so far: 4
## [15/25] seed = Q97739
##   depth 1 → next frontier: 1 nodes | edges so far: 1
##   depth 2 → next frontier: 0 nodes | edges so far: 1
## [16/25] seed = Q78465
##   depth 1 → next frontier: 2 nodes | edges so far: 2
##   depth 2 → next frontier: 0 nodes | edges so far: 2
## [17/25] seed = Q71074
##   depth 1 → next frontier: 0 nodes | edges so far: 0
## [18/25] seed = Q1741352
##   depth 1 → next frontier: 1 nodes | edges so far: 1
##   depth 2 → next frontier: 0 nodes | edges so far: 1
## [19/25] seed = Q1113071
##   depth 1 → next frontier: 1 nodes | edges so far: 1
##   depth 2 → next frontier: 0 nodes | edges so far: 1
## [20/25] seed = Q1674480
##   depth 1 → next frontier: 1 nodes | edges so far: 1
##   depth 2 → next frontier: 0 nodes | edges so far: 1
## [21/25] seed = Q17310790
##   depth 1 → next frontier: 1 nodes | edges so far: 1
##   depth 2 → next frontier: 0 nodes | edges so far: 1
## [22/25] seed = Q62543
##   depth 1 → next frontier: 0 nodes | edges so far: 0
## [23/25] seed = Q1560580
##   depth 1 → next frontier: 2 nodes | edges so far: 2
##   depth 2 → next frontier: 0 nodes | edges so far: 2
## [24/25] seed = Q19976123
##   depth 1 → next frontier: 0 nodes | edges so far: 0
## [25/25] seed = Q1416810
##   depth 1 → next frontier: 0 nodes | edges so far: 0
```

```r
edges <- if (length(all_edges) > 0) dplyr::bind_rows(all_edges) else
  tibble::tibble(from = character(), to = character(),
                 type = character(), fromLabel = character(), toLabel = character())
nodes <- if (length(all_nodes) > 0) dplyr::bind_rows(all_nodes) else
  tibble::tibble(node = character(), nodeLabel = character(), is_seed = logical())

# Deduplicate
edges <- unique(edges)
nodes <- nodes %>%
  group_by(node) %>%
  summarize(
    nodeLabel = dplyr::coalesce(dplyr::first(na.omit(nodeLabel)), NA_character_),
    is_seed = any(is_seed, na.rm = TRUE),
    .groups = "drop"
  )

# Add group tag (seed vs ancestor)
nodes <- nodes %>%
  mutate(group = ifelse(is_seed | node %in% df$id_wikidata, "rich 2019", "ancestor")) %>%
  mutate(id = dplyr::row_number() - 1L,
         nodeLabel2 = paste(nodeLabel, id))

# Minimal weights
edges$value <- 1L

# Remove duplicate edges by (from, to, type)
edges <- edges %>% distinct(from, to, type, .keep_all = TRUE)

# Save quick CSVs for inspection
readr::write_csv(nodes, "data_prepared/anc_nodes.csv")
readr::write_csv(edges, "data_prepared/anc_edges.csv")

# Build igraph and save RData
graph <- igraph::graph_from_data_frame(edges, directed = TRUE, vertices = nodes)
save(graph, nodes, edges, file = "data_prepared/mm2019_ancestors_only.RData")

cat("----\n")
```

```
## ----
```

```r
cat(sprintf("Nodes: %s | Edges: %s\n", igraph::vcount(graph), igraph::ecount(graph)))
```

```
## Nodes: 41 | Edges: 32
```

```r
cat("Saved:\n  - data_prepared/mm2019_ancestors_only.RData\n  - data_prepared/anc_nodes.csv\n  - data_prepared/anc_edges.csv\n")
```

```
## Saved:
##   - data_prepared/mm2019_ancestors_only.RData
##   - data_prepared/anc_nodes.csv
##   - data_prepared/anc_edges.csv
```

```r
# ====== 3. VISUALIZATION ======
# We'll make a static hierarchy plot (PNG) and an interactive HTML.

# If you haven't already:
dir.create("graphs", showWarnings = FALSE)

# libs for plotting (install if needed)
viz_pkgs <- c("ggraph", "ggplot2", "tidygraph", "visNetwork", "htmlwidgets")
lapply(viz_pkgs[!(viz_pkgs %in% installed.packages())], install.packages)
```

```
## list()
```

```r
lapply(viz_pkgs, library, character.only = TRUE)
```

```
## 
## Attaching package: 'tidygraph'
```

```
## The following object is masked from 'package:igraph':
## 
##     groups
```

```
## The following object is masked from 'package:stats':
## 
##     filter
```

```
## [[1]]
##  [1] "ggraph"    "igraph"    "WikidataR" "lubridate" "forcats"   "stringr"   "dplyr"     "purrr"     "readr"     "tidyr"     "tibble"    "ggplot2"   "tidyverse"
## [14] "writexl"   "readxl"    "stats"     "graphics"  "grDevices" "utils"     "datasets"  "methods"   "base"     
## 
## [[2]]
##  [1] "ggraph"    "igraph"    "WikidataR" "lubridate" "forcats"   "stringr"   "dplyr"     "purrr"     "readr"     "tidyr"     "tibble"    "ggplot2"   "tidyverse"
## [14] "writexl"   "readxl"    "stats"     "graphics"  "grDevices" "utils"     "datasets"  "methods"   "base"     
## 
## [[3]]
##  [1] "tidygraph" "ggraph"    "igraph"    "WikidataR" "lubridate" "forcats"   "stringr"   "dplyr"     "purrr"     "readr"     "tidyr"     "tibble"    "ggplot2"  
## [14] "tidyverse" "writexl"   "readxl"    "stats"     "graphics"  "grDevices" "utils"     "datasets"  "methods"   "base"     
## 
## [[4]]
##  [1] "visNetwork" "tidygraph"  "ggraph"     "igraph"     "WikidataR"  "lubridate"  "forcats"    "stringr"    "dplyr"      "purrr"      "readr"      "tidyr"     
## [13] "tibble"     "ggplot2"    "tidyverse"  "writexl"    "readxl"     "stats"      "graphics"   "grDevices"  "utils"      "datasets"   "methods"    "base"      
## 
## [[5]]
##  [1] "htmlwidgets" "visNetwork"  "tidygraph"   "ggraph"      "igraph"      "WikidataR"   "lubridate"   "forcats"     "stringr"     "dplyr"       "purrr"      
## [12] "readr"       "tidyr"       "tibble"      "ggplot2"     "tidyverse"   "writexl"     "readxl"      "stats"       "graphics"    "grDevices"   "utils"      
## [23] "datasets"    "methods"     "base"
```

```r
# Load what we just created
load("data_prepared/mm2019_ancestors_only.RData")   # gives: graph, nodes, edges
```

```r
# Pick a seed by label or by Q-ID
pick_seed <- function(id_or_label) {
  if (startsWith(id_or_label, "Q")) {
    return(id_or_label)
  } else {
    # resolve label -> Q-ID (first exact match)
    idx <- match(id_or_label, V(graph)$nodeLabel)
    if (is.na(idx)) stop("No exact nodeLabel match found for: ", id_or_label)
    return(V(graph)$name[idx])
  }
}

# Build an induced subgraph of all ancestors reachable from the seed
subgraph_for_seed <- function(seed_id) {
  # vertices reachable by following child -> parent (outgoing edges)
  vids <- subcomponent(graph, v = seed_id, mode = "out")
  induced_subgraph(graph, vids = vids)
}

# Compute generation depth from seed (0 = seed, 1 = parents, 2 = grandparents, ...)
depth_from_seed <- function(g, seed_id) {
  d <- igraph::distances(g, v = seed_id, mode = "out")
  dv <- as.numeric(d[1, ])
  names(dv) <- V(g)$name
  dv
}

# Nicely color father vs. mother edges
edge_color_map <- function(etype) ifelse(etype == "has_father", "#2c7fb8", "#f768a1")
```

```r
# Examples you have in your data: "Stefan Quandt", "Wolfgang Porsche",
# or use Q-IDs directly like "Q75900"
SEED_INPUT <- "Stefan Quandt"   # <-- change this to any seed label or Q-ID

seed_qid <- pick_seed(SEED_INPUT)
cat("Chosen seed:", SEED_INPUT, "→ Q-ID:", seed_qid, "\n")
```

```
## Chosen seed: Stefan Quandt → Q-ID: Q75900
```

```r
g_sub <- subgraph_for_seed(seed_qid)
cat("Subgraph size:", vcount(g_sub), "nodes |", ecount(g_sub), "edges\n")
```

```
## Subgraph size: 9 nodes | 8 edges
```

```r
# Depth for each node from this seed
depth_vec <- depth_from_seed(g_sub, seed_qid)
V(g_sub)$generation <- depth_vec[V(g_sub)$name]          # numeric
V(g_sub)$generation[!is.finite(V(g_sub)$generation)] <- NA

# Optional: cap super-deep tails in the plot (keeps figure readable)
MAX_GEN_SHOW <- 12
keep <- which(is.na(V(g_sub)$generation) | V(g_sub)$generation <= MAX_GEN_SHOW)
g_sub <- induced_subgraph(g_sub, vids = keep)

cat("Plotted subgraph (capped at", MAX_GEN_SHOW, "generations):",
    vcount(g_sub), "nodes |", ecount(g_sub), "edges\n")
```

```
## Plotted subgraph (capped at 12 generations): 9 nodes | 8 edges
```

```r
cat("Depth range (shown):",
    min(V(g_sub)$generation, na.rm = TRUE), "to",
    max(V(g_sub)$generation, na.rm = TRUE), "\n")
```

```
## Depth range (shown): 0 to 3
```

```r
# Color father/mother via the edge type:
p <- ggraph(g_sub, layout = "sugiyama") +
  geom_edge_link(aes(edge_colour = type), alpha = 0.6) +
  geom_node_point(aes(color = group, size = ifelse(group == "rich 2019", 4, 2))) +
  geom_node_text(
    aes(label = ifelse(group == "rich 2019" | generation <= 2, nodeLabel, "")),
    repel = TRUE, size = 3
  ) +
  scale_edge_colour_manual(values = c(has_father = "#2c7fb8", has_mother = "#f768a1")) +
  scale_size_identity() +
  labs(
    title = paste0("Ancestor tree (parents only) — seed: ",
                   V(g_sub)$nodeLabel[V(g_sub)$name == seed_qid]),
    subtitle = "Edges: child → parent (blue = father, pink = mother)"
  ) +
  theme_minimal()

print(p)
```

![plot of chunk 3a) Static hierarchy plot (fixed)](figure/3a) Static hierarchy plot (fixed)-1.png)

```r
ggsave(filename = file.path("graphs", paste0("ancestors_", seed_qid, ".png")),
       plot = p, width = 10, height = 7, dpi = 300)
cat("Saved static plot → graphs/", paste0("ancestors_", seed_qid, ".png"), "\n", sep = "")
```

```
## Saved static plot → graphs/ancestors_Q75900.png
```

```r
# install.packages(c("visNetwork","htmlwidgets"))

library(visNetwork)
library(htmlwidgets)

# Ensure a folder exists for output
dir.create("graphs", showWarnings = FALSE, recursive = TRUE)

# We assume you already have:
#   - graph, nodes, edges loaded
#   - seed_qid (Q-ID of the chosen person)
#   - g_sub     (ancestor-only subgraph for that seed)
#   - V(g_sub)$generation set to depth (0 = seed)

# Build nodes dataframe for visNetwork
nodes_df <- data.frame(
  id    = V(g_sub)$name,                                   # Q-ID
  label = ifelse(is.na(V(g_sub)$nodeLabel) | V(g_sub)$nodeLabel=="",
                 V(g_sub)$name, V(g_sub)$nodeLabel),       # readable label
  group = V(g_sub)$group,                                  # "rich 2019" / "ancestor"
  level = as.integer(ifelse(is.finite(V(g_sub)$generation),
                            V(g_sub)$generation, 0)),       # hierarchy levels
  title = paste0(
    "<b>", V(g_sub)$nodeLabel, "</b>",
    "<br/>Q-ID: ", V(g_sub)$name,
    "<br/>Generation: ", V(g_sub)$generation
  ),                                                       # hover tooltip
  stringsAsFactors = FALSE
)

# Build edges dataframe (color father/mother)
e <- igraph::as_data_frame(g_sub, what = "edges")
edges_df <- data.frame(
  from   = e$from,
  to     = e$to,
  arrows = "to",
  color  = ifelse(e$type == "has_father", "#2c7fb8", "#f768a1"),
  title  = e$type,                                         # hover tooltip
  smooth = FALSE,
  stringsAsFactors = FALSE
)

# Make the widget
vis <- visNetwork(nodes_df, edges_df, width = "100%", height = "720px") %>%
  visHierarchicalLayout(direction = "UD",                   # top (parents) down
                        levelSeparation = 120,
                        nodeSpacing = 180,
                        treeSpacing = 180,
                        sortMethod = "directed") %>%
  visGroups(groupname = "rich 2019",
            color = list(background = "#00b3b3", border = "#008080")) %>%
  visGroups(groupname = "ancestor",
            color = list(background = "#f0f0f0", border = "#bdbdbd")) %>%
  visOptions(highlightNearest = TRUE,
             nodesIdSelection = list(enabled = TRUE, useLabels = TRUE)) %>%
  visLegend() %>%
  visEdges(width = 2) %>%
  visPhysics(stabilization = FALSE)

# Show in the Viewer pane
vis
```

```
## Error in loadNamespace(name): there is no package called 'webshot'
```

```r
# Save to a standalone HTML (double-click to open, or share)
seed_label <- nodes_df$label[match(seed_qid, nodes_df$id)]
out_html <- sprintf("graphs/ancestor_%s.html",
                    gsub("\\s+", "_", seed_label, perl = TRUE))
saveWidget(vis, file = out_html, selfcontained = TRUE)

# Open in your default browser (macOS)
browseURL(out_html)
```

