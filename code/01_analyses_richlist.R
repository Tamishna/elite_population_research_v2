## Top wealth and its historical origins: Identifying entrenched fortunes by linking rich lists over 100 years
## Authors: Daria Tisch and Emma Ischinsky
## Task: Analyses, Part I of II
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
  "sjPlot",
  "GGally",
  "flextable",
  "officer",
  "sf",
  "maps",
  "rstatix"
) 

## Install uninstalled packages
lapply(pkgs[!(pkgs %in% installed.packages())], install.packages)

## Load all packages to library
lapply(pkgs, library, character.only = TRUE)


# Clean environment
rm(list = ls())

# Load data
df = read_excel("data/mm2019.xlsx") %>%
  rename(rank1913 = rang_1913)


# Label data
labels = data.frame(var = colnames(df), 
                    label_en= c("Identifier Family", 
                                "Name in MM list",
                                "Names of Companies, MM",
                                "networth",
                                "Big clan",
                                "Year of foundation",
                                "Decade of foundation",
                                "Company sold",
                                "Wealth 1913",
                                "Rank 1913",
                                "Names 1913",
                                "Rank 2019 in MM",
                                "Nobility",
                                "Clan",
                                "Families",
                                "Rich 1913",
                                "Family foundation",
                                "Company foundation",
                                "NACE",
                                "lat",
                                "long"
                    ))

#### 1. Data analysis ####


##### 1.1 Descriptive statistics of variables in the regression model (Table 2) #####

controls = df %>% filter(sippe==0) %>%
  get_summary_stats(
    rich1913, noble, sold, familienstiftung_mm, unternehmensstiftung_mm,  # columns to calculate for
    type = "common", show = c( "mean","min", "max", "n"))    %>%
  left_join(labels %>% select(var, label_en), by = c("variable" = "var")) %>%
  mutate(Share = mean*100) %>%
  rename(N = n,
         Variable = label_en) %>%
  relocate(Variable, N, Share) %>%
  select(-variable, 
         -min,
         -max,
         -mean)
controls2 = controls %>%
  flextable() %>%
  colformat_double(1:3, j=3:3,big.mark = ",", decimal.mark = ".", na_str = "na", digits = 1 ) 

controls2

# export tables
save_as_docx("Table 2 Descriptive statistics of variables in the regression model" = controls2, path = "tables/table2.docx")

write_xlsx(controls, "tables/table2.xls")
remove(controls, controls2)



##### 1.2 Number of fortunes and combined wealth by founding date of company (Figure 1)  #####


df = df %>%
  mutate(
    foundation_cat = NA,
    foundation_cat = replace(foundation_cat, foundation_year<=1913, "-1913"),
    foundation_cat = replace(foundation_cat, foundation_year>1913 & foundation_year<=1945, "1914-1945"),
    foundation_cat = replace(foundation_cat, foundation_year>1945 & foundation_year<=1980, "1945-1980"),
    foundation_cat = replace(foundation_cat, foundation_year>1980 , "1981-2019"),
    foundation_cat = ifelse(is.na(foundation_cat), replace(foundation_cat, foundation_decade<=1910, "-1913"),foundation_cat),
    foundation_cat = ifelse(is.na(foundation_cat), replace(foundation_cat, foundation_decade>=1920 & foundation_decade<=1950, "1914-1945"),foundation_cat),
    foundation_cat = ifelse(is.na(foundation_cat), replace(foundation_cat, foundation_decade>=1950 & foundation_decade<=1970, "1945-1980"),foundation_cat),
    foundation_cat = ifelse(is.na(foundation_cat), replace(foundation_cat, foundation_decade>=1980 , "1981-2019"),foundation_cat)
  )



# plot
bar_foundation_cat= ggplot(df) + 
  geom_bar(aes(x=as.factor(foundation_cat),  y=networth, fill = as.factor(rich1913)), alpha = 0.7, position="stack", stat="identity", na.rm = TRUE)  + 
  theme_classic()  +
  geom_text(stat='count', aes(x=as.factor(foundation_cat),label=paste0('n=', ..count..),
                              hjust=-0.2), family = "serif", size = 12/.pt, show.legend  = FALSE)+
  geom_line(stat='count', aes(x=as.factor(foundation_cat), group= 1, y = ..count.., linetype = "n = Number of entries"))+
  geom_point(stat='count', aes(x=as.factor(foundation_cat), group= 1, y = ..count..),  show.legend  = FALSE)+
  scale_fill_manual(breaks=c("0","1"),
                    labels=c("No", "Yes"),
                    name = "Entrenched fortune",
                    values = c("gray45","black")) +
  scale_linetype_manual(name = "", values = c(1))+
  labs(x="", y="Wealth in billion", title = "", subtitle= "", caption= "")+
  theme_classic(base_size = 12, base_family ="serif") +
  theme(axis.line.x = element_line(color = "white", size=1, lineend = 'round'),
        axis.line.y = element_line(color = "black", size=0.5, lineend = 'round'),
        strip.background = element_blank(),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        legend.position = "bottom")


bar_foundation_cat

# export graph
ggsave("graphs/figure_01.png", plot = bar_foundation_cat, 
       width = 6, height = 6,  units = "in")
cairo_ps(filename = "graphs/figure_01.eps",
         width = 6, height = 4, pointsize = 12)
bar_foundation_cat
dev.off()  

remove(bar_foundation_cat)


##### 1.3 Entrenched fortunes (Table 3) #####

entrenched = df %>%
  filter(!is.na(wealth_1913)) %>%
  select(company, full_name, rank2019, networth, name_1913, wealth_1913) %>% 
  slice(1:10)

remove(entrenched)

##### 1.4 Characteristics of the wealth elite (Table 4) #####

char_elite = df %>%
  group_by(rich1913) %>%
  summarise_at(vars(noble, familienstiftung_mm, unternehmensstiftung_mm, #rank2019, 
                    sold), funs(mean(., na.rm=TRUE))) %>%
  pivot_longer(!rich1913) %>%
  pivot_wider(names_from = rich1913, values_from = value) %>%
  left_join(labels %>% select(var, label_en), by = c("name" = "var"))   %>%
  rename("Entrenched fortunes" = "1",
         "Other fortunes" = "0",
          "Variable" = label_en) %>%  
  select(-name) %>%
  relocate(Variable, "Entrenched fortunes") %>%
  arrange(-`Entrenched fortunes`) %>%
  mutate(`Entrenched fortunes` = `Entrenched fortunes`*100,
         `Other fortunes` = `Other fortunes`*100) 

char_elite2 =   char_elite %>%  flextable() %>%
  colformat_double(1:4, j=2:3,big.mark = ",", decimal.mark = ".", na_str = "na", digits = 1 ) 

char_elite2

# export tables

save_as_docx("Table 4 Characteristics of the wealth elite" = char_elite2, path = "tables/table4.docx")

write_xlsx(char_elite, "tables/table4.xls")
remove(char_elite, char_elite2)

##### 1.5 Share of entrenched fortunes #####

# Top 1032: 7,95%
df %>%
  group_by(rich1913) %>% 
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))


# Top 1000 without clan list: 6,49%

df %>%
  filter(id_fam < 1002) %>%
  group_by(rich1913) %>% 
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))


# Top 500 without clan list: 7,8%

df %>%
  filter(id_fam < 501) %>%
  group_by(rich1913) %>% 
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))



# Top 500: 10,5%

df %>%
  filter(networth > 0.35) %>%
  group_by(rich1913) %>% 
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))


##### 1.6 Geographical variation of today’s largest fortunes in Germany (Figure 2) #####

# Read in the shapefiles
shp <- read_sf('graphs/bland_shape/B-2020-AI001-2-5--AI0109--2022-01-27.shp')
shape = read_sf("graphs/shape_bevoelkerungsdichte/B-2019-AI002-1-5--AI0201--2022-01-31.shp")

# Convert df to an sf object 
df_sf = st_as_sf(df%>% filter(!is.na(long)), coords = c("lat", "long"))

# define crs
st_crs(df_sf) = 4326

# transform coordinates
df_sf = st_transform(df_sf ,3035)

# Separate maps for entrechend and other fortunes
dynastic_names <- c(
  `-1` = paste0("Entrenched fortunes", "\n", "(ancestors in rich list 1913)"),
  `0` =  paste0("Other fortunes", "\n", "(no ancestors in rich list 1913)")
)

# generate plot
geo_rich = ggplot(shape) + geom_sf() +
  geom_sf(data = df_sf %>% filter(!is.na(rich1913)) ,  
          shape = 21, alpha =0.7, 
          aes(fill = as.factor(noble),
              size= networth)) +
  facet_wrap(~-rich1913, labeller = as_labeller(dynastic_names))+
  scale_fill_manual(name = "Nobility",  
                    values=c("darkred", "cyan"),
                    na.translate = FALSE,
                    labels=c("No", "Yes")) +
  scale_size_continuous(name = "Wealth (in billion)", 
                        breaks = c(0.5, 1, 5, 15)) +
  theme_classic(base_size = 12, base_family ="serif") +
  theme(text = element_text(family ="serif", size=12),
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size=12),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        legend.position = "bottom",
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")) +
  guides(fill = guide_legend(title.position="top",nrow = 2, byrow = TRUE),
         size = guide_legend(title.position="top",nrow = 2, byrow = TRUE))

geo_rich

# save plot
ggsave("graphs/figure_02.jpeg", plot = geo_rich,  dpi = 300, 
       width = 5.5, height = 5, units = "in", 
       type = "cairo", bg ="white")


cairo_ps(filename = "graphs/figure_02.eps",
         width = 5.5, height = 5, pointsize = 12)
geo_rich
dev.off()  

remove(shape, shp, geo_rich, df_sf)

##### 1.7 Regression results (Figure 3) #####

# Estimate regression models

lm_wealth_rank = lm(rank2019 ~ rich1913+noble+sold+ familienstiftung_mm +unternehmensstiftung_mm , data = df) 
lm_wealth_log = lm(log(networth) ~ rich1913+noble+sold+ familienstiftung_mm +unternehmensstiftung_mm , 
                     data = df %>% filter(!is.na(rank2019))) 
tab_model(lm_wealth_rank, lm_wealth_log, show.ci = FALSE)

# Coefficient plot
coefplot = ggcoef_model(lm_wealth_rank, signif_stars = FALSE, show_p_values = FALSE,
                        #categorical_terms_pattern = "{level} (ref: {reference_level})",
                        significance = NULL, colour = NULL,
                        variable_labels = c(
                          rich1913 = "Entrenched fortune",
                          noble = "Noble",
                          familienstiftung_mm = "Family foundation",
                          unternehmensstiftung_mm = "Company foundation",
                          sold = "Company sold"
                        ), facet_row = NULL)+
  xlab("Coefficients (in ranks)") +
  labs(title = "", subtitle= "", caption= "")+
  theme(text = element_text(family ="serif", size=12, face = "plain"),
        axis.text=element_text(size=12,family ="serif", face = "plain"),
        axis.title=element_text(size=12, family ="serif",face = "plain"),
        strip.text.y = element_text(size=12,family ="serif", face = "plain")
  )


coefplot
ggsave("graphs/figure_03.jpeg", plot = coefplot,width = 6, height = 3.5, units = "in")
cairo_ps(filename = "graphs/figure_03.eps",
         width = 10, height = 7, pointsize = 16)
coefplot
dev.off()

##### 1.8 Coefficient plot for logged net worth (Appendix 2) #####

coefplot2 = ggcoef_model(lm_wealth_log, signif_stars = FALSE, show_p_values = FALSE,
                         significance = NULL, colour = NULL,
                         variable_labels = c(
                           rich1913 = "Rich in 1913",
                           noble = "Noble",
                           familienstiftung_mm = "Family foundation",
                           unternehmensstiftung_mm = "Company foundation",
                           sold = "Company sold"
                         ), facet_row = NULL)+
  xlab("Coefficients") +
  labs(title = "", subtitle= "", caption= "")+
  theme(text = element_text(family ="serif", size=12, face = "plain"),
        axis.text=element_text(size=12,family ="serif", face = "plain"),
        axis.title=element_text(size=12, family ="serif",face = "plain"),
        strip.text.y = element_text(size=12,family ="serif", face = "plain")
  )


coefplot2
ggsave("graphs/appendix_02.jpeg", plot = coefplot2,width = 6, height = 3.5, units = "in")
cairo_ps(filename = "graphs/appendix_02.eps",
         width = 10, height = 7, pointsize = 16)
coefplot2
dev.off()

# The end