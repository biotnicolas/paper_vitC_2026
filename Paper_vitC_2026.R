########################################### Paper Vitamin C ###############################################

# Auteur: Nicolas Biot
# Paper title: ""
# Inspired from Oriane Dallemagne master thesis (2026, UCLouvain)

# Packages

require(dplyr)
require(ggplot2)
require(lme4)
require(lmerTest)
require(emmeans)
require(tidyverse)    
require(readxl)
require(broom.mixed)
require(openxlsx)
require(patchwork)

# Themes

source("my_boxplot_theme.R")
source("clean_anova_table.R")
#for using clean_anova_table here I specify already the name of my factors:
rename_map = c(
  "type_magasin" = "Supply chain type",
  "type_production" = "Production type",
  "semaine" = "Sampling week"
)

#####################################################################################
#--------- 1. Data Download & characterization -------------------------------------#
#####################################################################################

## Data download ##

data_total <- read_excel("Data_set.xlsx",
                         sheet = "Data_set_vitC_2026",
                         trim_ws = TRUE)

## Data characterization ##

#Types of variables
data_total$pourcentage_MS <- as.numeric(gsub(",",".", data_total$pourcentage_MS))
data_total$conc_vitaminec <- as.numeric(gsub(",",".", data_total$conc_vitaminec))
data_total$lieu <- as.factor(data_total$lieu)
data_total$type_production <- as.factor(data_total$type_production)
data_total$type_magasin <- as.factor(data_total$type_magasin)
data_total$semaine <- as.factor(data_total$semaine)


#Delete some values / NA
data_mod <- data_total %>%
  filter(dtt != "sansdtt_congele") %>%
  filter(!(lieu == "2" & variete == "coeur_de_boeuf") | is.na(lieu) | is.na(variete))

####################################################################################
#--------- 2. Analysis ------------------------------------------------------------#
####################################################################################

# Random effects: "lieu"
# Fixed effects: "semaine", "type production", "type de magasin"

###################################################################################
### --------- 2.1. SALADS ------------------------------------------------------###
###################################################################################

###############

                ##################################################
                ## 2.1.1 MOD.1 = Total vit C (AA + DHA) ; SALAD ##
                ##################################################

###############

## Data visualisation ##

#Filtering Data 
salade_dtt <- data_mod %>%
  filter(dtt == "avecdtt") %>%
  filter(legume=="salade")

#Changing names in english
salade_dtt <- salade_dtt %>%
  mutate(
    type_production = recode(type_production, "bio" = "Organic", "conv" = "Conventional"),
    type_magasin = recode(type_magasin, "maraich" = "Local Farms", "superm" = "Supermarkets")
  )

#Data visualisation
salad_vitC_tot <- VitC_boxplot(salade_dtt,
             x_var = "type_production",
             y_var = "conc_vitaminec",
             facet_var = "type_magasin",
             y_label = "Total Vitamin C concentration in Salads [mg/100g]",
             x_label = ""
             )
salad_vitC_tot

#Saving graph
#ggsave("salad_vitC_tot_boxplot.svg", width = 8, height = 6, dpi = 300)

## Statistical analysis ## 

#Mixed model (log transformation)
salade_dtt$log_conc_vit <- log(salade_dtt$conc_vitaminec)
mod_saladtt <- lmer(log_conc_vit ~  type_magasin * type_production + semaine + (1|lieu), data=salade_dtt)

#Fixed effect
anova(mod_saladtt, ddf = "Kenward-Roger") 

#Random effect
ranova(mod_saladtt)

#Saving tab
VitC_tot_ANOVA_salad_table <- anova(mod_saladtt, ddf = "Kenward-Roger")
VitC_tot_ANOVA_salad_table <- clean_anova_table(VitC_tot_ANOVA_salad_table, rename_map = rename_map) #clean ANOVA table for scientific papers
write.xlsx(VitC_tot_ANOVA_salad_table, "VitC_tot_ANOVA_salad_table.xlsx", rowNames = TRUE)

#Mean estimates 
emm <- emmeans(mod_saladtt, ~ type_production+type_magasin, type = "response", adjust="holm", tran="log")
emm
#All Mean comparison
pairs((emm))
# pairs(regrid(emm)) # regrid for having the difference BUT less good than the other one if in log

#Saving tab
emm_clean <- clean_contrast_table(as.data.frame(pairs(emm)))
write.xlsx(emm_clean, "salad_vitC_tot_comparison.xlsx", rowNames = FALSE)

#Visualization 
emm_df <- as.data.frame(emm)

salad_vitC_tot_stats <- VitC_emm_plot(emm_df,
              x_var = "type_production",  
              facet_var = "type_magasin",  
              y_label = "Total Vitamin C concentration in Salads [mg/100g]",
              x_label = "",
              fill_label = "Production type")
salad_vitC_tot_stats
#Saving graph
#ggsave("salad_vitC_tot_stats.svg", width = 9, height = 5, dpi = 300)

#Mean comparison by Production types or Supply chain type
  #By production type
emm_prod_salad_tot <- emmeans(mod_saladtt, ~ type_production, type = "response", adjust = "holm", tran="log") #att. must specify the log transform via tran
pairs(emm_prod_salad_tot)
#pairs(regrid(emm_prod_salad_tot)) # regrid for having the difference BUT less good than the other one if in log

#save tab for comparison 
prod_pairs <- as.data.frame(summary(pairs(emm_prod_salad_tot), adjust = "holm"))

#graph
emm_prod_salad_tot <- as.data.frame(emm_prod_salad_tot)
VitC_tot_plot_prod <- VitC_emm_plot(emm_prod_salad_tot,
              x_var = "type_production",  
              y_label = "Total Vitamin C concentration in Salads [mg/100g]",
              x_label = "",
              fill_label = "Production type")
VitC_tot_plot_prod

  #By supply chain type
emm_sc_salad_tot <- emmeans(mod_saladtt, ~ type_magasin, type = "response", adjust = "holm", tran="log")
pairs(emm_sc_salad_tot)
#pairs(regrid(emm_sc_salad_tot)) # regrid for having the difference BUT less good than the other one if in log
#Save tab of comparisons
sc_pairs   <- as.data.frame(summary(pairs(emm_sc_salad_tot), adjust = "holm"))

 #By location
emm_loc_salad_tot <- emmeans(mod_saladtt, ~ semaine, type = "response", adjust = "holm", tran="log")
pairs(emm_loc_salad_tot)
#pairs(regrid(emm_loc_salad_tot)) # regrid for having the difference BUT less good than the other one if in log
#Save tab of comparisons
loc_pairs   <- as.data.frame(summary(pairs(emm_loc_salad_tot), adjust = "holm"))



#compile tabs
VitC_tot_salad_table <- rbind(prod_pairs, sc_pairs, loc_pairs)
VitC_tot_salad_table <- clean_contrast_table(VitC_tot_salad_table)
write.xlsx(VitC_tot_salad_table, "salad_vitC_tot.xlsx", rowNames = FALSE)

#graph
emm_sc_salad_tot <- as.data.frame(emm_sc_salad_tot)
VitC_tot_sc_prod <- VitC_emm_plot(emm_sc_salad_tot,
                                    x_var = "type_magasin",  
                                    y_label = "Total Vitamin C content in Salads [mg/100g]",
                                    x_label = "",
                                    color_var = "type_magasin",
                                  fill_label = "Supply Chain Type")
#VitC_tot_sc_prod

#graph
Plot_vitC_tot_salad <- VitC_tot_plot_prod + VitC_tot_sc_prod +
  plot_annotation(tag_levels = 'a',
                  tag_prefix = '(', 
                  tag_suffix = ')',
                  tag_sep = '')  
Plot_vitC_tot_salad


#Save graph
#ggsave("Plot_vitC_tot_salad.svg", width = 9, height = 5, dpi = 300)

###############
                    
                    ######################################
                    ## 2.1.2 MOD.2 = vit C (AA) ; SALAD ##
                    ######################################

###############

## Filtering Data ##

salade_sdtt <- data_mod %>%
  filter(dtt == "sansdtt") %>%
  filter(legume=="salade")

#Changing names in english
salade_sdtt <- salade_sdtt %>%
  mutate(
    type_production = recode(type_production, "bio" = "Organic", "conv" = "Conventional"),
    type_magasin = recode(type_magasin, "maraich" = "Local Farms", "superm" = "Supermarkets")
  )
#Data visualisation
salad_vitC_AA_boxplot <- VitC_boxplot(salade_sdtt,
                                      x_var = "type_production",
                                      y_var = "conc_vitaminec",
                                      facet_var = "type_magasin",
                                      y_label = "AA concentration in Salads [mg/100g]",
                                      x_label = ""
)
salad_vitC_AA_boxplot

#Saving graph
#ggsave("salad_vitC_AA_boxplot.svg", width = 8, height = 6, dpi = 300)

## Statistical analysis ## 

#Mixed model (log transformation)
salade_sdtt$log_conc_vit <- log(salade_sdtt$conc_vitaminec)
mod_saladsdtt <- lmer(log_conc_vit ~  type_magasin * type_production + semaine + (1|lieu), data=salade_sdtt)
#Fixed effect
anova(mod_saladsdtt, ddf = "Kenward-Roger") 
#Random effect
ranova(mod_saladsdtt)

#no significant differences except by location

#By location
emm_loc_salad_AA <- emmeans(mod_saladsdtt, ~ semaine, type = "response", adjust = "holm", tran="log")
pairs(emm_loc_salad_AA)
#pairs(regrid(emm_loc_salad_AA)) # regrid for having the difference BUT less good than the other one if in log


#Saving tab of the anova table
VitC_AA_salad_table <- anova(mod_saladsdtt, ddf = "Kenward-Roger")
VitC_AA_ANOVA_salad_table <- clean_anova_table(VitC_AA_salad_table, rename_map = rename_map) #clean ANOVA table for scientific papers

write.xlsx(VitC_AA_ANOVA_salad_table, "VitC_AA_salad_table.xlsx", rowNames = TRUE)



###################################################################################
### --------- 2.2. TOMATOES ----------------------------------------------------###
###################################################################################

###############

                  ###################################################
                  ## 2.2.1 MOD.3 = Total Vit C (AA+DHA) ; TOMATOES ##
                  ###################################################

###############

## Filtering Data ##

tomate_dtt <- data_mod %>%
  filter(dtt == "avecdtt") %>%
  filter(legume=="tomate")

tomate_dtt <- tomate_dtt %>%
  mutate(
    type_production = recode(type_production, "bio" = "Organic", "conv" = "Conventional"),
    type_magasin = recode(type_magasin, "maraich" = "Local Farms", "superm" = "Supermarkets")
  )

## Data visualisation ##

tomato_vitC_tot_boxplot <- VitC_boxplot(tomate_dtt,
                                      x_var = "type_production",
                                      y_var = "conc_vitaminec",
                                      facet_var = "type_magasin",
                                      y_label = "Total Vitamin C concentration in Tomatoes [mg/100g]",
                                      x_label = ""
)
tomato_vitC_tot_boxplot
#Saving graph
#ggsave("tomato_vitC_tot_boxplot.svg", width = 8, height = 6, dpi = 300)

## Statistical analysis ## 

#Mixed model (log transformation)
tomate_dtt$log_conc_toma_dtt <- log(tomate_dtt$conc_vitaminec)
mod_tomdtt <- lmer(log_conc_toma_dtt ~  type_magasin * type_production + semaine + (1|lieu), data=tomate_dtt)

#Fixed effect
VitC_tot_tomato_table <- anova(mod_tomdtt, ddf = "Kenward-Roger")
VitC_tot_tomato_table
#Random effect
ranova(mod_tomdtt)

#Mean estimates 
emm_t <- emmeans(mod_tomdtt, ~ type_production+type_magasin, type = "response", adjust="holm", tran="log")
emm_t
#All Mean comparison
pairs((emm_t))
# pairs(regrid(emm_t)) # regrid for having the difference BUT less good than the other one if in log

#Saving tab
#Anova table
VitC_tot_ANOVA_tomato_table <- clean_anova_table(VitC_tot_tomato_table, rename_map = rename_map) #clean ANOVA table for scientific papers
write.xlsx(VitC_tot_ANOVA_tomato_table, "VitC_tot_tomato_table.xlsx", rowNames = TRUE)
#Contrast table
#Saving tab
emm_clean_t <- clean_contrast_table(as.data.frame(pairs(emm_t)))
write.xlsx(emm_clean_t, "tomato_vitC_tot_main.xlsx", rowNames = FALSE)

#Visualization 
emm_df_t <- as.data.frame(emm_t)

tomato_vitC_tot_stats <- VitC_emm_plot(emm_df_t,
                                      x_var = "type_production",  
                                      facet_var = "type_magasin",  
                                      y_label = "Total Vitamin C concentration in Tomatoes [mg/100g]",
                                      x_label = "",
                                      fill_label = "Production type")
tomato_vitC_tot_stats
#Saving graph
#ggsave("tomato_vitC_tot_stats.svg", width = 9, height = 5, dpi = 300)

#Mean comparison by Production types, Supply chain type and week
#By production type
emm_prod_tomato_tot <- emmeans(mod_tomdtt, ~ type_production, type = "response", adjust = "holm", tran="log")
p_t_pairs <- pairs(emm_prod_tomato_tot)
p_t_pairs
#pairs(regrid(emm_prod_tomato_tot)) # regrid for having the difference BUT less good than the other one if in log

#save comparison for tab
prod_pairs_t <- as.data.frame(summary(p_t_pairs, adjust = "holm"))

#graph
emm_prod_tomato_tot <- as.data.frame(emm_prod_tomato_tot)
VitC_tot_plot_prod_t <- VitC_emm_plot(emm_prod_tomato_tot,
                                    x_var = "type_production",  
                                    y_label = "Total Vitamin C concentration in Tomatoes [mg/100g]",
                                    x_label = "",
                                    fill_label = "Production type")
VitC_tot_plot_prod_t

#By supply chain type
emm_sc_tomato_tot <- emmeans(mod_tomdtt, ~ type_magasin, type = "response", adjust = "holm", tran="log")
sc_t_pairs <- pairs(emm_sc_tomato_tot)
sc_t_pairs
#pairs(regrid(emm_sc_tomato_tot)) # regrid for having the difference BUT less good than the other one if in log
#save tab for comparison
sc_pairs_t   <- as.data.frame(summary(sc_t_pairs, adjust = "holm"))
#By week
emm_week_tomato_tot <- emmeans(mod_tomdtt, ~ semaine, type = "response", adjust = "holm", tran="log")
sc_week_pairs <- pairs(emm_week_tomato_tot)
sc_week_pairs
#save tab for comparison
sc_week_pairs_t   <- as.data.frame(summary(sc_week_pairs, adjust = "holm"))

#compil tab for comparion (production and supply chain)
VitC_tot_tomato_table <- rbind(prod_pairs_t, sc_pairs_t,sc_week_pairs_t)
VitC_tot_tomato_table <- clean_contrast_table(VitC_tot_tomato_table)
write.xlsx(VitC_tot_tomato_table, "VitC_tot_tomato_table.xlsx", rowNames = FALSE)

#graph sc
emm_sc_tomato_tot <- as.data.frame(emm_sc_tomato_tot)
VitC_tot_sc_t <- VitC_emm_plot(emm_sc_tomato_tot,
                                  x_var = "type_magasin",  
                                  y_label = "Total Vitamin C content in Tomatoes [mg/100g]",
                                  x_label = "",
                                  color_var = "type_magasin",
                                  fill_label = "Supply Chain Type")
VitC_tot_sc_t

#Main effect graph
Plot_vitC_tot_tomato <- VitC_tot_plot_prod_t + VitC_tot_sc_t +
  plot_annotation(tag_levels = 'a',
                  tag_prefix = '(', 
                  tag_suffix = ')',
                  tag_sep = '')  
Plot_vitC_tot_tomato

#Save graph
#ggsave("Plot_vitC_tot_tomato.svg", width = 9, height = 5, dpi = 300)

###############

                  #########################################
                  ## 2.2.2 MOD.4 = vit C (AA) ; TOMATOES ##
                  #########################################

###############

## Filtering Data ##

tomate_sdtt <- data_mod %>%
  filter(dtt == "sansdtt") %>%
  filter(legume=="tomate")

tomate_sdtt <- tomate_sdtt %>%
  mutate(
    type_production = recode(type_production, "bio" = "Organic", "conv" = "Conventional"),
    type_magasin = recode(type_magasin, "maraich" = "Local Farms", "superm" = "Supermarkets")
  )

## Data visualisation ##

tomato_vitC_AA_boxplot <- VitC_boxplot(tomate_sdtt,
                                        x_var = "type_production",
                                        y_var = "conc_vitaminec",
                                        facet_var = "type_magasin",
                                        y_label = "AA concentration in Tomatoes [mg/100g]",
                                        x_label = ""
)
tomato_vitC_AA_boxplot

## Statistical analysis ## 

#Mixed model (log transformation)
tomate_sdtt$log_conc_toma_sdtt <- log(tomate_sdtt$conc_vitaminec)
mod_tomsdtt <- lmer(log_conc_toma_sdtt ~  type_magasin * type_production + semaine + (1|lieu), data=tomate_sdtt)

#Fixed effect
AA_Anova_tomato <- anova(mod_tomsdtt, ddf = "Kenward-Roger") 
AA_Anova_tomato

#Random effect
ranova(mod_tomsdtt)

#Mean estimates 
emm_ts <- emmeans(mod_tomsdtt, ~ type_production+type_magasin, type = "response", adjust="holm", tran="log")
emm_ts
#All Mean comparison
main_pairs <- pairs((emm_ts))
# pairs(regrid(emm_t)) # regrid for having the difference BUT less good than the other one if in log

#Saving tab
#Anova table
AA_Anova_tomato_table <- clean_anova_table(AA_Anova_tomato, rename_map = rename_map) #clean ANOVA table for scientific papers
write.xlsx(AA_Anova_tomato_table, "AA_anova_tomato_table.xlsx", rowNames = TRUE)
#Contrast table
#Saving tab
emm_clean_AA_t <- clean_contrast_table(as.data.frame(main_pairs))
write.xlsx(emm_clean_AA_t, "tomato_AA_tot_main.xlsx", rowNames = FALSE)

#Visualization 
emm_df_t <- as.data.frame(emm_ts)

Plot_AA_tomato <- VitC_emm_plot(emm_df_t,
                                       x_var = "type_production",
                                       facet_var = "type_magasin",  
                                       y_label = "AA concentration in Tomatoes [mg/100g]",
                                       x_label = "",
                                       fill_label = "Production type")
Plot_AA_tomato

#Save plot
#ggsave("Plot_AA_tomato_2.svg", width = 9, height = 5, dpi = 300)

#Mean comparison by Production types, Supply chain type and week
#By production type
emm_prod_tomato_AA <- emmeans(mod_tomsdtt, ~ type_production, type = "response", adjust = "holm", tran="log")
p_AA_pairs <- pairs(emm_prod_tomato_AA)
p_AA_pairs
#pairs(regrid(emm_prod_tomato_AA)) # regrid for having the difference BUT less good than the other one if in log
#save comparison for tab
prod_pairs_AA <- as.data.frame(summary(p_AA_pairs, adjust = "holm"))

#graph
emm_prod_tomato_AA <- as.data.frame(emm_prod_tomato_AA)
tomato_plot_prod_AA <- VitC_emm_plot(emm_prod_tomato_AA,
                                      x_var = "type_production",  
                                      y_label = "AA concentration in Tomatoes [mg/100g]",
                                      x_label = "",
                                      fill_label = "Production type")
tomato_plot_prod_AA

#By supply chain type
emm_sc_tomato_AA <- emmeans(mod_tomsdtt, ~ type_magasin, type = "response", adjust = "holm", tran="log")
sc_AA_pairs <- pairs(emm_sc_tomato_AA)
sc_AA_pairs
#pairs(regrid(emm_sc_tomato_tot)) # regrid for having the difference BUT less good than the other one if in log
#save tab for comparison
sc_pairs_AA   <- as.data.frame(summary(sc_AA_pairs, adjust = "holm"))

#graph sc
emm_sc_tomato_AA <- as.data.frame(emm_sc_tomato_AA)
tomato_sc_AA_plot <- VitC_emm_plot(emm_sc_tomato_AA,
                               x_var = "type_magasin",  
                               y_label = "Total Vitamin C content in Tomatoes [mg/100g]",
                               x_label = "",
                               color_var = "type_magasin",
                               fill_label = "Supply Chain Type")
tomato_sc_AA_plot
#By week
emm_week_tomato_AA <- emmeans(mod_tomsdtt, ~ semaine, type = "response", adjust = "holm", tran="log")
sc_week_pairs_AA <- pairs(emm_week_tomato_AA)
sc_week_pairs_AA
#save tab for comparison
sc_week_pairs_AA   <- as.data.frame(summary(sc_week_pairs_AA, adjust = "holm"))

#compil tab for comparion (production and supply chain)
AA_tomato_table <- rbind(prod_pairs_AA, sc_pairs_AA,sc_week_pairs_AA)
AA_tomato_table <- clean_contrast_table(AA_tomato_table)
write.xlsx(AA_tomato_table, "AA_tomato_table.xlsx", rowNames = FALSE)

#Main effect graph
Plot_AA_tomato_main <- tomato_plot_prod_AA + tomato_sc_AA_plot +
  plot_annotation(tag_levels = 'a',
                  tag_prefix = '(', 
                  tag_suffix = ')',
                  tag_sep = '')  
Plot_AA_tomato_main

#Save graph
#ggsave("Plot_AA_tomato_main.svg", width = 9, height = 5, dpi = 300)

                      #########################################
                      ## SUPPL. GRAPHS - Descriptive data    ##
                      #########################################

# Salad descriptive
descr_plot_salad <- (salad_vitC_tot / salad_vitC_AA_boxplot) +
  plot_annotation(tag_levels = 'a',
                  tag_prefix = '(', 
                  tag_suffix = ')',
                  tag_sep = '') 
descr_plot_salad
#Saving graphs
ggsave("descr_plot_salad.svg", width = 8, height = 10, dpi = 300)

# Salad stats tot vit C
salad_stats <- salad_vitC_tot_stats / Plot_vitC_tot_salad + 
  plot_annotation(title = "") & 
  theme( axis.title = element_blank(), 
         legend.position = "none") 
salad_stats
#ggsave("salad_stats.svg", width = 8, height = 7, dpi = 300)

# Tomato descriptive
descr_plot_tomato <- (tomato_vitC_tot_boxplot / tomato_vitC_AA_boxplot)+
  plot_annotation(tag_levels = 'a',
                  tag_prefix = '(', 
                  tag_suffix = ')',
                  tag_sep = '') 
descr_plot_tomato

# tomato stats tot vit C
tomato_stats <- tomato_vitC_tot_stats/Plot_vitC_tot_tomato
tomato_stats

# tomato stats tot AA
tomato_AA_stats <- Plot_AA_tomato/Plot_AA_tomato_main
tomato_AA_stats

#Saving graphs
#ggsave("descr_plot_tomato.svg", width = 8, height = 10, dpi = 300)

### Supplementary analysis 

#ratio DHA/AA+DHA
#for salad
salad_ratio <- data_mod %>%
  filter(legume=="salade")

df_ratio <- salad_ratio %>%
  select(lieu, semaine, type_production,type_magasin, Pool, dtt, conc_vitaminec) %>%
  pivot_wider(names_from = dtt, values_from = conc_vitaminec) %>%
  mutate(ratio = (avecdtt-sansdtt) / avecdtt)

df_ratio <- df_ratio %>%
  mutate(
    type_production = recode(type_production, "bio" = "Organic", "conv" = "Conventional"),
    type_magasin = recode(type_magasin, "maraich" = "Local Farms", "superm" = "Supermarkets")
  )


salad_ratio <- VitC_boxplot(df_ratio,
                                       x_var = "type_production",
                                       y_var = "ratio",
                                       facet_var = "type_magasin",
                                       y_label = "ratio (DHA/(AA+DHA)) [mg/100g]",
                                       x_label = ""
)
salad_ratio
#for tomato
tomato_ratio <- data_mod %>%
  filter(legume=="tomate")

df_ratio <- tomato_ratio %>%
  select(lieu, semaine, type_production,type_magasin, Pool, dtt, conc_vitaminec) %>%
  pivot_wider(names_from = dtt, values_from = conc_vitaminec) %>%
  mutate(ratio = (avecdtt-sansdtt) / avecdtt)

df_ratio <- df_ratio %>%
  mutate(
    type_production = recode(type_production, "bio" = "Organic", "conv" = "Conventional"),
    type_magasin = recode(type_magasin, "maraich" = "Local Farms", "superm" = "Supermarkets")
  )


tomato_ratio <- VitC_boxplot(df_ratio,
                            x_var = "type_production",
                            y_var = "ratio",
                            facet_var = "type_magasin",
                            y_label = "ratio (DHA/(AA+DHA)) [mg/100g]",
                            x_label = ""
)
tomato_ratio
