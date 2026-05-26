### Function for Anova-Like table in scientific publication

clean_anova_table <- function(tab, rename_map = NULL) {
  
  # ---- Significativité
  tab$Signif <- symnum(
    tab$`Pr(>F)`,
    cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
    symbols = c("***", "**", "*", ".", " ")
  )
  
  # ---- Format nombres
  num_cols <- c("Sum Sq", "Mean Sq", "F value")
  
  tab[num_cols] <- lapply(tab[num_cols], function(x) {
    x <- as.numeric(as.character(x))
    
    ifelse(x < 0.01,
           format(x, scientific = TRUE, digits = 2),
           sprintf("%.2f", x))
  })
  
  # ---- p-values
  tab$`Pr(>F)` <- as.numeric(as.character(tab$`Pr(>F)`))
  tab$`Pr(>F)` <- ifelse(tab$`Pr(>F)` < 0.001,
                         "<0.001",
                         sprintf("%.3f", tab$`Pr(>F)`))
  
  # ---- Renommage flexible (y compris interactions)
  if(!is.null(rename_map)){
    for(i in seq_along(rename_map)){
      rownames(tab) <- gsub(names(rename_map)[i],
                            rename_map[i],
                            rownames(tab))
    }
  }
  
  return(tab)
}


clean_contrast_table <- function(tab, rename_map = NULL) {
  
  # ---- Significativité
  tab$Signif <- symnum(
    tab$p.value,
    cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
    symbols = c("***", "**", "*", ".", " ")
  )
  
  # ---- Colonnes numériques à formater
  num_cols <- c("ratio", "SE", "t.ratio")
  
  tab[num_cols] <- lapply(tab[num_cols], function(x) {
    x <- as.numeric(as.character(x))
    
    ifelse(abs(x) < 0.01,
           format(x, scientific = TRUE, digits = 2),
           sprintf("%.2f", x))
  })
  
  # ---- p-values
  tab$p.value <- as.numeric(as.character(tab$p.value))
  tab$p.value <- ifelse(tab$p.value < 0.001,
                        "<0.001",
                        sprintf("%.3f", tab$p.value))
  
  # ---- Renommage flexible du contraste
  if(!is.null(rename_map)){
    for(i in seq_along(rename_map)){
      tab$contrast <- gsub(names(rename_map)[i],
                           rename_map[i],
                           tab$contrast)
    }
  }
  
  return(tab)
}
