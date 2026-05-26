library(ggplot2)
library(dplyr)

# Personalized theme
my_boxplot_theme <- function() {
  theme_bw() %+replace%
    theme(
      panel.grid.major.y = element_line(color = "gray85", linetype = "dashed"),  # Grille claire
      panel.grid.minor = element_blank(),                                        # Pas de grille mineure
      legend.position = c(0.9, 0.85),                                            # Légende en haut à droite
      legend.background = element_blank(),                                       # Pas de fond pour la légende
      strip.background = element_rect(fill = "gray90"),                          # Fond des facettes
      plot.title = element_text(hjust = 0.5, face = "bold")                      # Titre centré (optionnel)
    )
}

# Coloc palet
my_palette <- c("Organic" = "#2ECC71", "Conventional" = "#95A5A6")

# create boxplot 

VitC_boxplot <- function(data,
                         x_var = "type_production",
                         y_var = "conc_vitaminec",
                         fill_var = "type_production",
                         facet_var = NULL,
                         x_label = NULL,
                         y_label = NULL) {
  
  # Déterminer les labels automatiquement si non fournis
  x_label <- if(is.null(x_label)) x_var else x_label
  y_label <- if(is.null(y_label)) y_var else y_label
  
  p <- ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]], fill = .data[[fill_var]])) +
    geom_boxplot(alpha = 0.8, outlier.shape = NA) +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 4, color = "black") +
    geom_jitter(width = 0.15, size = 1.5, alpha = 0.5, color = "gray30") +
    scale_fill_manual(values = my_palette) +
    my_boxplot_theme() +
    labs(x = x_label, y = y_label, fill = "Production Type")
  
  if(!is.null(facet_var)) {
    p <- p + facet_wrap(as.formula(paste("~", facet_var)))
  }
  
  p
}

# Create barplot or points

VitC_emm_plot <- function(emm_df,
                          x_var = "type_magasin",
                          y_var = "response",
                          color_var = "type_production",
                          facet_var = NULL,
                          x_label = NULL,
                          y_label = NULL,
                          fill_label = NULL,
                          plot_type = "bar",
                          dodge_width = 0.8,
                          show_values = TRUE) {
  
  # Palette de couleurs
  my_palette <- c("Organic" = "#2ECC71", 
                  "Conventional" = "#95A5A6", 
                  "Local Farms"= "#3498DB",
                  "Supermarkets"= "#F90C3C")
  
  # Labels par défaut = noms des variables
  x_label <- if (is.null(x_label)) x_var else x_label
  y_label <- if (is.null(y_label)) y_var else y_label
  fill_label <- if (is.null(fill_label)) color_var else fill_label
  
  # Graphique de base
  p <- ggplot(emm_df, aes(x = .data[[x_var]], y = .data[[y_var]],
                          fill = .data[[color_var]])) +
    my_boxplot_theme() +
    scale_fill_manual(values = my_palette) +
    theme(legend.text = element_text(size = 10),  
          legend.key.height = unit(0.5, "cm"),
          legend.position = "bottom")+
    labs(x = x_label, y = y_label, fill = fill_label)
  
  # Géométries
  if (plot_type == "bar") {
    p <- p +
      geom_bar(stat = "identity", position = position_dodge(width = dodge_width), alpha = 0.8) +
      geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                    width = 0.2, position = position_dodge(width = dodge_width))
    
    # Valeurs des moyennes (en haut à gauche des barres)
    if (show_values) {
      p <- p + geom_text(
        aes(label = round(.data[[y_var]], 1),
            y = .data[[y_var]] + 0.04 * max(.data[[y_var]]),  # Décalage vertical
            color = .data[[color_var]]),
        position = position_dodge(width = dodge_width),
        hjust = -0.1,  # Décalage horizontal à gauche
        vjust = 0,     # Alignement vertical haut
        size = 6,      # Taille augmentée
        fontface = "bold",
        show.legend = FALSE
      ) +
        scale_color_manual(values = my_palette)
    }
  }
  
  # Facettes
  if (!is.null(facet_var)) {
    p <- p + facet_wrap(as.formula(paste("~", facet_var)))
  }
  
  p
}

