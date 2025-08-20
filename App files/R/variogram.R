variogramModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("variogramNote")),
    plotOutput(ns("variogramPlot")),
    plotOutput(ns("semiVariogramPlot"))
  )
}

compute_variogram <- function (df, valcol, modelType) {
  
  fmla <- as.formula(paste(valcol, "~ 1"))
  vgm_exp <- variogram(fmla, df)
  
  initial <- vgm(psill = var(df[[valcol]], na.rm=TRUE),
                 model = modelType,
                 range = diff(bbox(df)[1,]), nugget = 0)
  
  vgm_model <- suppressWarnings(
    fit.variogram(vgm_exp, model = initial,
                  fit.sills = TRUE, fit.ranges = TRUE, fit.kappa = TRUE)
  )
  return(list(model = vgm_model, exp = vgm_exp, fmla = fmla))
}

make_variogram_plot <- function(vgm_exp, vgm_model, vgm_df, varline_df, modelType) {
  ggplot() +
    geom_point(data = vgm_df, aes(dist, gamma), shape=16, size=3) +
    geom_line(data = varline_df, aes(dist, gamma), linewidth=1) +
    labs(title = paste0("Variogram (", modelType, ")"),
         x = "Lag distance", y = "Semivariance") +
    theme_minimal() +
    theme(
      panel.border = element_rect(color="black", fill=NA, linewidth=1),
      plot.title = element_text(face="bold", size=18, hjust=0.5),
      axis.title = element_text(size=14),
      axis.text = element_text(size=12)
    ) +
    scale_x_continuous(expand = expansion(mult = c(0.01, 0.05))) +
    scale_y_continuous(expand = expansion(mult = c(0.01, 0.1)))
}

make_semi_variogram_plot <- function(vgm_exp, modelType) {
  ggplot(vgm_exp, aes(dist, gamma)) +
    geom_point(shape=16, size=3) +
    geom_line(linewidth=1) +
    labs(title = paste0("Semi-Variogram"),
         x = "Lag distance", y = "Semivariance") +
    theme_minimal() +
    theme(
      panel.border = element_rect(color="black", fill=NA, linewidth=1),
      plot.title = element_text(face="bold", size=18, hjust=0.5),
      axis.title = element_text(size=14),
      axis.text = element_text(size=12)
    ) +
    scale_x_continuous(expand=c(0,0), limits=c(0,NA)) +
    scale_y_continuous(expand=c(0,0), limits=c(0,NA))
}

