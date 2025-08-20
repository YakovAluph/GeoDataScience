make_voronoi_plot <- function(df, xcol, ycol, valcol) {
  validate(
    need(nrow(df) >= 3, "Need at least 3 points to compute Voronoi diagram.")
  )
  
  df_points <- as.data.frame(df)
  vor <- deldir(df_points[[xcol]], df_points[[ycol]])
  tiles <- tile.list(vor)
  
  poly_list <- lapply(seq_along(tiles), function(i) {
    tile <- tiles[[i]]
    data.frame(x = tile$x, y = tile$y, id = i)
  })
  
  vor_df <- do.call(rbind, poly_list)
  vor_df <- vor_df %>%
    left_join(df_points %>% mutate(id = row_number()), by = "id")
  
  ggplot(vor_df, aes(x = x, y = y, group = id, fill = .data[[valcol]])) +
    geom_polygon(color = "white", linewidth = 0.3) +
    coord_fixed() +
    scale_fill_gradientn(
      colors = c("darkblue", "blue", "skyblue", "lightgreen", "yellow", "orange", "red", "darkred"),
      name = valcol
    ) +
    labs(
      title = paste("Voronoi Diagram (", valcol, ")"),
      x = xcol,
      y = ycol
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.title.x = element_text(face = "bold", color = "black"),
      axis.title.y = element_text(face = "bold", color = "black"),
      axis.text = element_text(color = "black")
    )
}