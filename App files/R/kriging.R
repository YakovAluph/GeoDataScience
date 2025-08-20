# THIS MODULE IS NOT FINISHED YET AND ALL THIS CODE IS STILL IN SERVER.R

perform_kriging_full <- function(df, fmla, vgm_model, input, valcol, xcol, ycol, model_type, grid_resolution) {
  
  # build prediction grid
  xr <- seq(bbox(df)[1,1], bbox(df)[1,2], length.out = grid_resolution)
  yr <- seq(bbox(df)[2,1], bbox(df)[2,2], length.out = grid_resolution)
  grid_df <- setNames(expand.grid(xr, yr), c(xcol, ycol))
  coordinates(grid_df) <- c(xcol, ycol)
  proj4string(grid_df) <- CRS(proj4string(df))
  
  kriged <- tryCatch({
    switch(krigingType,
           "OK" = krige(fmla, df, grid_df, model = vgm_model),
           "SK" = krige(fmla, df, grid_df, model = vgm_model,
                        beta = mean(df@data[[valcol]], na.rm = TRUE)),
           "UK" = krige(as.formula(paste(valcol, "~", xcol, "+", ycol)), df, grid_df, model = vgm_model),
           
           "IK" = {
             ...
             krige(indVar ~ 1, df_ind, grid_df, model = vgm_model_ind)
           },
           
           "CK" = {
             ...
             v <- tryCatch(variogram(g), error = function(e) {
               cat("❌ Co-kriging variogram() error:", conditionMessage(e), "\n")
               showModal(modalDialog("Error calculating experimental variograms.")); return(NULL)
             })
             req(v)
             
             fitted_model <- tryCatch({
               fit.lmc(v, g, vgm(model = model_type, psill = 1,
                                 range = diff(bbox(df)[1,]), nugget = 0))
             }, error = function(e) {
               cat("❌ Co-kriging fit.lmc() error:", conditionMessage(e), "\n")
               showModal(modalDialog("Failed to fit cross-variogram. Try different variables or a simpler model.")); return(NULL)
             })
             req(fitted_model)
             
             tryCatch({
               predict(fitted_model, grid_df)
             }, error = function(e) {
               cat("❌ Co-kriging predict() error:", conditionMessage(e), "\n")
               showModal(modalDialog("Co-Kriging prediction failed.")); return(NULL)
             })
           }
    )
  }, error = function(e) {
    cat("❌ General kriging error:", conditionMessage(e), "\n")
    showModal(modalDialog(
      title = "Incompatible Configuration",
      "The selected Kriging method and Variogram model function are incompatible with the dataset or failed to compute. Please try a different combination.",
      easyClose = TRUE
    ))
    return(NULL)
  })
  
  return(kriged)
}