plot_dists <- function(x0, x1)  {

  library(ggplot2)

  df1 <- as.data.frame(x1)
  colnames(df1) <- c("size", "freq")
  df1$group <- "pop_df"

  df0 <- as.data.frame(x0)
  colnames(df0) <- c("size", "freq")
  df0$group <- "baseline"

  # Ensure 'size' is numeric
  df1$size <- as.numeric(as.character(df1$size))
  df0$size <- as.numeric(as.character(df0$size))

  df1$prop <- df1$freq / sum(df1$freq)
  df0$prop <- df0$freq / sum(df0$freq)

  df <- rbind(df1, df0)

  ggplot(df, aes(x = size, y = prop, fill = group)) +
    geom_col(position = "dodge") +
    theme_minimal()
}
