library(tidyverse)


vec_dice <- c(1, 2, 3, 4, 5, 6)

df_dice_face <- tibble(
  dice_result = vec_dice,
  x = list(2, c(1,3), c(1,2,3), c(1,1,3,3), c(1,1,2,3,3), c(1,1,1,3,3,3)),
  y = list(2, c(1,3), c(1,2,3), c(1,3,1,3), c(1,3,2,1,3), c(1,2,3,1,2,3))
) |> unnest(cols = c(x, y))

df_dice_outline <- tibble(
    xmin = 0.75,
    xmax = 3.25,
    ymin = 0.75,
    ymax = 3.25
)


roll_dice <- function(n) {
 if ( n < 1) {
   stop("n must be a positive integer")}
 

 if(n>5) {
    stop("n must be less than or equal to 5")
 }

  dice_results <- sample(vec_dice, n, replace = TRUE)

df_dice_out <- tibble()
for(i in 1:n) {
  dice_result <- dice_results[i]
  x_adjust <- (i-1)*3

  df_dice_placeholder <- tibble(dice_number = i,
  dice_result) |>
  left_join(df_dice_face, by = "dice_result") |>
  bind_cols(df_dice_outline) |>
  mutate(x = x +x_adjust,
         y = y,
         xmin = xmin + x_adjust,
         xmax = xmax + x_adjust,
         ymin = ymin,
         ymax = ymax)
  

  df_dice_out <- bind_rows(df_dice_out, df_dice_placeholder)
}

return(df_dice_out)
}

plot_dice <- function(df, n) {
  size_adjust = ifelse(n < 3, 20, 20-(2*n))
  ggplot(data = df) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "pink", color = "black") +
  geom_point(aes(x = x, y = y), size = size_adjust, color = "white") +
  coord_fixed() +
  theme_void()
}

roll_and_record <- function(n) {
  df <- roll_dice(n)
  print(df)
  plot_dice(df, n)
}
