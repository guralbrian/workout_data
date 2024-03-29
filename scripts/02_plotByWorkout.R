data_new <- read.csv("data/processed/lid/old_routine.csv", row.names = 1)

# Plot
# Make a palette for the workout days and subtypes
pal <- c()
pal[[1]] <- c("#e66b29", "#e89a6f")
pal[[2]] <- c("#0f7bd4", "#75acd9")
pal[[3]] <- c("#1cb855", "#97debc")
#8346eb #ad8be8
#23b06c #77c9a2
plot.list <- c()
for(i in 1:length(unique(data_new$day_type))){
variable <- unique(data_new$day_type)[[i]]
# Make an ordered list of workout types by weight
temp.df <- data_new |> 
  filter(workout != "" & day_type == variable) |> 
  mutate(date = ymd(date),
         day_type = factor(day_type),
         workout = factor(workout)) |>
  group_by(date, workout, day_type, workout_day) |> 
  select(workout, date, weight_real, weight_total, day_type) |> 
  summarize(workout_total = sum(as.numeric(weight_total), na.rm = TRUE)) 

workout.list <- temp.df |> 
  group_by(workout) |> 
  arrange(desc(workout_total)) |> 
  slice_head(n = 1) |> 
  ungroup() |> 
  arrange(desc(workout_total)) |> 
  pull(workout)

temp.df$workout <- factor(temp.df$workout, levels = workout.list)

plot.list[[i]] <- ggplot(temp.df,
                         aes(x = date, y = workout_total, color = workout_day)) +
  geom_point(size = 4) +
  geom_smooth(method = "lm", se = F) +
  scale_color_manual(values = pal[[i]]) +
  theme_minimal() +
  facet_wrap(~workout) +
  theme(axis.title.x = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(color = "white")) +
  ylab("Cumulative weight moved (lbs)") 
}

library(patchwork)


png(filename = "results/lid/old_routine/workout_type_net_wt.png",
    width = 7,
    height = 16,
    units = "in",
    res = 300)
wrap_plots(plot.list, ncol = 1)
dev.off()
