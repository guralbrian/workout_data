library(tidyverse)
library(stringr)
# Load the data from the Excel file
workout_data <- read.csv("data/raw/lid/old_routine.csv")


# First make it so that there is only one "iteration"

no_iter <- workout_data[,1:8] 
row_count <- nrow(no_iter)
no_iter[c((nrow(no_iter)+1):(nrow(no_iter)+row_count)),] <- workout_data[,c(1,9:15)]
no_iter[c((nrow(no_iter)+1):(nrow(no_iter)+row_count)),] <- workout_data[,c(1,16:22)]
no_iter[c((nrow(no_iter)+1):(nrow(no_iter)+row_count)),] <- workout_data[,c(1,23:29)]
no_iter[c((nrow(no_iter)+1):(nrow(no_iter)+row_count)),] <- workout_data[,c(1,30:36)]
no_iter[c((nrow(no_iter)+1):(nrow(no_iter)+row_count)),] <- workout_data[,c(1,37:43)]

colnames(no_iter) <- c("workout", "weight", "set_1", "set_2", "set_3", "set_4", "set_5", "set_6")

no_iter$date <- "empty"
no_iter$gym <- "empty"
no_iter$workout_day <- "empty"
# Replace empty cells 
no_iter[is.na(no_iter)] <- "empty"
# track the type of day
# Loop through each row, use while and str_detect to assign
workout_rows <- c()
workout_type <- date_of <- gym <- "empty"

for(workout in 1:nrow(no_iter)){
  if(str_detect(no_iter[workout ,1], "Pull ") |
     str_detect(no_iter[workout ,1], "Push ") |
     str_detect(no_iter[workout ,1], "Legs ")){
    workout_type <- no_iter[workout ,1] |> as.character()
    date_of <- no_iter[workout, 2] |> as.character()
    gym <- no_iter[workout ,3] |> as.character()
  } else {
    no_iter[workout, "date"] <- date_of
    no_iter$workout_day[workout] <- workout_type
    no_iter$gym[workout] <- gym
  }
}



# Subset to just workout rows 
data <- no_iter |>
  filter(date != "empty")

# Make supersets their own rows
ss_data <- data[str_detect(data$workout, "SS "), ]
ss_data$workout <- lapply(str_split(ss_data$workout, "SS "), "[[", 2) |> unlist()
ss_data$weight <- 6 #lapply(str_split(ss_data$set_4, "[(]"), "[[", 2) |> unlist()
#ss_data$weight <- NA #lapply(str_split(ss_data$weight, "[)]"), "[[", 1) |> unlist()
ss_data$set_4 <- lapply(str_split(ss_data$set_4, "[()]"), "[[", 1) |> unlist()


ss_data[,3:5] <- ss_data[,6:8]
ss_data[,6:8] <- "empty"

# Join back together
data <- data |> rbind(ss_data)

# Clean up rows that contain the first workout in the SS
data[str_detect(data$workout, "SS "), 6:8] <- "empty"
ss_workouts <- data$workout[str_detect(data$workout, "SS ")]
ss_workouts <- lapply(str_split(ss_workouts, "SS "), "[[", 1) |> unlist()
data[str_detect(data$workout, "SS "), 1] <- ss_workouts

# Make data long
data_long <- pivot_longer(data, cols = c("set_1", "set_2", "set_3", "set_4", "set_5", "set_6"))

# Continue data cleaning...
colnames(data_long)[7] <- "reps"
weight_notes <- str_split(data_long$weight, "[(]")
data_long$weight <- lapply(weight_notes, "[[", 1) |> unlist() # add weight back w/o notes

# Add 'none' to notes for empty notes
for(set in 1:length(weight_notes)){
  if(length(weight_notes[[set]]) < 2){
    weight_notes[[set]] <- c(weight_notes[[set]], "none")
  }
}

# Add notes to long data
data_long$notes <- lapply(weight_notes, "[[", 2) |> unlist()

# Now, add the mid-set weight changes
data_long$weight_real <- str_extract(data_long$reps, "\\((\\d+)\\)") %>% 
  str_remove_all("[()]") #! this doesn't extract characters or special characters, stuff is missing

# Leave only rep numbers in reps row
data_long$reps[which(str_detect(data_long$reps, "[(]"))] <- str_extract(data_long$reps, "\\d+(?=\\()")[which(!is.na(str_extract(data_long$reps, "\\d+(?=\\()")))]

# Join starting and changed weights
data_long <- data_long %>% 
  group_by(date, workout) %>% 
  mutate(weight_adj = case_when(
    name == "set_1" ~ weight,
    !is.na(weight_real) ~ weight_real
  )
  ) %>% 
  fill(weight_adj, .direction = "down") %>%
  ungroup()

# Make supported pull ups reflect bw
bw <- 185
data_adj <- data_long %>% 
  mutate(
    #weight_adj = case_when(weight_adj == "" ~ 0),
    weight_adj = abs(as.numeric(weight_adj)),
    bw_adjusted = case_when(
      str_detect(workout, "Pulldowns/Pullups/Chinups") ~ bw - weight_adj,
      str_detect(workout, "Bulgarian") ~ bw/2,
      str_detect(workout, "push-ups") ~ bw/2,
      str_detect(workout, "Dips") ~ bw/2 + weight_adj,
      TRUE ~ weight_adj
    )
  )



# Remove empty workouts and unused rows

data_adj <- data_adj %>% 
  select(-c(weight_adj, weight)) %>% 
  filter(reps != "empty" & reps != "Reps")

data_adj$date <- data_adj$date %>% mdy() # Ridiculous date conversion for excel
#data_adj[which(data_adj$date == "2024-03-12" & data_adj$workout == "3x8-12 Pulldowns/Pullups/Chinups"),"weight_real"] <- 70


# Cumulative weight moved over time
data_adj <- data_adj %>% 
  ungroup() %>% 
  arrange(., date) %>% 
  mutate(
    reps = as.numeric(reps),
    weight_total = bw_adjusted * reps,
    weight_alltime = cumsum(weight_total/2204.6),
    day_type = case_when(
      workout_day %in% c("Pull A", "Pull B") ~ "Pull",
      workout_day %in% c("Push A", "Push B") ~ "Push",
      workout_day %in% c("Legs A", "Legs B") ~ "Legs"
    )
  ) 


write.csv(data_adj, "data/processed/lid/old_routine.csv")

