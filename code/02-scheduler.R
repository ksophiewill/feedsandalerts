library(pacman)
p_load(tidyverse)

# Define path and source it
file_path <- "/Users/sophiewill/Documents/data_projects/feedsandalerts"
source(paste0(file_path, "/code/01-RSS-setup.R"))

# Define times to run
target_times <- c("07:00", "13:00", "23:45")

# Keep track of what has already been run to avoid duplicates
last_run_date <- Sys.Date()
runs_today <- c()

message("Custom scheduler started. Target times: ", paste(target_times, collapse = ", "))
message("Scheduler is running under caffeinate. Detach with Ctrl+A then D.")

# The Infinite Polling Loop
while (TRUE) {
  current_time <- Sys.time()
  current_date <- as.Date(current_time)
  current_hm   <- format(current_time, "%H:%M")
  
  # Reset the daily tracker at midnight
  if (current_date > last_run_date) {
    last_run_date <- current_date
    runs_today    <- c()
  }
  
  # If the current minute is a target AND we haven't run it yet today...
  if (current_hm %in% target_times && !(current_hm %in% runs_today)) {
    message("\n[", Sys.time(), "] Target time reached: ", current_hm, ". Executing RSS job...")
    
    tryCatch({
      rss_fn()
      message("[", Sys.time(), "] Successfully completed job for ", current_hm)
      
      # Record that we successfully triggered this time slot today
      runs_today <- c(runs_today, current_hm)
      
    }, error = function(e) {
      message("[", Sys.time(), "] ERROR running job: ", e$message)
    })
  }
  
  # Sleep for 20 seconds before checking the clock again. 

  Sys.sleep(30)
}