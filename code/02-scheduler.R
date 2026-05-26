library(pacman)
p_load(tidyverse, later)

#define path and source it
file_path <- "/Users/sophiewill/Documents/data_projects/feedsandalerts"
source(paste0(file_path, "/code/01-RSS-setup.R"))

#set up safe run
safe_run <- function(label, fn, retries = 3) {
  for (attempt in seq_len(retries)) {
    result <- tryCatch({
      message("[", Sys.time(), "] Running ", label, if (attempt > 1) paste0(" (attempt ", attempt, ")") else "")
      fn()
      message("[", Sys.time(), "] Completed ", label)
      return(invisible(TRUE))
    }, error = function(e) {
      message("[", Sys.time(), "] ERROR in ", label, ": ", e$message)
      if (attempt < retries) Sys.sleep(30)
      return(invisible(FALSE))
    })
  }
  message("[", Sys.time(), "] FAILED ", label, " after ", retries, " attempts")
}

#set daily schedule
schedule_daily <- function(label, hour, minute, task_fn) {
  run_at_next <- function() {
    now    <- Sys.time()
    target <- as.POSIXct(format(now, paste0("%Y-%m-%d ", sprintf("%02d:%02d:00", hour, minute))), tz = Sys.timezone())
    
    if (target <= now) target <- target + 86400
    
    delay_secs <- as.numeric(difftime(target, now, units = "secs"))
    message("[", Sys.time(), "] ", label, " next run in ", round(delay_secs / 3600, 1), " hours (", target, ")")
    
    later(function() {
      safe_run(label, task_fn)
      run_at_next()
    }, delay_secs)
  }
  run_at_next()
}

# Your jobs
schedule_daily("Morning",   7,  0, rss_fn)
schedule_daily("Afternoon", 13, 00, rss_fn)
schedule_daily("Evening",   23, 45, rss_fn)

message("Scheduler is running. Detach with Ctrl+A then D.")

# Keep session alive passively without thrashing the event loop
while (TRUE) {
  later::run_now() # Explicitly tells 'later' to execute anything due
  Sys.sleep(5)    
}