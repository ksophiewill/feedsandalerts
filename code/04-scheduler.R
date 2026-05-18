library(pacman)
p_load(tidyverse, later)

source("./code/01-morning-RSS.R")
source("./code/02-afternoon-RSS.R")
source("./code/03-evening-RSS.R")

# wrap each send in a safe function so one failure doesn't kill the scheduler
safe_run <- function(label, fn) {
  tryCatch({
    message("[", Sys.time(), "] Running ", label)
    fn()
    message("[", Sys.time(), "] Completed ", label)
  }, error = function(e) {
    message("[", Sys.time(), "] ERROR in ", label, ": ", e$message)
  })
}

#daily schedule
schedule_daily <- function(label, hour, minute, task_fn) {
  run_at_next <- function() {
    now    <- Sys.time()
    target <- as.POSIXct(format(now, paste0("%Y-%m-%d ",
                         sprintf("%02d:%02d:00", hour, minute))),
                         tz = Sys.timezone())
    
    # if that time has already passed today, push to tomorrow
    if (target <= now) target <- target + 86400
    
    delay_secs <- as.numeric(difftime(target, now, units = "secs"))
    message("[", Sys.time(), "] ", label, " next run in ",
            round(delay_secs / 3600, 1), " hours (", target, ")")
    
    later(function() {
      safe_run(label, task_fn)
      run_at_next()  # reschedule for tomorrow
    }, delay_secs)
  }
  run_at_next()
}

# your three jobs
schedule_daily("Morning",   7,  0,  morning_fn)
schedule_daily("Afternoon", 13, 0,  afternoon_fn)
schedule_daily("Evening",   23, 59, evening_fn)

# keep the session alive and let later's event loop tick
message("Scheduler running. Detach with Ctrl+A then D.")
while (TRUE) later::run_now(sleep_time = 60)