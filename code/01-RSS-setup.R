library(pacman)
p_load(tidyverse, magrittr, tidyRSS, blastula, knitr, DBI, RSQLite, glue)

#set file path and wd
file_path <- "/Users/sophiewill/Documents/data_projects/feedsandalerts"
setwd(file_path)

## Helper function made global so both loops can access it ##
make_md_list <- function(df) {
  if (nrow(df) == 0) return("_No updates for today._")
  
  df %>%
    mutate(
      bullet = glue::glue("* **{item_title}**\n  Published: {item_pub_date} | [View Document]({item_link})\n _{item_description}_\n\n")
    ) %>%
    pull(bullet) %>%
    paste(collapse = "\n") 
}

#function to get RSS feeds
rss_fn <- function() {
  #define today and the connection
  today <- as.Date(Sys.Date())
  con <- dbConnect(RSQLite::SQLite(), "./data/created/feeds_database.db")
  
  # Ensure tables exist
  dbExecute(con, "CREATE TABLE IF NOT EXISTS all_fed_reg 
            (feed_pub_date TEXT, 
            item_title TEXT, 
            item_link TEXT, 
            item_description TEXT, 
            item_pub_date TEXT)")
  dbExecute(con, "CREATE TABLE IF NOT EXISTS filtered_fed_reg
            (feed_pub_date TEXT,
            item_title TEXT, 
            item_link TEXT, 
            item_description TEXT, 
            item_pub_date TEXT)")
  dbExecute(con, "CREATE TABLE IF NOT EXISTS all_gao 
            (feed_pub_date TEXT, 
            item_title TEXT, 
            item_link TEXT, 
            item_description TEXT, 
            item_pub_date TEXT)")
  dbExecute(con, "CREATE TABLE IF NOT EXISTS filtered_gao 
            (feed_pub_date TEXT,
            item_title TEXT, 
            item_link TEXT, 
            item_description TEXT, 
            item_pub_date TEXT)")
  
  # Keywords regex
  keywords_regex <- regex("technology|website|telecommunications|surveillance|IT network|internet network|artificial intelligence|computer|data|privacy|cyber|modernization|fedramp|onegov|online|network|cloud|digitization|USDS|DOGE|a\\.i\\.|u\\.s\\.d\\.s\\.|d\\.o\\.g\\.e\\.|\\btech\\b", ignore_case = TRUE)
  
  #### PROCESS FUNCTION ####
  process_feed <- function(name, link, all_table, filtered_table) {
    tryCatch({
      #get feed and simplify
      df <- tidyfeed(link) %>% 
        select(item_pub_date, feed_pub_date, item_title, item_link, item_description)
      
      #check if feed results are already in the database
      existing <- dbGetQuery(con, glue::glue("SELECT item_title, item_link, item_description, item_pub_date FROM {all_table}"))
      
      #isolate new rows
      new_rows <- df %>%
        mutate(item_pub_date = as.character(item_pub_date), feed_pub_date = as.character(feed_pub_date)) %>%
        anti_join(
          existing %>% mutate(item_pub_date = as.character(item_pub_date)),
          by = c("item_title", "item_link", "item_description", "item_pub_date")
        )
      
      #if there are new rows, then write to to the table
      if (nrow(new_rows) > 0) {
        dbAppendTable(con, all_table, new_rows %>% select(feed_pub_date, item_title, item_link, item_description, item_pub_date))
      }
      
      #filter the new rows
      filtered_new <- new_rows %>% filter(str_detect(item_description, keywords_regex))
      
      #if there are filtered new rows, then write to the table
      if (nrow(filtered_new) > 0) {
        dbAppendTable(con, filtered_table, filtered_new %>% select(feed_pub_date, item_title, item_link, item_description, item_pub_date))
      }
      
      return(make_md_list(filtered_new))
    }, error = function(e) {
      message("Error processing feed ", name, ": ", e$message)
      return("_Error pulling updates for today._")
    })
  }
  
  # 1. Federal Register
  registers <- read.csv("./data/created/fedreg.csv")
  fedreg_results <- map2(registers$name, registers$link, ~process_feed(.x, .y, "all_fed_reg", "filtered_fed_reg")) %>%
    setNames(registers$name)
  
  # 2. GAO
  gao <- read.csv("./data/created/gao.csv")
  gao_results <- map2(gao$name, gao$link, ~process_feed(.x, .y, "all_gao", "filtered_gao")) %>%
    setNames(gao$name)
  
  dbDisconnect(con)
  
  # Combine results into a single environment safe list
  all_results <- c(fedreg_results, gao_results)
  
  # Safeguard: Define defaults so glue doesn't crash if a specific feed wasn't in the CSV
  expected_vars <- c("gsa_fed_register", "ag_fed_register", "ag_sig_fed_register", "edu_fed_register", 
                     "va_fed_register", "epa_fed_register", "epa_sig_fed_register", "interior_fed_register", 
                     "interior_sig_fed_register", "usps_fed_register", "nara_fed_register", "uspto_fed_register",
                     "gao_reports", "gao_legal", "gao_legal_rules", "gao_press", "gao_blog")
  
  for (v in expected_vars) {
    if (!v %in% names(all_results)) all_results[[v]] <- "_No feed configuration found._"
  }
  
  # Generate email string using the explicit list environment
  email_body <- with(all_results, glue::glue("
  # **🌞 RSS Feeds for {today} 🌞**:
  -----
  # _📜FEDERAL REGISTER:📜_ 
  ## 🏢 GSA 🏢\n{gsa_fed_register}
  ## 🌾 Agriculture 🌾\n{ag_fed_register}
  ### 🌾 Agriculture significant docs 🌾\n{ag_sig_fed_register}
  ## 📚 Education 📚\n{edu_fed_register}
  ## 🪖 VA 🪖\n{va_fed_register}
  ## 🌎 EPA 🌎\n{epa_fed_register}
  ### 🌎 EPA significant docs 🌎\n{epa_sig_fed_register}
  ## 🏜️ Interior 🏜\n{interior_fed_register}
  ### 🏜️ Interior significant docs 🏜\n{interior_sig_fed_register}
  ## 💌 USPS 💌\n{usps_fed_register}
  ## 🏛️ Archives 🏛\n{nara_fed_register}
  ## 🔬 Patents & Trademarks 🔬\n{uspto_fed_register}
  -----
  # _📜GAO:📜_\n
  ## Reports\n{gao_reports}
  ## Legal\n{gao_legal}
  ## Legal Rules\n{gao_legal_rules}
  ## Press releases\n{gao_press}
  ## Blog\n{gao_blog}
  -----
  _This is an automated message sent from KSW's Work Laptop_
  "))
  
  #create email and log file
  email <- compose_email(body = md(email_body))
  log_file <- "./data/created/email_log.txt"
  
  #send email and log it
  log_output <- capture.output({
    tryCatch({
      email %>% smtp_send(
        to = "sophie.will@fedscoop.com",
        from = "ksophiewill@gmail.com",
        subject = "🌞 RSS Updates 🌞",
        verbose = FALSE,
        credentials = creds_envvar(user = "ksophiewill@gmail.com", pass_envvar = "SMTP_PASSWORD", provider = "gmail")
      )
      cat(sprintf("[%s] SUCCESS: Email sent\n", Sys.time()))
    }, error = function(e) {
      cat(sprintf("[%s] ERROR: %s\n", Sys.time(), e$message))
    })
  })
  #log it 
  cat(log_output, file = log_file, sep = "\n", append = TRUE)
}