library(pacman)
p_load(tidyverse, magrittr, tidyRSS, blastula, knitr)

afternoon_fn <- function() {
  
  #set today
  today <- as.Date(Sys.Date())
  
  #### function to get registers and clean ####
  make_register_lists <- function(name, link){
    
    ## helper function to make a md list instead of a table ##
    make_md_list <- function(df) {
      if (nrow(df) == 0) return("_No updates for today._")
      
      df %>%
        mutate(
          #creating a cleaner bulleted list with extra lines
          bullet = glue::glue("* **{item_title}**\n  Published: {item_pub_date} | [View Document]({item_link})\n _{item_description}_\n\n")
        ) %>%
        pull(bullet) %>%
        paste(collapse = "\n") 
    }
      #cmbine all bullets into one long string
    result <- tidyfeed(link) %>% 
      filter(feed_pub_date >= today | item_pub_date >= today) %>% #only items posted or put in the feed today
      select(item_pub_date, feed_pub_date, item_title, item_link, item_description) %>% #just which columns I'm interested in
      filter(str_detect(item_description, 
                        regex("technology|website|artificial intelligence|computer|data|privacy|cyber|modernization|fedramp|onegov|online|network|cloud|digitization|USDS|DOGE|a\\.i\\.|u\\.s\\.d\\.s\\.|d\\.o\\.g\\.e\\.|\\btech\\b", 
                              ignore_case = TRUE))) %>% #get only key words
      make_md_list()
    
    return(result)
  }
  
  #read in registers 
  registers <- read.csv("./data/created/fedreg/fedreg.csv")
  
  #run function and set names 
  result_list <- map2(registers$name, registers$link, make_register_lists) %>%
    setNames(registers$name)
  
    #write that
    write_rds(result_list, "./data/created/fedreg/afternoon_register.rds")
  
  #compare what was just got with what was gotten this morning
  morning <- read_rds("./data/created/fedreg/morning_register.rds") 
  
  #### filter out what was already sent this morning ####
  result_list_updated <- imap(result_list, function(afternoon_str, register_name) {
    
    # if no updates, nothing to compare
    if (afternoon_str == "_No updates for today._") return(afternoon_str)
    
    morning_str <- morning[[register_name]]
    
    # split each string back into individual bullets on the double newline separator
    afternoon_bullets <- str_split(afternoon_str, "\n(?=\\*)") [[1]]
    morning_bullets   <- str_split(morning_str,   "\n(?=\\*)")[[1]]
    
    # keep only bullets not in the morning send
    new_bullets <- afternoon_bullets[!afternoon_bullets %in% morning_bullets]
    
    if (length(new_bullets) == 0) return("_No updates for today._")
    
    paste(new_bullets, collapse = "\n")
  })
  
  #put them each into the environment
  list2env(result_list_updated, envir = .GlobalEnv)
  
  ###### set up email #####
  email <- compose_email(
    body = md(glue::glue(
      "# **🍽️ Afternoon Federal Register updates for {today} 🍽️**:
      
      ## 🏢 GSA
      {gsa_fed_register}
      
      ## ⚖️ Justice
      {justice_fed_register}
      
      ### ⚖️ Justice significant docs
      {justice_sig_fed_register}
      
      ## 📚 Education
      {edu_fed_register}
      
      ## 🪖 VA
      {va_fed_register}
      
      ## 👵 SSA
      {ssa_fed_register}
      
      ## 🌎 EPA
      {epa_fed_register}
      
      ### 🌎 EPA significant docs
      {epa_sig_fed_register}
      
      ## 🏜️ Interior
      {interior_fed_register}
      
      ### 🏜️ Interior significant docs
      {interior_sig_fed_register}
      
      -----
      _This is an automated message sent at 1:00 p.m. {today} from KSW's Work Laptop_"
    ))
  )
  
  #check that there is a password via smtp
  #set up creds here https://myaccount.google.com/u/1/apppasswords and put in renviron
  #usethis::edit_r_environ() if not set up
  smtp_password <- Sys.getenv("SMTP_PASSWORD")
  
  # #send email
  email %>%  smtp_send(
    to = "sophie.will@fedscoop.com",
    from = "ksophiewill@gmail.com",
    subject = "Afternoon Fed Register Updates",
    credentials = creds_envvar(
      user = "ksophiewill@gmail.com",
      pass_envvar = "SMTP_PASSWORD",
      provider = "gmail"
    )
  )
}