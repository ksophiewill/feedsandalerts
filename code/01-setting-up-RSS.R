library(pacman)
p_load(tidyverse, magrittr, tidyRSS, blastula, cronR, shinyFiles)

#set today
today <- as.Date(Sys.Date())

# justice_fed_register <- tidyfeed("https://www.federalregister.gov/api/v1/documents.rss?conditions%5Bagencies%5D%5B%5D=justice-department") %>% 
  # filter(feed_pub_date >= today | item_pub_date >= today)

gsa_fed_register <- tidyfeed("https://www.federalregister.gov/api/v1/documents.rss?conditions%5Bagencies%5D%5B%5D=general-services-administration") %>% 
  # filter(feed_pub_date >= today | item_pub_date >= today) %>% 
  slice_head(n = 1)

#set up creds https://myaccount.google.com/u/1/apppasswords
create_smtp_creds_file(
  file = "email_creds",
  user = "ksophiewill@gmail.com",
  provider = "gmail"
)

#set up email
email <- compose_email(
  body = md(glue:::glue(
    "Federal register updates for {today}
    
    GSA {gsa_fed_register}"
)))

#send email
email %>%  smtp_send(
  to = "sophie.will@fedscoop.com",
  from = "ksophiewill@gmail.com",
  subject = "Fed Register test", 
  credentials = creds_file("email_creds")
)
