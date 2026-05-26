# Feeds and Alerts

#### By K. Sophie Will

This repository automates RSS feeds to customized email alerts.

### Repo structure:

- code:

  - 01-RSS-setup.R - code to pull RSS feeds and format it to be readable, writes RSS feeds to database then filters it

  - 02-scheduler.R - schedule for screen & caffeinate to use

- docs:

  - data-dictionary.qmd - daily log of changes made to this repo

- data

  - created

    - fedreg.csv - list of names and links to the federal register RSS feeds

    - gao.csv - list of names and links to the gao RSS feeds

    - feeds_database.db - SQL database of the feeds

    - email_log.txt - log of emails sent

### INFO FOR CAFFEINATE

###install screen if needed

brew install screen

###start a named session

screen -S RSSfeeds

###run caffeinate in screen

caffeinate -ism Rscript /path/to/scheduler.R
