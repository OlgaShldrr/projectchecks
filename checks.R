print(Sys.time())
setwd("/home/dockr/resubmissions")
library(blastula)
library(dplyr)
library(lubridate)
library(reader)
Sys.setenv(RSTUDIO_PANDOC="/usr/lib/rstudio-server/bin/pandoc")


# files for dashboards------------
files <<- list.files("XXXdatapath/data/1920/")
checkBSQ <- data.frame(File = as.character(paste0("XXXdatapath/data/1920/",files)),
                       Survey= "BSQ",
                       Created= as.Date(as.character(file.info(paste0("XXXdatapath/data/1920/",files))$ctime)),
                       stringsAsFactors=FALSE)
files <<- list.files("XXXdatapath/data/1920/")
checkBSQFM <- data.frame(File = as.character(paste0("XXXdatapath/data/1920/",files)),
                         Survey= "BSQ FM",
                         Created= as.Date(as.character(file.info(paste0("XXXdatapath/data/1920/",files))$ctime)),
                         stringsAsFactors=FALSE)
check <- rbind(checkBSQ, checkBSQFM)
files <<- list.files("XXXdatapath/data/1920/")
checkAPQ <- data.frame(File = as.character(paste0("XXXdatapath/data/1920/",files)),
                       Survey= "APQ",
                       Created= as.Date(as.character(file.info(paste0("XXXdatapath/data/1920/",files))$ctime)),
                       stringsAsFactors=FALSE)
check <- rbind(check, checkAPQ)
files <<- list.files("XXXdatapath/data/1920/")
checkSCDS <- data.frame(File = as.character(paste0("XXXdatapath/data/1920/",files)),
                        Survey= "SCDS",
                        Created= as.Date(as.character(file.info(paste0("XXXdatapath/data/1920/",files))$ctime)),
                        stringsAsFactors=FALSE)
check <- rbind(check, checkSCDS)
check$Outdated = as.logical(check$Created<today())

check <- check %>% 
  mutate(
    Status = case_when(
      grepl(x=check$File, pattern = "INCOMPLETE") ~ "\U0002705",
      Outdated == TRUE ~ "\U000274C",
      Outdated == FALSE ~ "\U000274E")
  ) %>% 
  tidyr::separate(File, into = c("rest", "File"), sep="data/") %>% 
  select(Status, File, Created, Outdated)


# deployment of dashboards-------------
conbsq <- length(readLines("XXXdatapath/run.log"))
status_bsq <- n.readLines("XXXdatapath/run.log", n=1, skip=conbsq-1)
conbsqfm <- length(readLines("XXXdatapath/run.log"))
status_bsqfm <- n.readLines("XXXdatapath/run.log", n=1, skip=conbsqfm-1)
conapq <- length(readLines("XXXdatapath/run.log"))
status_apq <- n.readLines("XXXdatapath/run.log", n=1, skip=conapq-1)
conscds <- length(readLines("XXXdatapath/run.log"))
status_scds <- n.readLines("XXXdatapath/run.log", n=1, skip=conscds-1)

#sql server api pull-----------

consql <- length(readLines("XXXdatapath/bsq_sql.log"))
status_sql <- n.readLines("XXXdatapath/bsq_sql.log", n=1, skip=consql-1)
status_sql <-  !grepl(status_sql, pattern = "halted")


#removing old reports for api-----------
conscdsremove <- length(readLines("XXXdatapath/remove_old_reports.log"))
status_scdsremove <- n.readLines("XXXdatapath/remove_old_reports.log", n=1, skip=conscdsremove-1)
status_scdsremove <- status_scdsremove %>% 
  grep(.,pattern = "Halted", invert = TRUE) %>% 
  as.logical()


#resubmissions -------------
conresub <- length(readLines("XXXdatapath/resubmission.log"))
status_resub <- n.readLines("XXXdatapath/resubmission.log", n=1, skip=conresub-1)
status_resub <- status_resub %>% 
  grepl(pattern = "environment")


#data import for cleaning app---------
status_scds_import <- lubridate::date(file.info("/XXXdatapath/users.xlsx")$ctime)==lubridate::today()


#reports created for  cleaning api----------
scds_reports_api <- dir.exists(paste0("XXXdatapath/reports/", lubridate::today()))

#survey_participation_history_toSQL -------------
consurvey_part_SQL <- length(readLines("XXXdatapath/data_pull.log"))
status_survey_part_SQL <- n.readLines("XXXdatapath/data_pull.log", n=1, skip=consurvey_part_SQL-1)
status_survey_part_SQL <- status_survey_part_SQL %>% 
  grep(.,pattern = "Halted", invert = TRUE)%>% 
  as.logical()

#overall status-----------
status_all <- all(status_survey_part_SQL, status_sql, status_scdsremove, status_resub, status_scds_import, scds_reports_api, !(filter(check, !grepl(x=File, pattern = "LOCKED_INCOMPLETE.csv", fixed = TRUE))$Outdated),
                  grepl(x = c(status_apq, status_bsq, status_bsqfm, status_scds), pattern = "success"))

if (status_all=="TRUE") {
  status_all_emoji <- "\U0002705"
} else {
  status_all_emoji <- "\U000274C"
}



#email
send_alert_email <- function(email) {
  email_object <- blastula::render_email("project_checks/checks.Rmd")
  
  blastula::smtp_send(
    email = email_object,
    from = "XXX@XXX.com",
    to = email,
    subject = paste0("Current Status of R Projects ", status_all_emoji),
    credentials = blastula::creds_file(".email_creds")
    
  )
}

send_alert_email("test@test.COM")  

