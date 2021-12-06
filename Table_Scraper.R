# Libraries ---------------------------------------------------------------
Libraries <- as.list(c("tidyverse","pdftools","janitor",
                       "rvest","xml2","tabulizer","lubridate","tabulizerjars"))
lapply(Libraries,require,character.only=TRUE)
rm(Libraries)
# Collect Documents -------------------------------------------------------
Report_Document <- data.frame(report_document =
                                list.files(full.names=TRUE,path ="./Daily_Reports"))

Report_Document$document <- lapply(Report_Document$report_document,pdf_text)
# Date_Extractor ----------------------------------------------------------
date_extractor <- function(x){
  y <- data.frame(verified_date =
                    str_extract(x[,2],"^\\s{1,}\\d{1,2}[[:punct:]]\\d{1,2}[[:punct:]]\\d{4}|\\d{1,2}\\s{1,}\\w{1,}\\s{1,}\\d{4}|\\s{1,}\\w{1,}\\s{1,}\\d{1,2}[[:punct:]]\\s{1,}\\d{4}|\\s{1,}\\w{1,}\\s{1,}\\d{2,}"))
  return(y)
}

Report_Document <- cbind.data.frame(Report_Document,date_extractor(Report_Document))

Tables <- list()
for(i in seq_along(1:nrow(Report_Document))){
  Tables[[i]] <- try(extract_tables(file = Report_Document$report_document[[i]],
                                    output = "data.frame"))
}
####### Ignore the "Error in read.table(file=file,header=header,sep=sep,quote=quote)" message. 


# Select Tables -----------------------------------------------------------
find_valid_tables <- function(x){
  number_of_rows <- 1:length(x)
  tables <- list()
  for(i in seq_along(x)){
    for(j in seq_along(x[[i]])){
      tables[[i]] <- x[[i]][[j]]
    }
  }
  return(tables)}

Tables <- find_valid_tables(Tables)

names(Tables) <- Report_Document$verified_date

Tables <- lapply(Tables,as.data.frame)

Tables <- Tables %>% 
  keep(~nrow(.)>25)

Clean_Tables <- Tables %>% 
  keep(~ncol(.)==8)

Clean_Tables <- map(Clean_Tables,na.omit)

Clean_Tables <- Clean_Tables %>% 
  keep(~nrow(.)>20)
# Format_Tables -----------------------------------------------------------
table_cleaner <- function(x){
x %>% 
  mutate(province = str_extract(x$Province,"Eastern Cape|Western Cape|Northern Cape|KwaZulu-Natal|Gauteng|Mpumalanga|Free State|Limpopo|North West")) %>%
  fill(province,.direction="down") %>% 
  filter(!str_detect(Province,"Total")) %>% 
  filter(!str_detect(Province,"Eastern Cape|Western Cape|Northern Cape|KwaZulu-Natal|Gauteng|Mpumalanga|Free State|Limpopo|North West")) %>% 
  rename(sector =Province)}

Clean_Tables <- lapply(Clean_Tables,table_cleaner)
# Add Date ----------------------------------------------------------------
for(i in seq_along(1:length(Clean_Tables))){
  Clean_Tables[[i]]$date_extracted <- names(Clean_Tables)[[i]]
}

Final_Table <- do.call(rbind,Clean_Tables)

Final_Table <- tibble(Final_Table)

