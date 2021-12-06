# Libraries ---------------------------------------------------------------
Libraries <- as.list(c("tidyverse","pdftools","janitor",
                       "rvest","xml2","tabulizer","lubridate","tabulizerjars"))
lapply(Libraries,require,character.only=TRUE)
rm(Libraries)
# Website -----------------------------------------------------------------
Reports <- data.frame(pdf_links = read_html("https://www.nicd.ac.za/diseases-a-z-index/disease-index-covid-19/surveillance-reports/daily-hospital-surveillance-datcov-report/") %>%
  html_elements("a") %>%
  html_attr("href") %>% 
  grep(".pdf$",.,value=TRUE))


Reports <- Reports %>% 
  filter(str_detect(pdf_links,"Sentinel")) %>% 
  mutate(report_date = str_extract(pdf_links,"\\d{8}.pdf") %>% 
           str_replace(.,"[[:punct:]]pdf",""),
         report_date = ymd(report_date))
# Create Sub-Directory and Download ---------------------------------------
dir.create("Daily_Reports")

for(i in seq_along(1:nrow(Reports))){
  download.file(url= Reports$pdf_links[[i]],
                destfile= paste0("./Daily_Reports/",Reports$report_date[[i]],"_Daily_Hospitalisations.pdf")
                )
}