options(stringsAsFactors = FALSE)

library(rvest)
library(tidyverse)
library(lubridate)
library(tidyquant)

# url with search criteria
# make url with specific end date as todays data
td <- paste(str_pad(month(today()), 2, pad = "0"), "/", str_pad(day(today()), 2, pad = "0"), "/", year(today()), sep = "")
url <- paste("https://searchwww.sec.gov/EDGARFSClient/jsp/EDGAR_MainAccess.jsp?search_text=%2234-88318%22+or+%2234-88465%22&sort=ReverseDate&startDoc=1&numResults=100&isAdv=true&formType=Form8K&fromDate=03/04/2020&toDate=,",
             td,
             "&stemming=true",
             sep = "")

# get total number of hits because we need to loop over pages
totalnum <- read_html(url) %>% html_node('#header .normal+ .normalbold') %>% html_text() %>% as.numeric()

# get the number of pages we need to loop through
pages <- floor(totalnum/100) + 1

# make list of starting entries 
pagestubs <- (1:pages)*100 + 1 - 100

# make function to read tables by page
read_by_page <- function(p) {
  
  url <- paste("https://searchwww.sec.gov/EDGARFSClient/jsp/EDGAR_MainAccess.jsp?search_text=%2234-88318%22+or+%2234-88465%22&sort=ReverseDate&startDoc=",
               p,
               "&numResults=100&isAdv=true&formType=Form8K&fromDate=03/04/2020&toDate=",
               td,
               "&stemming=true",
               sep = "")
  
  # read the page
  page <- read_html(url)
  
  # get the company names
  compnames <- 
    page %>%
    html_nodes(xpath = paste0("body/form/table[3]/tr/td[3]/table[4]/tr[2]/td/div/table[2]/tr/td[2]/font[2]")) %>%
    html_text()
  
  compnames <- compnames[compnames != "|"]
  
  # get the links to the filing
  links <- 
    page %>%
    html_nodes(xpath = paste0("body/form/table[3]/tr/td[3]/table[4]/tr[2]/td/div/table[2]/tr/td[2]/a[contains(@id, 'viewFiling') and contains(@class, 'filing')]")) %>%
    html_attr("href") %>%
    str_extract("'http[^']*'") %>%
    str_replace_all("'","")
  
  # get the date
  date <- 
    page %>%
    html_nodes(xpath = paste0("body/form/table[3]/tr/td[3]/table[4]/tr[2]/td/div/table[2]/tr/td[1]/i")) %>%
    html_text("")

  # store and export as table  
  tibble(date = lubridate::mdy(date), compnames = compnames, link = links)

}

# run over our pages and combine
table <- map_dfr(pagestubs, read_by_page) %>% 
  rowwise() %>% 
  # add a name column, CIK column, and SIC column
  mutate(company_name = toupper(str_sub(compnames, 1, str_locate(compnames, " \\(CIK - ")[1] - 1)),
         CIK = str_sub(compnames, str_locate(compnames, "\\(CIK - ")[2] + 1, 
                       str_locate(compnames, " /SIC")[1] - 1),
         SIC = str_sub(compnames, str_locate(compnames, " /SIC - ")[2] + 1, 
                       str_locate(compnames, "\\)")[length(str_locate(compnames, "\\)"))] - 1)) %>% 
  ungroup() %>% 
  select(date, company_name, CIK, SIC, link)

# go through and save the htmls in a folder
for (i in 1:nrow(table)) {
  url <- table$link[i]
  webpage <- read_html(url)
  name <- paste(i, ". ", table$company_name[i], " ", table$date[i], ".html", sep = "")
  write_xml(webpage, file = here::here("HTML", name))
}

# do the same thing for form 12b-25
# first for 10ks
url <- paste("https://searchwww.sec.gov/EDGARFSClient/jsp/EDGAR_MainAccess.jsp?search_text=*&sort=ReverseDate&formType=FormNT10K&isAdv=true&stemming=true&numResults=100&fromDate=01/01/2020&toDate=",
             td,
             "&numResults=100",
             sep = "")

totalnum <- read_html(url) %>% html_node('#header .normal+ .normalbold') %>% html_text() %>% as.numeric()

# get the number of pages we need to loop through
pages <- floor(totalnum/100) + 1

# make list of starting entries 
pagestubs <- (1:pages)*100 + 1 - 100

# make function to read tables by page
read_by_page_10k <- function(p) {
  
  url <- paste("https://searchwww.sec.gov/EDGARFSClient/jsp/EDGAR_MainAccess.jsp?search_text=*&sort=ReverseDate&startDoc=",
               p,
               "&numResults=100&isAdv=true&formType=FormNT10K&fromDate=01/01/2020&toDate=",
               td,
               "&stemming=true",
               sep = "")
  
  # read the page
  page <- read_html(url)
  
  # get the company names
  compnames <- 
    page %>%
    html_nodes(xpath = paste0("body/form/table[3]/tr/td[3]/table[4]/tr[2]/td/div/table[2]/tr/td[2]/font[2]")) %>%
    html_text()
  
  compnames <- compnames[compnames != "|"]
  
  # get the links to the filing
  links <- 
    page %>%
    html_nodes(xpath = paste0("body/form/table[3]/tr/td[3]/table[4]/tr[2]/td/div/table[2]/tr/td[2]/a[contains(@id, 'viewFiling') and contains(@class, 'filing')]")) %>%
    html_attr("href") %>%
    str_extract("'http[^']*'") %>%
    str_replace_all("'","")
  
  # get the date
  date <- 
    page %>%
    html_nodes(xpath = paste0("body/form/table[3]/tr/td[3]/table[4]/tr[2]/td/div/table[2]/tr/td[1]/i")) %>%
    html_text("")
  
  # store and export as table  
  tibble(date = lubridate::mdy(date), compnames = compnames, link = links)
  
}

# run the table for the 10ks
table12_10k <- map_dfr(pagestubs, read_by_page_10k) %>% 
  rowwise() %>% 
  # add a name column, CIK column, and SIC column
  mutate(company_name = toupper(str_sub(compnames, 1, str_locate(compnames, " \\(CIK - ")[1] - 1)),
         CIK = str_sub(compnames, str_locate(compnames, "\\(CIK - ")[2] + 1, 
                       str_locate(compnames, " /SIC")[1] - 1),
         SIC = str_sub(compnames, str_locate(compnames, " /SIC - ")[2] + 1, 
                       str_locate(compnames, "\\)")[length(str_locate(compnames, "\\)"))] - 1)) %>% 
  ungroup() %>% 
  select(date, company_name, CIK, SIC, link)

# Now with 10Qs
url <- paste("https://searchwww.sec.gov/EDGARFSClient/jsp/EDGAR_MainAccess.jsp?search_text=*&sort=ReverseDate&formType=FormNT10Q&isAdv=true&stemming=true&numResults=100&fromDate=01/01/2020&toDate=",
             td,
             "&numResults=100",
             sep = "")

totalnum <- read_html(url) %>% html_node('#header .normal+ .normalbold') %>% html_text() %>% as.numeric()

# get the number of pages we need to loop through
pages <- floor(totalnum/100) + 1

# make list of starting entries 
pagestubs <- (1:pages)*100 + 1 - 100

# make function to read tables by page
read_by_page_10q <- function(p) {
  
  url <- paste("https://searchwww.sec.gov/EDGARFSClient/jsp/EDGAR_MainAccess.jsp?search_text=*&sort=ReverseDate&startDoc=",
               p,
               "&numResults=100&isAdv=true&formType=FormNT10Q&fromDate=01/01/2020&toDate=",
               td,
               "&stemming=true",
               sep = "")
  
  # read the page
  page <- read_html(url)
  
  # get the company names
  compnames <- 
    page %>%
    html_nodes(xpath = paste0("body/form/table[3]/tr/td[3]/table[4]/tr[2]/td/div/table[2]/tr/td[2]/font[2]")) %>%
    html_text()
  
  compnames <- compnames[compnames != "|"]
  
  # get the links to the filing
  links <- 
    page %>%
    html_nodes(xpath = paste0("body/form/table[3]/tr/td[3]/table[4]/tr[2]/td/div/table[2]/tr/td[2]/a[contains(@id, 'viewFiling') and contains(@class, 'filing')]")) %>%
    html_attr("href") %>%
    str_extract("'http[^']*'") %>%
    str_replace_all("'","")
  
  # get the date
  date <- 
    page %>%
    html_nodes(xpath = paste0("body/form/table[3]/tr/td[3]/table[4]/tr[2]/td/div/table[2]/tr/td[1]/i")) %>%
    html_text("")
  
  # store and export as table  
  tibble(date = lubridate::mdy(date), compnames = compnames, link = links)
  
}

# run the table for the 10ks
table12_10q <- map_dfr(pagestubs, read_by_page_10q) %>% 
  rowwise() %>% 
  # add a name column, CIK column, and SIC column
  mutate(company_name = toupper(str_sub(compnames, 1, str_locate(compnames, " \\(CIK - ")[1] - 1)),
         CIK = str_sub(compnames, str_locate(compnames, "\\(CIK - ")[2] + 1, 
                       str_locate(compnames, " /SIC")[1] - 1),
         SIC = str_sub(compnames, str_locate(compnames, " /SIC - ")[2] + 1, 
                       str_locate(compnames, "\\)")[length(str_locate(compnames, "\\)"))] - 1)) %>% 
  ungroup() %>% 
  select(date, company_name, CIK, SIC, link)


# combine the two tables
table12 <- bind_rows(table12_10q, table12_10k)

# add column with whether there was any type of Form 12b-25 during the year
find_form12 <- function(i) {
  firm <- table$CIK[i]
  file_date <- table$date[i]
  
  # flag if there was a 12 filing by the same CIK before the original filing date
  if_else(nrow(table12 %>% filter(CIK == firm & date < file_date)) > 0, 1, 0)
}

# add to the table
table <- table %>% mutate(prior_12b25 = map_dbl(1:nrow(table), find_form12))

# finally, add in the ticker
tickers <- read.delim("https://www.sec.gov/include/ticker.txt") %>% magrittr::set_colnames(c("ticker", "CIK"))

# function to get tickers by CIK
get_tic <- function(i) {
  tic <- tickers %>% 
    filter(CIK == table$CIK[i]) %>% 
    pull(ticker)
  
  if (identical(tic, character(0))) {
    NA
  } else {
    tic
  }
}

table <- table %>% 
  mutate(ticker = map(1:nrow(table), get_tic)) %>% 
  unnest_wider(ticker) %>% 
  magrittr::set_colnames(c(colnames(table)[1:6], 
                           paste("ticker", 1:3, sep = "_")))

table <- distinct(table)

table <- table %>% mutate(merge_ticker = coalesce(ticker_1, ticker_2, ticker_3))

# pull stock prices from merge_ticker

stocklist<-table[['merge_ticker']]
stocklist<-stocklist[!is.na(stocklist)]
as.numeric('stocklist')
#have to do some cleaning here as some symbols gen errors
stocklist <- stocklist %>% tq_get(get="stock.prices", from="2017-04-25", to="2020-04-25")
#Estimates of NP Var and ES (no Garch)

#Estimates of Parametric Var and ES (can then do Garch)

#GARCH vs. GARCH-E
