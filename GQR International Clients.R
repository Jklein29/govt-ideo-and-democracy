#### GQR International Clients

### Load packages

library(tidyverse)
library(rvest)

### Scrape GQR

read_html("https://www.gqrr.com/clients#us-camp") %>% 
  html_nodes("#page-538e09e7e4b02065ca8a93c7 > div:nth-child(4)") %>% 
  html_text() %>% 
  strsplit(")") %>%
  lapply(function(x) {
    paste0(x, ")")
  }) -> clients

### Data wrangling

lapply(clients, function(x) {
  x <- x[-c(9, 19, 28, 31:36)]
}) %>% 
  unlist() -> clients

strsplit(as.character(clients[33]), "ia")%>% 
  lapply(function(x) {
    x[1] <- paste0(x[1], "ia")
    return(x)
  }) %>% 
  unlist() -> Serb_SA

 clients <- c(clients[-33], Serb_SA)
 
 ### Create international clients file

 write.csv(data.frame(Clients = clients), "GQR_International_Clients.csv")
 