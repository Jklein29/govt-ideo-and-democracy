#### Left Parties Project ####

#### Load Packages

library(XLConnect)
library(tidyverse)

#### Import data

# Freedom in the world

FIW_wb <- loadWorkbook("Aggregate Category and Subcategory Scores FIW2003-2018.xlsx")
FIW_sheets <- readWorksheet(FIW_wb, sheet = getSheets(FIW_wb))

# CSES Integrated Module Dataset

CSES_IMD <- read.csv(unz("cses_imd_csv.zip", "cses_imd.csv"))

# CSES Module 1

CSES_1 <- read.csv(unz("cses1_csv.zip", "cses1.csv"))

# CSES Module 2

CSES_2 <- read.csv(unz("cses2_csv.zip", "cses2.csv"))

# CSES Module 3

CSES_3 <- read.csv(unz("cses3_csv.zip", "cses3.csv"))

# CSES Module 4

CSES_4 <- read.csv(unz("cses4_csv.zip", "cses4.csv"))

#### Data wrangling

### Create single FIW dataset

## 2005-2017 data

# Create and reduce list into data frame

FIW_05_17_l <- lapply(FIW_sheets[14:2], function(x) {
  select(x, c(Country.Territory:CL.Rating, PR.Aggr, CL.Aggr:Total.Aggr))
})

reduce(FIW_05_17_l, left_join, by = "Country.Territory") %>% 
  mutate(Country.Territory = trimws(Country.Territory)) -> FIW_05_17

# Edit names

gsub(".x", "", names(FIW_05_17)[-1]) %>%
  gsub(".y", "", .) %>% 
  paste(., rep(as.numeric(substr(names(FIW_05_17_l), 4, 7)) - 1, each = 6), 
        sep = "_") -> names(FIW_05_17)[-1]

## Add 2002-2004 data

# Extract data

FIW_02_04 <- as.data.frame(FIW_sheets[15])[, c(1, 2, 5, 3, 6, 4, 7)]
FIW_02_04[, -1] <- sapply(FIW_02_04[, -1], as.numeric)

# Edit names

gsub("FIW2003.", "", names(FIW_02_04)) %>% 
  gsub("FIW2004.", "", .) %>% 
  gsub("FIW2005.", "", .) -> names(FIW_02_04)
names(FIW_02_04)[-1] <- paste(names(FIW_02_04)[-1], 
                              rep(c(2002, 2003, 2004), each = 2), sep = "_")

# Create 2002-2004 PR Rating

sapply(select(FIW_02_04, starts_with("PR.Aggr")), function(x) {
  case_when(x < 6 ~ 7, x >= 6 & x < 12 ~ 6, x >= 12 & x < 18 ~ 5, 
            x >= 18 & x < 24 ~ 4, x >= 24 & x < 30 ~ 3, 
            x >= 30 & x < 36 ~ 2, x >= 36 ~ 1)
}) %>% 
  as.data.frame() -> PR_rt
names(PR_rt) <- c("PR.Rating_2002", "PR.Rating_2003", "PR.Rating_2004")

# Create 2002-2004 CL Rating

sapply(select(FIW_02_04, starts_with("CL.Aggr")), function(x) {
  case_when(x < 8 ~ 7, x >= 8 & x < 17 ~ 6, x >= 17 & x < 26 ~ 5, 
            x >= 26 & x < 35 ~ 4, x >= 35 & x < 44 ~ 3, 
            x >= 44 & x < 53 ~ 2, x >= 53 ~ 1)
}) %>% 
  as.data.frame() -> CL_rt
names(CL_rt) <- c("CL.Rating_2002", "CL.Rating_2003", "CL.Rating_2004")

# Create 2002-2004 Status

rating <- cbind(PR_rt, CL_rt)[order(c(seq_along(PR_rt), seq_along(CL_rt)))]

tapply(as.list(rating), gl(ncol(rating)/2, 2), as.data.frame) %>% 
  sapply(function(x) {
    rowMeans(x, na.rm = T) %>% 
      sapply(function(x) {
        case_when(x < 3 ~ "F", x >= 3 & x < 5.5 ~ "PF", 
                  x >= 5.5 ~ "NF")
      })
    }) %>% 
  as.data.frame() -> status
names(status) <- c("Status_2002", "Status_2003", "Status_2004")

# Create full 2002-2004 dataset

cbind(status, PR_rt, CL_rt)[order(c(seq_along(status), seq_along(PR_rt), seq_along(CL_rt)))] %>% 
  cbind(select(FIW_02_04, Country.Territory), .) %>% 
  mutate(Country.Territory = trimws(Country.Territory)) -> FIW_02_04_new

# Edit country names to match for join

FIW_02_04_new$Country.Territory[FIW_02_04_new$Country.Territory == "Israeli-Occupied Territories*"] <- 
  "Israeli Occupied Territories*"
FIW_02_04_new$Country.Territory[FIW_02_04_new$Country.Territory == "Palestinian Authority-Administered Territories*"] <- 
  "Palestinian Authority Administered Territories*"
FIW_05_17$Country.Territory[FIW_05_17$Country.Territory == "Transnistria"] <- 
  "Transnistria*"

# Join into full FIW data frame & export

FIW <- full_join(FIW_02_04_new, FIW_05_17)

write.csv(FIW, "FIW_full_calculated.csv")
