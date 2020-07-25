library(rvest)
library(curl)
library(tidyverse)
library(httr)

x <- "https://www.basketball-reference.com/boxscores/pbp/201905190TOR.html"
df <- read_html(curl(x, handle = curl::new_handle("useragent" = "Mozilla/5.0"))) %>% 
            html_nodes("table") %>% 
            html_table() %>%
            .[[1]] %>%
            setNames(paste0(colnames(.), as.character(.[1,]))) %>% 
            slice(-1)
  
away_tm <- df[1,2]
home_tm <- df[1,6]
date <- str_extract(string = x, pattern = "(?<=pbp\\/)\\d{8}")
year <- str_extract(string = x, pattern = "(?<=pbp\\/)\\d{4}")
month <- str_extract(string = x, pattern = "(?<=pbp\\/\\d{4})\\d{2}")
day <- str_extract(string = x, pattern = "(?<=pbp\\/\\d{6})\\d{2}")

df <- df %>%
  mutate(away_team = away_tm,
         home_team = home_tm,
         date = date,
         year = year,
         month = month,
         day = day)

#that's a good script to loop. come back to this later. now the task is to build urls to rip through

# get all teams 3 digit abreviation
  
tm_abr <- c("ATL",
            "BOS",
            "NJN",
            "CHA",
            "CHI",
            "CLE",
            "DAL",
            "DEN",
            "DET",
            "GSW",
            "HOU",
            "IND",
            "LAC",
            "LAL",
            "MEM",
            "MIA",
            "MIL",
            "MIN",
            "NOH",
            "NYK",
            "OKC",
            "ORL",
            "PHI",
            "PHO",
            "POR",
            "SAC",
            "SAS",
            "TOR",
            "UTA",
            "WAS"
            )

#MAP team names into urls (let's focus on 2019 so dont bracket that)

url <- "https://www.basketball-reference.com/boxscores/pbp/1996{month}{date}0{home_team}.html"
url2 <- map(.x = c(tm_abr),
            .f = function(x){gsub(x = url, pattern = "\\{home_team\\}", replacement = x)}) %>% 
  unlist

days <- c("01","02","03","04","05","06","07","08","09",
          "10","11","12","13","14","15","16","17","18",
          "19","20","21","22","23","24","25","26","27",
          "28","29","30","31")

url3 <- map(.x = c(days),
                    .f = function(x){gsub(x = url2, pattern = "\\{date\\}", replacement = x)}) %>% 
  unlist

mnth <- c("01","02","03","04","05","06","07","08","09",
          "10","11","12")

url4 <- map(.x = c(mnth),
            .f = function(x){gsub(x = url3, pattern = "\\{month\\}", replacement = x)}) %>% 
  unlist

#ok that's it for 2019 - 11k possible combos now need to find out

#blocked the code below as I already ran - takes long time and saved as RDS
check_link <- sapply(url4, url_success) %>%
  as.data.frame() 

#NOTE - this has the version w too many (28k) bc I used days twice instead of month in url4
saveRDS(check_link, "check_links.rds")

#this should give me the row numbers for all links in that 28k pile with matches
#for 2019 let's reduce to playoffs only while figuring this all out

link_hit_index <- which(check_link == TRUE)

#now reduce to only links that are live

url_1996 <- url4[link_hit_index]

#ok now map the links with the code and rip it!

#come back to code from start

ugh <- read_html('https://www.basketball-reference.com/boxscores/pbp/199611010DEN.html') %>%
  html_nodes("#table2")

nba_1996a <- map_dfr(.x = url_1996[1:3],
                    .f = function(x){Sys.sleep(2); cat(1); 
                     df <- read_html(curl(x, handle = curl::new_handle("useragent" = "Mozilla/5.0"))) %>% 
                       html_nodes("table") %>% 
                       html_table(fill = T) %>%
                       .[[1]] %>%
                       setNames(paste0(c("time","away_play","away_score",
                                         "combined_Score","home_score", "home_play")))

  away_tm <- df[1,2]
  home_tm <- df[1,6]

df$away_team <- away_tm
df$home_team <- home_tm
df$date <- str_extract(string = x, pattern = "(?<=pbp\\/)\\d{8}")
df$year <- str_extract(string = x, pattern = "(?<=pbp\\/)\\d{4}")
df$month <- str_extract(string = x, pattern = "(?<=pbp\\/\\d{4})\\d{2}")
df$day <- str_extract(string = x, pattern = "(?<=pbp\\/\\d{6})\\d{2}")
df$playid <- c(1:nrow(df))
df$gameid <- paste0(df$away_team,df$home_team,df$date)
df$gameid <- str_replace_all(df$gameid, " ", "")

df 

})

not_all_na <- function(x) any(!is.na(x))

why <- nba_1996a %>%
  select_if(not_all_na)


saveRDS(nba_2019, "nba_pbp_2019.rds")





