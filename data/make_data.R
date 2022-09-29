library(DBI)
library(config)
library(tidyverse)
library(lubridate)
library(janitor)
library(feather)


usr <- config::get("postgres_user")
con <- dbConnect(RPostgres::Postgres(), 
                 dbname = usr$dbname, 
                 host = usr$host,
                 port = usr$port, 
                 user = usr$username, 
                 password = usr$password)


d <- tbl(con, "sveitarfelog_arsreikningar") |> 
    collect()


d |> 
    write_feather(
        "data/arsreikningagogn.feather"
    )
