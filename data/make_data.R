library(DBI)
library(config)
library(tidyverse)
library(lubridate)
library(janitor)
library(feather)
library(metill)


init_db()


d <- mtl_sveitarfelog_arsreikningar() |> 
    collect()


d |> 
    write_feather(
        "data/arsreikningagogn.feather"
    )


d |> 
    write_csv(
        "data/arsreikningagogn.csv"
    )
