library(shiny)
library(plotly)
library(cowplot)
library(tidyverse)
library(scales)
library(ggthemes)
library(kableExtra)
library(gganimate)
library(lubridate)
library(geomtextpath)
library(ggtext)
library(here)
library(readxl)
library(janitor)
library(DT)
library(bslib)
library(thematic)


##### Data #####
# Ársreikningagögn
d <- read_csv("data/arsreikningagogn.csv") 



# Fasteignagjöld
fasteignamat <- read_excel("data/tafla-14-fasteignagjold-2022.xlsx", skip = 5) |> 
    slice(-1) |> 
    set_names(c("nr", "sveitarfelag", "utsvar", "fskattur_a", "fskattur_b", "fskattur_c", 
                "fraveitugjald", "vatnsgjald", "sorphreinsunargjald",
                "sorpeydingargjald", "lodaleiga_ibudir", "lodaleiga_fyrirtaeki", "fj_gjd")) |> 
    mutate(sveitarfelag = str_replace_all(sveitarfelag, "[0-9]+\\)", "") |> 
               str_replace_all("1 \\)", "") |> 
               str_squish()) |> 
    drop_na(nr) |> 
    select(-utsvar, -fskattur_b, -sorphreinsunargjald, -sorpeydingargjald, -fj_gjd, -lodaleiga_fyrirtaeki, -nr, -lodaleiga_ibudir) |> 
    rename("Íbúðarhúsnæði" = "fskattur_a", "Atvinnueignir" = "fskattur_c") |> 
    pivot_longer(c("Íbúðarhúsnæði", "Atvinnueignir"), names_to = "tegund_eigna", values_to = "fskattur") |> 
    mutate_at(
        vars(fskattur, fraveitugjald, vatnsgjald), 
        ~ ifelse(str_detect(., "kr/m2") | is.na(.), "0", as.character(.)) |> 
            str_replace(",,", "\\.") |> 
            parse_number()
    ) |> 
    mutate(fasteignamat = (fskattur + fraveitugjald + vatnsgjald) / 100) |> 
    select(sveitarfelag, tegund_eigna, fasteignamat)


# Kaupskrá fasteigna
kaupskra <- read_csv("data/kaupskra.csv")




##### Sidebar Info and Plot Captions #####
# This is pasted into the sidebar on each page
sidebar_info <- paste0(
    br(" "),
    br(" "),
    h5("Höfundur:"),
    p("Brynjólfur Gauti Guðrúnar Jónsson"),
    HTML("<a href='https://github.com/bgautijonsson/sveitarfelog_maelabord'> Kóði og gögn </a>")
)
# This is the caption for plots
caption <- "Mynd var fengin frá: https://www.bggj.is/sveitarfelog"

##### THEMES #####
# Making a light and dark theme in case I want to offer the option later
light <- bs_theme(bootswatch = "flatly", primary = "#08306b")
dark <- bs_theme(bootswatch = "superhero")
theme_set(theme_half_open(font_size = 12))
thematic_shiny()

##### List of all outcome variables #####
#  To be used by functions in utils.R

y_vars <- list(
    "Árafjöldi til niðurgreiðslu nettó skulda" = "timi_borga_skuldir",
    "Eiginfjárhlutfall" = "eiginfjarhlutfall",
    "Framlegð per íbúi (kjörtímabil í heild)" = "framlegd_per_ibui_kjortimabil",
    "Framlegð sem hlutfall af tekjum" = "framlegd_hlutf",
    "Framlegð sem hlutfall af tekjum (kjörtímabil í heild)" = "framlegd_hlutf_kjortimabil",
    "Handbært fé per íbúi" = "handbaert_fe_per_ibui",
    "Jöfnunarsjóðsframlög per íbúi" = "jofnunarsjodur_a_ibua",
    "Jöfnunarsjóðsframlög sem hlutfall af skatttekjum" = "hlutf_jofnunarsjods_skottum",
    "Launa- og launatengd gjöld per íbúi" = "launagjold_per_ibui",
    "Launa- og launatengd gjöld sem hlutfall af útgjöldum" = "launagjold_hlutf_gjold",
    "Nettó jöfnunarsjóðsframlög per íbúi" = "netto_jofnunarsjod_per_ibui",
    "Nettóskuldir sem hlutfall af tekjum" = "nettoskuldir_hlutf_tekjur",
    "Rekstrarniðurstaða per íbúi (kjörtímabil í heild)" = "rekstrarnidurstada_per_ibui_kjortimabil",
    "Rekstrarniðurstaða sem hlutfall af tekjum" = "rekstrarnidurstada_hlutf",
    "Rekstrarniðurstaða sem hlutfall af tekjum (kjörtímabil í heild)" = "rekstrarnidurstada_hlutf_kjortimabil",
    "Rekstrarniðurstaða undanfarinna 3 ára  sem hlutfall af tekjum" = "rekstur_3_ar_hlutf_tekjur",
    "Skuldir" = "heildarskuldir",
    "Skuldir per íbúi"  = "skuldir_per_ibui",
    "Skuldir sem hlutfall af tekjum" = "skuldir_hlutf_tekjur",
    "Skuldaaukning" = "skuldaaukning",
    "Skuldaaukning á kjörtímabili (leiðrétt fyrir verðbólgu)" = "skuldaaukning_2021",
    "Skuldahlutfall" = "skuldahlutfall",
    "Útsvar og fasteignaskattur per íbúi" = "skattur_a_ibua",
    "Veltufé frá rekstri sem hlutfall af tekjum" = "veltufe_hlutf_tekjur",
    "Veltufjárhlutfall" = "veltufjarhlutfall"
)

percent_vars <- c(
    "Eiginfjárhlutfall",
    "Framlegð sem hlutfall af tekjum",
    "Framlegð sem hlutfall af tekjum (kjörtímabil í heild)",
    "Jöfnunarsjóðsframlög sem hlutfall af skatttekjum",
    "Launa- og launatengd gjöld sem hlutfall af útgjöldum",
    "Nettóskuldir sem hlutfall af tekjum",
    "Rekstrarniðurstaða sem hlutfall af tekjum",
    "Rekstrarniðurstaða sem hlutfall af tekjum (kjörtímabil í heild)",
    "Rekstrarniðurstaða undanfarinna 3 ára  sem hlutfall af tekjum",
    "Skuldir sem hlutfall af tekjum",
    "Skuldaaukning",
    "Skuldaaukning á kjörtímabili (leiðrétt fyrir verðbólgu)",
    "Skuldahlutfall",
    "Veltufé frá rekstri sem hlutfall af tekjum",
    "Veltufjárhlutfall"
)
