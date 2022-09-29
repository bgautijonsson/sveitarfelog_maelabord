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
library(shinycssloaders)
library(feather)
library(shinyWidgets)
library(metill)

shinyOptions(plot.autocolor = TRUE)

##### Data #####
# Ársreikningagögn
d <- read_feather("data/arsreikningagogn.feather") 


##### Sidebar Info and Plot Captions #####
# This is pasted into the sidebar on each page
sidebar_info <- paste0(
    br(" "),
    h5("Höfundur:"),
    p("Brynjólfur Gauti Guðrúnar Jónsson"),
    HTML("<a href='https://github.com/bgautijonsson/sveitarfelog_maelabord' target='_top'> Kóði og gögn </a><br>")
)
# This is the caption for plots
caption <- ""

##### THEMES #####
# Making a light and dark theme in case I want to offer the option later
theme_set(
    theme_metill() +
        theme(legend.text = element_text(size = 11),
              legend.spacing = unit(0.001, units = "npc"),
              legend.key.width = unit(0.001, units = "npc"))
    )



bs_global_theme(
    bootswatch = "flatly"
)

bs_global_add_variables(
    primary = "#484D6D",
    secondary = "#969696",
    success = "#969696",
    # danger = "#FF8CC6",
    # info = "#FF8CC6",
    light = "#faf9f9",
    dark = "#484D6D",
    bg = "#faf9f9",
    fg = "#737373",
    "body-bg" = "#faf9f9",
    base_font = "Lato",
    heading_font = "Segoe UI",
    "navbar-brand-font-family" = "Playfair Display",
    code_font = "SFMono-Regular"
)

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
