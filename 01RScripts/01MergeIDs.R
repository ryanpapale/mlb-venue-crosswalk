# This script is included to show how the crosswalk was derived. Because the
# names were not always exact I had to fix several by hand. I have included
# this file for completeness and to show the data sources.

# Data retrieved from the public MLB Stats API.
# Retrosheet data courtesy of Retrosheet (https://www.retrosheet.org).


library("data.table")
library("httr2")
library("fuzzyjoin")
library("stringr")

#Get Park Info from MLB & Manually Rename Parks
mlb <- request("https://statsapi.mlb.com/api/v1/venues/") |>
#  req_url_query(sportId = 1) |>
  req_perform() |>
  resp_body_json(simplifyVector = TRUE, flatten = TRUE) |>
  _[["venues"]] |>
  as.data.table() |>
  _[, !c("link")
  ][, lower_name := tolower(str_replace_all(name, " ", ""))
  ][lower_name == "globelifefield", 
    lower_name := "globelifefieldinarlington"
  ][lower_name == "choctawstadium", 
    lower_name := "rangersballparkinarlington"
  ][lower_name == "robertf.kennedymemorialstadium", 
    lower_name := "robertf.kennedystadium"
  ][lower_name == "bb&tcenter", 
    lower_name := "bb&tballparkatbowmanfield"
  ][lower_name == "hardrockstadium", 
    lower_name := "sunlifestadium"
  ][lower_name == "clevelandmunicipalstadium", 
    lower_name := "clevelandstadium"
  ][lower_name == "3comparkatcandlestickpoint", 
    lower_name := "candlestickpark"
  ][lower_name == "thestadiumattheespnwideworldofsports", 
    lower_name := "theballparkatdisney'swideworld"
  ][lower_name == "walmartpark", 
    lower_name := "estadiomonterrey"
  ][lower_name == "hirambithornstadium", 
    lower_name := "estadiohirambithorn"
  ][lower_name == "daikin", 
    lower_name := "minutemaidpark"
  ][lower_name == "loandepotpark", 
    lower_name := "marlinspark"
  ][lower_name == "milwaukeecountystadium", 
    lower_name := "countystadium"
  ][lower_name == "yankeestadium", 
    lower_name := "yankeestadiumii"
  ][lower_name == "t-mobilepark", 
    lower_name := "safecofield"
  ][lower_name == "at&tfield", 
    lower_name := "at&tpark"
  ][lower_name == "buschstadium", 
    lower_name := "buschstadiumiii"
  ][lower_name == "angelstadium", 
    lower_name := "angelstadiumofanaheim"
  ][lower_name == "ratefield", 
    lower_name := "guaranteedratefield;u.s.cellularfield"
  ][lower_name == "olympicstadium", 
    lower_name := "stadeolympique"
  ][lower_name == "americanfamilyfield", 
    lower_name := "millerpark"]

#Get Park Info from Retrosheet (https://www.retrosheet.org/parkcode.txt)
retro <- fread("https://www.retrosheet.org/parkcode.txt") |>
  as.data.table() |>
  setnames(c("PARKID", "NAME", "AKA", "END"), 
           c("id", "name", "aka", "end")) |>
  _[, .(id, name, aka, end)
  ][, end := fifelse(end == "", "01/01/2025", end)
  ][as.IDate("1980-01-01") <= as.IDate(end, format = "%m/%d/%Y"),
  ][, lower_name := tolower(str_replace_all(name, " ", ""))] |>
  rbind(data.table("SAC01", "Sutter Health Park", 
                   "", "", "sutterhealthpark"),
        data.table("BST01", "Bristol Motor Speedway", 
                   "", "", "bristolmotorspeedway"),
        data.table("TAM01", "George M. Steinbrenner Field", 
                   "", "", "georgem.steinbrennerfield"), use.names = FALSE)

merge <- stringdist_join(retro, mlb, by = "lower_name", max_dist = 5, 
                         mode = "left", distance_col = "dist") |>
  as.data.table() |>
  setorder("id.x", "dist") |>
  _[, .SD[which.min(dist)], by = id.x
  ][, .(id.x, name.x, id.y, name.y)] |>
  setnames(c("id.x", "name.x", "id.y", "name.y"), 
           c("retrosheet_id", "retrosheet_venue", "mlb_id", "mlb_venue"))

fwrite(merge, "../02Data/VenueCrosswalk.csv")
