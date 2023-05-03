rm(list=ls())
library(usethis)

load('data-raw/transects/TRANSECT_DEFINITIONS.RData')
load('data-raw/transects/gullyNewTransectDefinition.RData')
load('data-raw/transects/sableIslandBankNewTransectDefinition.RData')
load('data-raw/transects/sableIslandBankWestEastTransectDefinition.RData')

# first re-name the list
transectDefinitions <- TRANSECT_DEFINITIONS
# names of each list
names(transectDefinitions) <- tolower(names(transectDefinitions))

# names of each element for each transect,
# and elements of elements
for(it in 1:length(transectDefinitions)){
  names(transectDefinitions[[it]]) <- tolower(names(transectDefinitions[[it]]))
  for(itt in 1:length(transectDefinitions[[it]])){
    names(transectDefinitions[[it]][[itt]]) <- tolower(names(transectDefinitions[[it]][[itt]]))
  }
}

# re-name lines based on my definitions and some renaming of lines
okbq <- which(names(transectDefinitions) == 'banquereau')
names(transectDefinitions)[okbq] <- 'laurentianChannelMouth'
okyl <- which(names(transectDefinitions) == 'yarmouth_line')
names(transectDefinitions)[okyl] <- 'yarmouth'
okpm <- which(names(transectDefinitions) == 'portsmouth_line')
names(transectDefinitions)[okpm] <- 'portsmouth'
okpe <- which(names(transectDefinitions) == 'portsmouth_line_extended')
names(transectDefinitions)[okpe] <- 'portsmouthExtended'
okbb <- which(names(transectDefinitions) == 'browns_bank')
names(transectDefinitions)[okbb] <- 'brownsBank'
oknec <- which(names(transectDefinitions) == 'ps_line')
names(transectDefinitions)[oknec] <- 'northEastChannel'
okhl <- which(names(transectDefinitions) == 'hfxline_inshore')
names(transectDefinitions)[okhl] <- 'halifaxInshore'
okhe <- which(names(transectDefinitions) == 'hfxline_extended')
names(transectDefinitions)[okhe] <- 'halifaxExtended'
# louisbourg is OK
okcs <- which(names(transectDefinitions) == 'cabot_strait')
names(transectDefinitions)[okcs] <- 'cabotStrait'
oksa <- which(names(transectDefinitions) == 'st_anns')
names(transectDefinitions)[oksa] <- 'stAnnsBank'
oksp <- which(names(transectDefinitions) == 'stpierre_bank')
names(transectDefinitions)[oksp] <- 'stPierreBank'
oksi <- which(names(transectDefinitions) == 'sableisland_bank')
names(transectDefinitions)[oksi] <- 'sableIslandBank'
# add gully
gtd <- list(gullyTransectDef)
names(gtd) <- 'theGullyNew'
# sable island bank new
sibn <- list(sableIslandBankNewTransectDef)
names(sibn) <- 'sableIslandBankNew'
# sable island bank west east
sibwe <- list(sableIslandBankWestEastTransectDef)
names(sibwe) <- 'sableIslandBankWestEast'

transectDefinitions <- c(transectDefinitions, gtd, sibn, sibwe)


usethis::use_data(transectDefinitions, compress = 'xz', overwrite = TRUE)
