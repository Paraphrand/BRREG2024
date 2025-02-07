# load BRREG data in the Enhetsregister as of 4 Oct 2024

library(readxl)
library(dplyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(data.table)
library(lubridate)
library(explore)
library(forcats)
library(tidyr)
library(scales)
library(ggwordcloud)
library(lubridate)


rsthemes::rstheme_rainbow_parentheses(
ui_paren_0 = "#c03020",
ui_paren_1 = "#CFAA00",
ui_paren_2 = "orange2",
ui_paren_3 = "green4",
ui_paren_4 = "lightblue3",
ui_paren_5 = "#0022ff",
ui_paren_6 = "purple"
)

file <- "~/Downloads/Data/enheter_alle.xlsx"
brreg1 <- read_xlsx(path = file, sheet = 1, trim_ws = T, .name_repair = "universal",
                     col_types = c("text", "text", "text", "text", "text", "text",
                                   "text", "text", "text", "text", "text", "text",
                                   "numeric", "text", "text", "text", "text", 
                                   "text", "text", "text", "text", "text", "text", 
                                   "text", "text", "text", "text","text", "text",
                                   "text", "text", "text", "text", "text", "text", 
                                   "text", "text", "text", "text", "text", "text",
                                   "text", "text", "text", "text", "text", "text",
                                   "text", "text", "text", "text", "text", "text",
                                   "text", "text", "text", "text", "text", "text",
                                   "text", "text", "numeric", "text", "text", "text",
                                   "text"))
brreg2 <- read_excel(path = file, sheet = 2, trim_ws = T, .name_repair = "universal",
                     col_types = c("text", "text", "text", "text", "text", "text",
                                   "text", "text", "text", "text", "text", "text",
                                   "numeric", "text", "text", "text", "text", 
                                   "text", "text", "text", "text", "text", "text", 
                                   "text", "text", "text", "text","text", "text",
                                   "text", "text", "text", "text", "text", "text", 
                                   "text", "text", "text", "text", "text", "text",
                                   "text", "text", "text", "text", "text", "text",
                                   "text", "text", "text", "text", "text", "text",
                                   "text", "text", "text", "text", "text", "text",
                                   "text", "text", "numeric", "text", "text", "text",
                                   "text"))

brreg <- rbind2(brreg1,brreg2)
brreg <- janitor::clean_names(brreg,"snake")

organisasjonsformer <- data.frame(
  Organisasjonsform = c(
    "AAFY", "ADOS", "ANNA", "ANS", "AS", "ASA", "BA", "BBL", "BEDR", "BO",
    "BRL", "DA", "ENK", "EOFG", "ESEK", "FKF", "FLI", "FYLK", "GFS", "IKJP",
    "IKS", "KBO", "KF", "KIRK", "KOMM", "KS", "KTRF", "NUF", "OPMV", "ORGL",
    "PERS", "PK", "PRE", "SA", "SAM", "SE", "SF", "SPA", "STAT", "STI",
    "SÆR", "TVAM", "UTLA", "VPFO"
  ),
  Beskrivelse = c(
    "Underenhet til ikke-næringsdrivende", "Administrativ enhet -offentlig sektor", "Annen juridisk person",
    "Ansvarlig selskap med solidarisk ansvar", "Aksjeselskap", "Allmennaksjeselskap",
    "Selskap med begrenset ansvar", "Boligbyggelag", "Underenhet til næringsdrivende og offentlig forvaltning",
    "Andre bo", "Borettslag", "Ansvarlig selskap med delt ansvar", "Enkeltpersonforetak",
    "Europeisk økonomisk foretaksgruppe", "Eierseksjonssameie", "Fylkeskommunalt foretak",
    "Forening/lag/innretning", "Fylkeskommune", "Gjensidig forsikringsselskap",
    "Andre ikke-juridiske personer", "Interkommunalt selskap", "Konkursbo", "Kommunalt foretak",
    "Den norske kirke", "Kommune", "Kommandittselskap", "Kontorfellesskap",
    "Norskregistrert utenlandsk foretak", "Særskilt oppdelt enhet, jf. mval. § 2-2", "Organisasjonsledd",
    "Andre enkeltpersoner som registreres i tilknyttet register", "Pensjonskasse",
    "Partrederi", "Samvirkeforetak", "Tingsrettslig sameie", "Europeisk selskap",
    "Statsforetak", "Sparebank", "Staten", "Stiftelse", "Annet foretak iflg. særskilt lov",
    "Tvangsregistrert for MVA", "Utenlandsk enhet", "Verdipapirfond"
  ),
  Økonomisk_sektor = c(
    "Ideelle organisasjoner & foreninger", "Offentlig administrasjon & myndigheter", "Eiendom & bolig / Juridiske & profesjonelle tjenester",
    "Shipping & maritim / Industri & handel", "Industri & handel / Bank & finans", "Industri & handel / Bank & finans",
    "Detaljhandel & forbrukersamvirke", "Eiendom & bolig", "Industri & handel",
    "Annet / Juridiske & profesjonelle tjenester", "Eiendom & bolig", "Industri & handel", "Industri & handel",
    "Industri & handel", "Eiendom & bolig", "Offentlig administrasjon & myndigheter",
    "Religion & ideelle organisasjoner", "Offentlig administrasjon & myndigheter", "Forsikring",
    "Annet", "Offentlig administrasjon & myndigheter", "Juridiske & profesjonelle tjenester", "Offentlig administrasjon & myndigheter",
    "Religion & ideelle organisasjoner", "Offentlig administrasjon & myndigheter", "Industri & handel", "Juridiske & profesjonelle tjenester",
    "Industri & handel", "Beskatning & spesiell registrering", "Ideelle organisasjoner & foreninger",
    "Annet", "Bank & finans / Forsikring", "Shipping & Maritime", "Detaljhandel & forbrukersamvirke / Agriculture & Fisheries",
    "Juridiske & profesjonelle tjenester", "Industri & handel", "Energi & infrastruktur",
    "Bank & finans", "Offentlig administrasjon & myndigheter", "Religion & ideelle organisasjoner",
    "Annet", "Beskatning & spesiell registrering", "Annet", "Bank & finans"
  )
)


