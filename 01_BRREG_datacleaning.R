# install.packages(c('summarytools',"gt","gtExtras","hrbrthemes","ggdark"),repos = 'https://cloud.r-project.org')
# library(summarytools)
# library(gtExtras)
# library(hrbrthemes)
# library(ggdark)
# library(pander)
# library(httr2)
# library(dplyr)
# library(clock)
# library(magrittr)
# library(skimr)
# library(DT)
# library(ggplot2)

brreg <- brreg |> #filtering columns, keeping those with the highest share(s) of complete observations
  select(-registrert_i_frivillighetsregisteret,
         -registreringsdato_i_frivillighetsregisteret,
         -registrert_i_partiregisteret,
         -registreringsdato_i_partiregisteret,
         -naeringskode_2,-naeringskode_2_beskrivelse,
         -naeringskode_3,-naeringskode_3_beskrivelse,
         -hjelpeenhetskode,
         -hjelpeenhetskode_beskrivelse,
         -overordnet_enhet_i_offentlig_sektor,
         -dato_frivillig_registrert_i_mva_registeret,
         -frivillig_registrert_i_mvaregisteret,
         -hjemmeside,
         -telefonnummer,
         -mobil,
         -epostadresse,
         -registreringsdato_i_foretaksregisteret
         )

date_variables <- ifelse(stringr::str_like(string = names(brreg[,1:48]), pattern = ".*dato.*"), TRUE, FALSE)
date_col <- names(brreg[,date_variables])
temp <- names(brreg)

brreg <- brreg |> 
  mutate(across(.cols = date_col, .fns = ymd)
  )

skimr::skim(brreg) -> skim_summary
skim_summary <- skim_summary |> arrange(match(skim_variable,temp))
datatable(skim_summary, options = list(pageLength = 48), rownames = FALSE)

brreg <- brreg[-695090,]

brreg <- brreg |>
  # mutate(siste_innsendte_arsregnskap = paste(siste_innsendte_arsregnskap,"01-01",sep = "")
  #        ) |>
  # mutate(siste_innsendte_arsregnskap = lubridate::ymd(siste_innsendte_arsregnskap)
  #        ) |>
  mutate(siste_innsendte_arsregnskap = lubridate::floor_date(siste_innsendte_arsregnskap,"year")
         )

# class(brreg$siste_innsendte_arsregnskap)

brreg <- brreg |> select(
  c(-"stiftelsesdato", #filtering columns, dropping those with the highest share(s) of complete observations
  -"registreringsdato_mva_registeret_i_enhetsregisteret",
  -"registreringsdato_i_mva_registeret",
  -"postadresse_adresse",
  -"postadresse_poststed",
  -"postadresse_postnummer",
  -"postadresse_kommune",
  -"postadresse_kommunenummer",
  -"postadresse_land",
  -"postadresse_landkode",
  -"registreringsdato_antall_ansatte_i_enhetsregisteret")
  )

report_corr <- brreg[,c(17,3)]

DataExplorer::plot_correlation(na.omit(report_corr), maxcat = 40L)

rm(skim_summary)
skimr::skim(brreg) -> skim_summary
skim_summary <- skim_summary |> arrange(match(skim_variable,temp))
datatable(skim_summary, options = list(pageLength = 48), rownames = FALSE)

install.packages("writexl")

sektor_count <- table(report_corr)
sektor_countf <- ftable(report_corr)
sektor_count <- as.data.frame(sektor_count)
sektor_count <- pivot_wider(sektor_count, names_from = c("institusjonell_sektorkode","organisasjonsform_kode"), values_from = "Freq")
writexl::write_xlsx(sektor_count)


`%nin%` <- Negate(`%in%`)

ENK4900 <- which(brreg$institusjonell_sektorkode == "4900" & brreg$organisasjonsform_kode == "ENK")
brreg[ENK4900,c("navn","institusjonell_sektorkode","organisasjonsform_kode")]

brreg |> 
  select(registreringsdato_i_enhetsregisteret) |> 
  mutate(year = year(registreringsdato_i_enhetsregisteret)) |> 
  select(year) |> 
  group_by(year) |> 
  count(.drop = T) |> 
  ungroup() |> 
  ggplot(aes(ordered(year),n, group = 1)) +
  geom_line(color = "#22FF00") +
  dark_theme_minimal() +
  theme(
    #   scale_x_continuous(breaks = seq.Date(from = as.Date("1995-01-01"), 
    #                                        to = as.Date("2024-01-01") , 
    #                                        by = "1 year"), 
    #                labels = 1995:2024),
    axis.text.x = element_text(angle = 90, vjust = 0.4, size = 16),
    axis.text.y = element_text(size = 18),
    title = element_text(size = 24),
    subtitle = element_text(size = 18),
    caption = element_text(size = 12),
    legend.position = "none") +
  labs(
    legend = "",
    x = "",
    y = "Entities registered during the year",
    title = "How many legal entities are registered in Norway?",
    subtitle = "February 1995 - November 2024",
    caption = "Source: Brønnøysundsregistrene"
  )



source("gt_summarytools.R")

brreg |> 
  select(registreringsdato_i_enhetsregisteret,
         siste_innsendte_arsregnskap) |> 
  mutate(registreringsdato_i_enhetsregisteret = lubridate::floor_date(registreringsdato_i_enhetsregisteret,unit = "year")) |> 
  gt_summarytools(title = "Brønnøysundsregistrene_okt2024")

brreg |> # find firms who have never submitted annual reports
  filter(siste_innsendte_arsregnskap %nin% seq(as.Date("1997-01-01"),
                                               as.Date("2024-01-01"),
                                               "1 year"),
         (organisasjonsform_kode == "ENK" & institusjonell_sektorkode == "4900" &
         registreringsdato_i_enhetsregisteret %nin% seq(as.Date("2024-01-01"),
                                                        as.Date("2024-12-31"),
                                                        "1 day"))) |> 
  select(organisasjonsnummer, aktivitet, naeringskode_1) |> 
  group_by(organisasjonsnummer,aktivitet,naeringskode_1) |> 
  summarise(n = n()) |> 
  print(n=165)

# registreringsdato_i_enhetsregisteret) |> 
  # mutate(registreringsdato_i_enhetsregisteret = lubridate::floor_date(registreringsdato_i_enhetsregisteret,"year")) |> 
