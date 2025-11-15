library(PNADcIBGE)

pnad <- get_pnadc(year=2022, quarter=4)

ir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

saveRDS(pnad, file = "data/raw/pnad_2022_q4_raw.rds")

## Baixa todo o RAW da PNADC, não usar.