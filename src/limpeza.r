library(dplyr)
library(readr)
library(stringr)

# --- Carregar PNADC raw ---
raw_path <- "data/raw/pnad_2022_q4_raw.rds"
if (!exists("pnad")) {
  if (file.exists(raw_path)) {
    message("Carregando PNADC raw de ", raw_path)
    pnad <- readRDS(raw_path)
  } else {
    stop("Arquivo raw não encontrado em: ", raw_path)
  }
}

# --- Extrair dataframe de variáveis ---
if (inherits(pnad, "svyrep.design")) {
  df <- pnad$variables
} else if (is.data.frame(pnad)) {
  df <- pnad
} else {
  stop("O objeto pnad não é dataframe nem svyrep.design")
}

# --- Criar pasta de saída ---
dir.create("data/clean", recursive = TRUE, showWarnings = FALSE)

# --- Seleção de variáveis ---
selected_vars <- c("UF","V2007","V2009","V3002","VD3004","VD4002","VD4015","VD4019","V4012")
present_vars <- intersect(selected_vars, names(df))
df_clean <- df %>% select(all_of(present_vars))

# --- Renomear variáveis ---
df_clean <- df_clean %>% rename(
  Sexo = V2007,
  Idade = V2009,
  Grau_instrucao = V3002,
  Anos_estudo = VD3004,
  Condicao_ocup = VD4002,
  Cat_ocup = VD4015,
  Rendimento = VD4019,
  Horas_trabalho = V4012
)

# --- Ajustes nos fatores já com labels ---
df_clean <- df_clean %>%
  mutate(
    # Apenas ocupados
    Condicao_ocup = as.character(Condicao_ocup),
    Rendimento = as.numeric(Rendimento),
    Idade = as.numeric(Idade),
    Horas_trabalho = as.numeric(Horas_trabalho)
  ) %>%
  filter(Condicao_ocup == "Pessoas ocupadas") %>%
  filter(!is.na(Rendimento) & Rendimento > 0) %>%
  filter(Idade >= 10 & Idade <= 100) %>%
  filter(!is.na(Horas_trabalho) & Horas_trabalho >= 0 & Horas_trabalho <= 120)

# --- Variáveis derivadas ---
df_clean <- df_clean %>%
  mutate(
    Faixa_Idade = case_when(
      Idade < 25 ~ "Jovem (0-24)",
      Idade >= 25 & Idade < 60 ~ "Adulto (25-59)",
      Idade >= 60 ~ "Idoso (60+)",
      TRUE ~ NA_character_
    ),
    Regiao = case_when(
      UF %in% c("Rondônia","Acre","Amazonas","Roraima","Pará","Amapá","Tocantins") ~ "Norte",
      UF %in% c("Maranhão","Piauí","Ceará","Rio Grande do Norte","Paraíba","Pernambuco","Alagoas","Sergipe","Bahia") ~ "Nordeste",
      UF %in% c("Minas Gerais","Espírito Santo","Rio de Janeiro","São Paulo") ~ "Sudeste",
      UF %in% c("Paraná","Santa Catarina","Rio Grande do Sul") ~ "Sul",
      UF %in% c("Mato Grosso","Mato Grosso do Sul","Goiás","Distrito Federal") ~ "Centro-Oeste",
      TRUE ~ NA_character_
    )
  )

# --- Salvar RDS e CSV ---
saveRDS(df_clean, "data/clean/pnad_2022_q4_clean.rds")
write_csv(df_clean, "data/clean/pnad_2022_q4_clean.csv")

# --- Mensagem ---
message("Limpeza concluída!")
message("Linhas: ", nrow(df_clean), ", Colunas: ", ncol(df_clean))
if(file.exists("data/clean/pnad_2022_q4_clean.rds")) message("RDS criado com sucesso!")
if(file.exists("data/clean/pnad_2022_q4_clean.csv")) message("CSV criado com sucesso!")
