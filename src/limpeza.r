library(dplyr)
library(readr)
library(stringr)
library(haven)  # Para lidar com labelled

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
selected_vars <- c("UF","V2007","V2009","VD3004","VD3006","VD4002","VD4011","VD4017","VD4013",
                   "UPA", "Estrato", "V1032")
present_vars <- intersect(selected_vars, names(df))
df_clean <- df %>% select(all_of(present_vars))

# --- Renomear variáveis ---
df_clean <- df_clean %>% rename(
  Sexo = V2007,
  Idade = V2009,
  Grau_instrucao = VD3004,
  Anos_estudo = VD3006,
  Condicao_ocup = VD4002,
  Cat_ocup = VD4011,
  Rendimento = VD4017,
  Horas_trabalho_semanais = VD4013
)

# --- Ajustes de tipos ---
df_clean <- df_clean %>%
  mutate(
    # Converte labelled para factor, depois character
    Grau_instrucao = as.character(as_factor(Grau_instrucao)),
    Condicao_ocup = as.character(as_factor(Condicao_ocup)),
    Cat_ocup = as.character(as_factor(Cat_ocup)),
    Sexo = as.character(as_factor(Sexo)),
    UF = as.character(as_factor(UF)),
    
    # Numéricas
    Idade = as.numeric(Idade),
    Anos_estudo = as.numeric(Anos_estudo),
    Rendimento = as.numeric(Rendimento),
    Horas_trabalho_semanais = as.numeric(Horas_trabalho_semanais)
  ) %>%
  # Filtros plausíveis
  filter(!is.na(Rendimento) & Rendimento > 0) %>%
  filter(Idade >= 10 & Idade <= 100) %>%
  filter(!is.na(Horas_trabalho_semanais) & Horas_trabalho_semanais >= 0 & Horas_trabalho_semanais <= 120)

# --- Variáveis derivadas e dados a partir dos códigos---
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
    ),
    Faixa_Anos_Estudo = case_when(
      Anos_estudo == 1 ~ "Menos de 1 ano de estudo ou sem instrução",
      Anos_estudo == 2 ~ "1 a 4 anos de estudo",
      Anos_estudo == 3 ~ "5 a 8 anos de estudo",
      Anos_estudo == 4 ~ "9 a 11 anos de estudo",
      Anos_estudo == 5 ~ "12 a 15 anos de estudo",
      Anos_estudo == 6 ~ "16 ou mais anos de estudo",
      TRUE ~ NA_character_
    ),
    Faixa_Horas_Trabalho = case_when(
      Horas_trabalho_semanais == 1 ~ "Até 14 horas",
      Horas_trabalho_semanais == 2 ~ "15 a 39 horas",
      Horas_trabalho_semanais == 3 ~ "40 a 44 horas",
      Horas_trabalho_semanais == 4 ~ "45 a 48 horas",
      Horas_trabalho_semanais == 5 ~ "49 horas ou mais",
      TRUE ~ NA_character_
    )
  )

# --- Winsorizar Rendimento ---
if ("Rendimento" %in% names(df_clean)) {
  qnts <- quantile(df_clean$Rendimento, probs = c(0.01, 0.99), na.rm = TRUE)
  p1 <- qnts[1]; p99 <- qnts[2]
  df_clean <- df_clean %>%
    mutate(Rendimento = pmin(pmax(Rendimento, p1), p99))
  message("Rendimento winsorizado com p1=", round(p1,2), " e p99=", round(p99,2))
}

# --- Salvar RDS e CSV ---
saveRDS(df_clean, "data/clean/pnad_2022_q4_clean.rds")
write_csv(df_clean, "data/clean/pnad_2022_q4_clean.csv")

# --- Mensagem final ---
message("Limpeza concluída!")
message("Linhas: ", nrow(df_clean), ", Colunas: ", ncol(df_clean))
if(file.exists("data/clean/pnad_2022_q4_clean.rds")) message("RDS criado com sucesso!")
if(file.exists("data/clean/pnad_2022_q4_clean.csv")) message("CSV criado com sucesso!")
