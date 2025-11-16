library(survey)
library(dplyr)

df_clean <- readRDS("data/clean/pnad_2022_q4_clean.rds")

# --- Função de verificação do raw PNADC ---
check_raw_data <- function(pnad, selected_vars = NULL, max_levels = 20) {
  # Extrair dataframe
  if (inherits(pnad, "svyrep.design")) {
    df <- pnad$variables
    if (is.null(df)) df <- getdata(pnad)  # fallback
  } else if (is.data.frame(pnad)) {
    df <- pnad
  } else {
    stop("O objeto pnad não é svyrep.design nem data.frame")
  }
  
  # Converter para data.frame caso necessário
  df <- as.data.frame(df)
  
  cat("Dataframe extraído!\n")
  cat("Linhas:", nrow(df), " Colunas:", ncol(df), "\n\n")
  
  if (!is.null(selected_vars)) {
    for (var in selected_vars) {
      cat("====================\n")
      cat("Variável:", var, "\n")
      if (!var %in% names(df)) {
        cat(">>> Variável não existe no dataframe!\n")
        next
      }
      x <- df[[var]]
      cat("Tipo:", class(x), "\n")
      if (is.factor(x) || is.character(x)) {
        vals <- unique(x)
        cat("Valores únicos (até", max_levels, "):", paste(head(vals, max_levels), collapse = ", "), "\n")
      } else if (is.numeric(x)) {
        print(summary(x))
      }
    }
  }
  
  return(df)
}

# --- Uso ---
raw_data <- readRDS("data/raw/pnad_2022_q4_raw.rds")
#df <- check_raw_data(raw_data, selected_vars = c("UF","V2007","V2009","V3002","VD3004","VD4002","VD4015","VD4019","V4012"))


# --- Verifica se o dataframe existe e não está vazio ---
check_df <- function(df) {
  if (!exists("df")) {
    message("O dataframe 'df' não existe.")
    return(FALSE)
  }
  if (nrow(df) == 0) {
    message("O dataframe existe mas está vazio!")
    return(FALSE)
  }
  message("Dataframe existe e possui ", nrow(df), " linhas e ", ncol(df), " colunas.")
  return(TRUE)
}

# --- Lista as variáveis disponíveis ---
list_vars <- function(df) {
  if (!check_df(df)) return(NULL)
  message("Variáveis disponíveis:")
  print(names(df))
}

summarize_all_vars <- function(data, max_categories = 10, top_n = 5) {
  
  # Inicializa uma lista para armazenar os resumos de cada variável
  all_summaries <- list()
  
  # Itera sobre cada coluna do dataframe
  for (col_name in names(data)) {
    
    col <- data[[col_name]]
    n_na <- sum(is.na(col))
    n_unique <- length(unique(col))
    
    # ----------------------------------------------------
    # 1. Variáveis Numéricas (Numeric ou Integer)
    # ----------------------------------------------------
    if (is.numeric(col) || is.integer(col)) {
      
      # Remove NAs para calcular estatísticas descritivas
      col_clean <- na.omit(col)
      
      summary_data <- list(
        Type = ifelse(is.integer(col), "Integer/Numerical (High Cardinality)", "Numerical"),
        Count = length(col),
        Missing = n_na,
        Unique_Values = n_unique,
        Min = if (length(col_clean) > 0) min(col_clean) else NA,
        Q1 = if (length(col_clean) > 0) quantile(col_clean, 0.25) else NA,
        Median = if (length(col_clean) > 0) median(col_clean) else NA,
        Mean = if (length(col_clean) > 0) mean(col_clean) else NA,
        Q3 = if (length(col_clean) > 0) quantile(col_clean, 0.75) else NA,
        Max = if (length(col_clean) > 0) max(col_clean) else NA,
        SD = if (length(col_clean) > 1) sd(col_clean) else NA,
        Top_Values = NA # Não se aplica a numéricas, mantido por consistência
      )
      
    # ----------------------------------------------------
    # 2. Variáveis Categóricas (Factor, Character ou Fator com poucas categorias)
    # ----------------------------------------------------
    } else if (is.factor(col) || is.character(col) || n_unique <= max_categories) {
      
      # Calcula a frequência dos top_n valores mais comuns
      top_counts <- head(sort(table(col), decreasing = TRUE), top_n)
      total_clean <- length(na.omit(col))
      
      top_summary <- paste(
        names(top_counts), 
        " (", 
        top_counts, 
        "; ", 
        round(top_counts / total_clean * 100, 2), 
        "%)",
        sep = "", collapse = "; "
      )
      
      summary_data <- list(
        Type = ifelse(is.factor(col), "Factor/Categorical", "Character/Categorical"),
        Count = length(col),
        Missing = n_na,
        Unique_Values = n_unique,
        Min = NA,
        Q1 = NA,
        Median = NA,
        Mean = NA,
        Q3 = NA,
        Max = NA,
        SD = NA,
        Top_Values = top_summary
      )
      
    } else {
      # Caso de outros tipos ou alta cardinalidade que não são numéricos
      summary_data <- list(
        Type = "Other/Unknown",
        Count = length(col),
        Missing = n_na,
        Unique_Values = n_unique,
        Min = NA, Q1 = NA, Median = NA, Mean = NA, Q3 = NA, Max = NA, SD = NA, Top_Values = NA
      )
    }
    
    # Adiciona o resumo da coluna atual à lista principal
    all_summaries[[col_name]] <- summary_data
  }
  
  # Converte a lista de listas em um dataframe final para fácil visualização
  summary_df <- do.call(rbind.data.frame, all_summaries)
  
  # Nomeia a coluna das variáveis (nomes das linhas)
  summary_df$Variable <- rownames(summary_df)
  rownames(summary_df) <- NULL
  
  # Reorganiza as colunas e retorna o resultado
  summary_df <- summary_df %>%
    select(Variable, Type, Count, Missing, Unique_Values, everything())
  
  return(summary_df)
}

# --- Exemplo de uso ---
data(df_clean)
resultado_sumario <- summarize_all_vars(df_clean)
print(resultado_sumario)

# --- Exemplo de uso ---
#check_df(df_clean)
#list_vars(df_clean)