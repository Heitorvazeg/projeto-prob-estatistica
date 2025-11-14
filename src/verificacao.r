library(survey)
library(dplyr)

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
#raw_data <- readRDS("data/raw/pnad_2022_q4_raw.rds")
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

# --- Mostra resumo rápido de cada variável ---
summary_vars <- function(df, max_levels = 20) {
  if (!check_df(df)) return(NULL)
  for (var in names(df)) {
    cat("\n====================\n")
    cat("Variável:", var, "\n")
    x <- df[[var]]
    cat("Tipo:", class(x), "\n")
    if (is.factor(x) || is.character(x)) {
      vals <- unique(x)
      cat("Valores únicos (até", max_levels, "):", paste(head(vals, max_levels), collapse = ", "), "\n")
      if (is.factor(x)) print(summary(x))
    } else if (is.numeric(x)) {
      print(summary(x))
    }
  }
}

# --- Exemplo de uso ---
#check_df(df_clean)
# list_vars(df_clean)
# summary_vars(df_clean)
