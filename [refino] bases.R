# ____________________________________________________________________________
# title: Formação das bases
# author:  DEE - CECAN
# date: 21/05/2024
# ____________________________________________________________________________

# Esse é o pré-operacional da análise do impacto da privatização de refinarias
#
# As refinarias - Acelen e Petrobras
# Os produtos - gasolina, diesel S500 (usual) e diesel S10
# 
# Estrutura do código, e seu conteúdo:
#
# Seção 1 - Importação
# Seção 2 - Tratamento 

# Carregando pacotes
library(tidyverse)
library(lubridate)
library(readxl)
library(data.table)
library(rvest)
library(tabulapdf)

# 1. Importação _______________________________________________________________

setwd("T:/CECAN/Felipe Raposo/Refino/Bases")

# 1.1 Refinarias privatizadas

# Em seus sites, os preços são publicados por produto, ano e mês
# 
# aqui, o objetivo é ter uma função que acesse cada url e retorne 
# somente um data frame com essas informações

ler_privatizada <- function(produto, ano, mes, refinaria = NULL) {
  ## definindo a forma da url e os parâmetros
  if(is.null(refinaria)){
    stop(
      "Informe a refinaria de interesse: 1 - Acelen, 2 - REAM"
    )
  }
  # mapeamento de produtos para cada refinaria, cada uma enumera como quer, por isso a ordem bagunçada
  produtos_acelen <- c("Gasolina" = 5, "Diesel S10" = 7, "Diesel S500" = 6)
  produtos_ream <- c("Gasolina" = 1, "Diesel S10" = 2, "Diesel S500" = 3)
  
  ## se a refinaria for a acelen
  if(refinaria == 1){
    produto_num <- produtos_acelen[produto]
    if (is.na(produto_num)) stop("Produto não encontrado para Acelen")
    
    url <- paste0("https://www.acelen.com.br/precos-as-distribuidoras/?produto=", 
                  produto_num,
                  "&ano=", as.character(ano), 
                  "&mes=", as.character(mes), 
                  "#resultados") 
  }
  ## se a refinaria for ream
  ## else
  if (refinaria == 2){
    produto_num <- produtos_ream[produto]
    if (is.na(produto_num)) stop("Produto não encontrado para REAM")
    
    url <- paste0("https://sisdipre.ream.com.br/lancamentos/?produto=",
                  produto_num,
                  "&data__year=", as.character(ano),
                  "&data__month=", as.character(mes))
    # a ream publica os dados do primeiro período em tempos passados
    
  }
  
  ## errou a refinaria
  # else {
  #   stop("Refinaria inválida. Use 1 para Acelen ou 2 para REAM.")
  # }
  ## entrando no site, extraindo as tabelas e armazenando numa lista
  url <- rvest::read_html(url) %>% html_nodes("table")
  
  ## É bom ressaltar que as operações da acelen só iniciam em dezembro de 2021,
  ## enquanto as da ream em dezembro de 2022
  
  if (refinaria == 1){
    if (length(url) != 0) { # verificando se há tabela
      ## pegando o primeiro elemento daquela lista e pivotando
      preco_ <- html_table(url, fill = TRUE)[[1]] %>% 
        pivot_longer(-c(1,2), names_to = "date", values_to = "preco") %>%  
        mutate(preco = gsub("R\\$|[.,]", '', preco) %>% as.numeric(),
               produto = produtos_acelen[produto] %>% names())
      return(preco_)
    }
  }
  # else
  if(refinaria == 2) {
    if (length(url) != 0) { # verificando se há tabela
      ## pegando o primeiro elemento daquela lista
      preco_ <- html_table(url, fill = TRUE)[[1]] %>% 
        rename(preco = "Preço", date = "Data") %>% 
        mutate(preco = gsub("R\\$|[.,]", '', preco) %>% as.numeric(),
               produto = produtos_ream[produto] %>% names())
      return(preco_)
    }
  }
}


# 1.1.1 Acelen - Diesel S10

# data.frame com os preço de diesel s500 da acelen

acelen_diesel <- tibble(
  ano = rep(c(2021:2024), 12),
  mes = rep(1:12, each = 4)
) %>% 
  # fixando o parâmetro de diesel s10 (7) para cada mês dos anos de 2021-2024
  mutate(tabelas = map2(ano, mes, ler_privatizada, produto = "Diesel S10", refinaria = 1)) %>% 
  unnest(tabelas) %>% 
  # acelen só começou a existir em dezembro de 21
  filter(as.Date(date, "%d/%m/%Y") >= as.Date("01/12/2021", "%d/%m/%Y"))


# se quiser para os dois tipos de diesel, use o pmap ao invés de usar o map2
# acelen_diesel <- tibble(
#   ano = rep(2021:2024, each = 24),
#   mes = rep(1:12, times = 8),
#   produto = rep(c("Diesel S500","Diesel S10"), each = 12, times = 4)
# ) %>% 
#   # diesel s500 (6) e diesel s10 (7)
#   mutate(tabelas = pmap(list(produto, ano, mes), ler_acelen)) %>% 
#   unnest(tabelas) %>% 
#   filter(as.Date(date, "%d/%m/%Y") >= as.Date("01/12/2021", "%d/%m/%Y"))


# 1.1.2 Acelen - Gasolina A

# data.frame com os preço de gasolina da acelen

acelen_gas <- tibble(
  ano = rep(c(2021:2024), 12),
  mes = rep(1:12, each = 4)
) %>% 
  # fixando o parâmetro de gasolina (5) para cada mês dos anos de 2021-2024
  mutate(tabelas = map2(ano, mes, ler_privatizada, produto = "Gasolina", refinaria = 1)) %>% 
  unnest(tabelas) %>% 
  # acelen só começou a existir em dezembro de 21
  filter(as.Date(date, "%d/%m/%Y") >= as.Date("01/12/2021", "%d/%m/%Y"))


# 1.1.3 REAM - Diesel S10

# data.frame com os preço de diesel s10 da ream

ream_diesel <- tibble(
  ano = rep(c(2022:2024), 12),
  mes = rep(1:12, each = 3)
) %>% 
  # fixando o parâmetro de diesel s10 (2) para cada mês dos anos de 2021-2024
  mutate(tabelas = map2(ano, mes, ler_privatizada, produto = "Diesel S10", refinaria = 2)) %>% 
  unnest(tabelas)

# 1.1.4 REAM - Gasolina A

# data.frame com os preço de gasolina da ream

ream_gas <- tibble(
  ano = rep(c(2022:2024), 12),
  mes = rep(1:12, each = 3)
) %>% 
  # fixando o parâmetro de gasolina (1) para cada mês dos anos de 2021-2024
  mutate(tabelas = map2(ano, mes, ler_privatizada, produto = "Gasolina", refinaria = 2)) %>% 
  unnest(tabelas)


# 1.2 Petrobras

# Dados disponibilizados em: https://precos.petrobras.com.br/#!

# aqui, o objetivo é ter uma função que acesse cada pdf e retorne 
# somente um data frame com as tabelas

# leitura do pdf da petrobras
ler_petrobras <- function(pages, produto, data = FALSE, pasta = NULL){
  ## checando se foi adicionado o diretório
  if(is.null(pasta)){
    stop(
      "Informe a pasta em que o pdf está localizado") 
  }
  ## checando se o produto condiz com o escopo da análise
  ## NORMALIZAR COMO O PRODUTO ESTÁ ESCRITO !!
  if(!(produto %in% c("Gasolina","Diesel S500 e S10"))){
    stop(
      "O nome do produto deve ser 'Gasolina', 'Diesel S500 e S10'"
    )
  }
  ## checando se botou ou não data
  if(data == FALSE){
    data <- "01-05-24"
    message("Usando publicação de '01-05-24'")
  }
  ## checando se o arquivo existe
  pdf_petrobras <- paste0(pasta, "/Tabelas de Preços - ", produto, " ", data, ".pdf", 
                          sep = "")
  if(file.exists(pdf_petrobras) == FALSE){
    stop(
      "Na pasta informada, insira o pdf baixado em: https://precos.petrobras.com.br/#!"
    )
  }
  ## determinando as coordenadas de cada arquivo
  if(produto == "Gasolina"){
    region <- c(77, 17, 695, 595)
  }
  else{
    region <- c(60, 13, 585, 575)
  }
  ## para as coordenadas, lendo só a página informada
  gas_petrobras <- tabulapdf::extract_tables(
    file = pdf_petrobras, 
    pages = pages, 
    guess = FALSE,
    area = list(region)
  )[[1]] %>%
    ## filtrando as linhas que realmente tenham informação de preço
    filter(!is.na(LOCAL)) %>%
    pivot_longer(-c(1:2), names_to = "date", values_to = "preco") %>%
    mutate(preco = case_when(preco > 1000 ~ preco/100,
                             preco < 1000 ~ preco*1000,
                             .default = NA)) %>% 
    rename("MODALIDADE VENDA" = "...2")
  
  ## resultado final
  if(pages == 1){print(pdf_petrobras)} # se tudo está certo com o caminho 
  return(gas_petrobras)
}

# 1.2.1 Petrobras - Gasolina

# data.frame com os preço da gasolina pela petrobras
petrobras_gas <- map(1:9, ler_petrobras, produto = "Gasolina", data = "09-07-24", 
                     pasta = "T:/CECAN/Felipe Raposo/Refino/Bases") %>% 
  list_rbind() %>% 
  mutate(produto = "Gasolina")

# 1.2.2 Petrobras - Diesel S10

# data.frame com os preço do diesel s10 pela petrobras
# se quiser prgar para s500, número de páginas de 1 a 8
petrobras_diesel <- map(9:16, ler_petrobras, produto = 'Diesel S500 e S10', data = "01-07-24",
                        pasta = "T:/CECAN/Felipe Raposo/Refino/Bases") %>% 
  list_rbind() %>% 
  mutate(produto = "Diesel S10")

# 2. Tratamento _______________________________________________________________
# Transformação e adição de variáveis

combustivel <- function(produto, dee = TRUE, caminho = NULL){
  
  # verificando parâmetros
  
  # se a pessoa que está lendo está no dee
  if(dee == TRUE){
    # definindo caminho do preço do brent
    brent_file_path <- "T:/CECAN/Felipe Raposo/Refino/Bases/Dados Históricos - Petróleo Brent Futuros.csv" 
    # Fonte: https://www.investing.com/commodities/brent-oil-historical-data
  }else{
    
    # caso não, ela precisa dar o caminho da pasta
    #else #if(dee == F)
    #{
    if(is.null(caminho)){
      stop("Adicione caminho do arquivo de Preços do Brent")
    }else{
      # if(!(is.null(caminho))){
      brent_file_path <- caminho
      if(file.exists(caminho) == F){
        stop("Informar caminho certo do arquivo de Preços do Brent")
      }
    }
  }
  
  
  # tipo de produto
  if(produto == "Gasolina"){
    petrobras_df <- petrobras_gas
    acelen_df <- acelen_gas
    ream_df <- ream_gas
  }
  # else
  if(produto == "Diesel"){
    petrobras_df <- petrobras_diesel
    acelen_df <- acelen_diesel
    ream_df <- ream_diesel
  }
  
  # Identificando cidade e estado
  estados <- geobr::read_municipality() %>% 
    as.data.frame() %>% 
    select(name_muni, code_muni, code_state) %>% 
    rename(Cidade = name_muni) %>% 
    # trazendo capitais ao df
    left_join(
      geobr::read_capitals() %>%
        as.data.frame() %>%
        select(abbrev_state, code_state, name_muni) %>% 
        rename(Estado = "abbrev_state", capital = "name_muni"),
      by = "code_state")
  
  # acelen
  processado <- 
    acelen_df %>%
    # organizando o básico das datas
    mutate(old.date = as.Date(date, "%d/%m/%Y"), #Salvei a data antiga
           date = as.Date(date, "%d/%m/%Y") %>% substr(start = 1, stop = 7) %>% ym() %>% as.numeric(),
           priv = 1, 
           preco = preco / 10000,
           refinaria = "acelen") %>%
    
    # adicionando os dados da petrobras
    rbind(
      petrobras_df %>%
        mutate(old.date = as.Date(date, "%d.%m.%Y"), #Salvei a data antiga
               date = as.Date(date, "%d.%m.%Y") %>% substr(start = 1, stop = 7),
               ano = substr(date, start = 1, stop = 4),
               mes = substr(date, start = 6, stop = 7),
               date = date %>% ym() %>% as.numeric(),
               priv = 0,
               refinaria = "petrobras") %>%
        select(ano, mes, date, old.date, LOCAL, preco, `MODALIDADE VENDA`, priv, refinaria, produto)
    ) %>%
    
    # organizando informações de localização (estado, cidade, etc)
    separate(LOCAL, into = c("Cidade","Estado"), sep = "\\(") %>%
    mutate(Estado = gsub(")", '', Estado),
           # Cidade = str_sub(Cidade, 1, nchar(Cidade) - 1) %>% str_to_title(), # Considere usar str_trim()
           Cidade = str_trim(Cidade) %>% str_to_title(),
           Cidade = ifelse(Cidade == "Vila Do Conde", "Barcarena", Cidade)) %>%
    
    # pausa para adicionar ream adicionando ream
    rbind(
      ream_df %>%
        mutate(old.date = as.Date(date, "%d/%m/%Y"),  #Salvei a data antiga
               date = as.Date(date, "%d/%m/%Y") %>% substr(start = 1, stop = 7) %>% ym() %>% as.numeric(),
               priv = 1,
               preco = preco / 10,
               Estado = "AM",
               refinaria = "ream") %>%
        rename(Cidade = Local, `MODALIDADE VENDA` = `Modalidade de Venda`)) %>%
    
    ## continuação de localização 
    left_join(estados, by = c("Cidade", "Estado")) %>% # Isso tem um potêncial de dar ruim!
    # mutate(UF = code_state) %>% # Pq não rename?
    rename(UF = code_state, code_cidade = code_muni) %>%
    select(-Estado) %>% #, -code_state) %>%
    
    # organizando informações de preço e modalidade
    rename(modalide = `MODALIDADE VENDA`) %>%
    left_join(
      read.csv(brent_file_path) %>%
        mutate(date = as.Date(Date, "%m/%d/%Y") %>% substr(start = 1, stop = 7) %>% ym() %>% as.numeric(),
               preco_brent = Price %>% as.numeric()) %>%
        select(date, preco_brent),
      by = c("date")
    )
  
  return(processado)
}

# 2.1 Gasolina

caminho <- "T:/CECAN/Felipe Raposo/Refino/Bases/Brent Oil Futures Historical Data.csv"

gas <- combustivel(produto = "Gasolina", dee = FALSE, caminho = caminho) %>% 
  # trazendo o preço para litro
  mutate(preco = preco/1000)

# 2.2 Diesel

diesel <- combustivel(produto = "Diesel", dee = FALSE, caminho = caminho) %>% 
  # trazendo o preço para litro
  mutate(preco = preco/1000)

# 2.3 Ambos os produtos

df_combustivel <- diesel %>% 
  rbind(gas) %>% 
  rename(modalidade = modalide)

## Passando para um xlsx
# xlsx::write.xlsx(df_combustivel, "preco_combustiveis.xlsx",
#                     sheetName = "combustiveis")

rm(list = ls()[!ls() %in% "df_combustivel"])

# Tratamentos adicionaiT:

# - deflacionando os valores.
df_combustivel$preco_def <- deflateBR::ipca(df_combustivel$preco, df_combustivel$old.date, "07/2024")

# - Os meses não tem um padrão único.
df_combustivel$mes<- df_combustivel$mes %>% as.integer() %>% as.character()
df_combustivel$mes<-  sprintf("%02d", as.numeric(df_combustivel$mes))
df_combustivel$data<- paste0(df_combustivel$ano, "-", df_combustivel$mes)
df_combustivel$produto <- gsub(" ","_", df_combustivel$produto) %>% str_to_lower()

# - Parece ser o mesmo registro: "Candeias" & "Candeias Interestadua".
df_combustivel<- df_combustivel[df_combustivel$Cidade != "Candeias Interestadual",]

# - O que faremos agora é considerar que cidades muito próximas são um só ente
## Coordenadas em graus (latitude e longitude) dos municípios brasileiros
spatial <- geobr::read_municipality(year = 2022, showProgress = FALSE) %>%
  dplyr::select(code_muni, name_muni, geom, abbrev_state, code_state)

## Pegando somente as cidades observadas
spatial <- spatial %>% 
  filter(code_muni %in% df_combustivel$code_cidade) %>% 
  arrange(name_muni)

## Calculando a distância entre as cidades - dois pontos de uma esfera
distancia <- sf::st_distance(spatial,spatial) %>% 
  as.data.frame() %>% 
  `colnames<-`(spatial$name_muni) %>% 
  mutate(cidade1 = spatial$name_muni,
         code_muni = spatial$code_muni,
         estado = spatial$abbrev_state, 
         code_estado = spatial$code_state) %>% 
  select(estado, code_estado, code_muni, cidade1, everything()) %>% 
  pivot_longer(cols = -c(1:4), names_to = "cidade2", values_to = "distancia") %>% 
  mutate(distancia = as.numeric(distancia)) %>% 
  filter(distancia == 0 & cidade1 != cidade2)

## Agregando cidades MUITO próximas
df_combustivel_red <- df_combustivel %>%
  filter(Cidade %in% distancia$cidade1) %>% 
  arrange(Cidade, UF) %>% 
  group_by(ano,mes,data,modalidade,old.date,date,priv,refinaria,capital,UF,preco_brent,produto) %>% 
  summarise(preco = mean(preco, na.rm = TRUE), preco_def = mean(preco_def, na.rm = TRUE), .groups = "drop") %>% 
  mutate(
    Cidade = case_when(
      UF == 13 ~ "Manaus_Itacoatiara",
      UF == 15 ~ "Barcarena_Belém",
      UF == 29 ~ "Candeias_São_Francisco_Do_Conde",
      UF == 35 ~ "Cubatão_Santos"),
    # cidade principal
    code_cidade = case_when(
      UF == 13 ~ 1302603,  # manaus
      UF == 15 ~ 1501402,  # belém
      UF == 29 ~ 2906501,  # candeias
      UF == 35 ~ 3548500,  # santos
    )) %>% 
  rbind(df_combustivel %>% filter(!(Cidade %in% distancia$cidade1)))

# Salvando bases diárias
saveRDS(object = df_combustivel, file = "preco_combustiveis_diario.rds")
saveRDS(object = df_combustivel, file = "preco_combustiveis_diario_red.rds")

# - Falta agregar os dados por mês.
df_combustivel <- 
  df_combustivel %>% 
  group_by(ano,mes,data,Cidade,modalidade,date,priv,refinaria,code_cidade,capital,UF,preco_brent,produto) %>% 
  summarise(preco = mean(preco, na.rm = TRUE), preco_def = mean(preco_def, na.rm = TRUE))

df_combustivel_red <- df_combustivel_red %>% 
  group_by(ano,mes,data,Cidade,modalidade,date,priv,refinaria,code_cidade,capital,UF,preco_brent,produto) %>% 
  summarise(preco = mean(preco, na.rm = TRUE), preco_def = mean(preco_def, na.rm = TRUE), .groups = "drop")
# Alguns NAs foram criados, inspecionar!

# Salvando bases mensais
saveRDS(object = df_combustivel, file = "preco_combustiveis_mensal.rds")
saveRDS(object = df_combustivel, file = "preco_combustiveis_mensal_red.rds")

# Prontinho! Penso que é suficiente para essa parte, mas fique a vontade para fazer qualquer mudança que desejar! 
# A continuação da análise está em: "Z:/CECAN/Felipe Raposo/Refino/Sobrepreço"