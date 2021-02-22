rm(list=ls())

library(gmailr)
library(readxl)
library(lubridate)

# Setup para envio de e-mail
gm_auth_configure(key = "324060675307-l9emjplcbti55k4rt9r0cp8olt9eao73.apps.googleusercontent.com",
                  secret = "Zz8Vf2V-0EOVwZIv2O7qyovU")

options(
  gargle_oauth_cache = ".secret",
  gargle_oauth_email = "bolao.brasileirao.ie@gmail.com"
)
gm_auth(email = "bolao.brasileirao.ie@gmail.com")


# Recebimento dos e-mails -------------------------------------------------

# Brasileirão -------------------------------------------------------------

# x <- read_xlsx(path = './insumos/jogos_lixo.xlsx', col_names = F) %>%
#   rename(jogos = 1)  %>%
#   filter(!str_detect(jogos, 'Rodada')) %>%
#   pull()
#   
# marcados <- which(x=='-')
# 
# data <- x[marcados-2]
# mandante <- x[marcados-1]
# visitante <- x[marcados+1]
# 
# jogos <- tibble(Rodada = rep(c(1:38), each = 10),
#                 data,
#                 mandante,
#                 visitante) %>%
#   mutate(dia = substr(data, 1,2),
#          mes = substr(data, 4,5),
#          hora = substr(data, 8,12),
#          data = case_when(as.numeric(mes) >= 8 ~ paste0('2020-',
#                                                         mes,
#                                                         '-',
#                                                         dia,
#                                                         ' ',
#                                                         hora,
#                                                         ' -3'),
#                           TRUE ~ paste0('2021-',
#                                         mes,
#                                         '-',
#                                         dia,
#                                         ' ',
#                                         hora,
#                                         ' -3')),
#          data = as.POSIXct(data))
# 
# jogos%>%
#   group_by(Rodada) %>%
#   summarise(start = min(data),
#             end = max(data)+120*60) %>%
#   right_join(jogos, by = 'Rodada') %>%
#   saveRDS('./insumos/jogos.RDS')

# Bolão -------------------------------------------------------------------
# read_xlsx(path = './insumos/confrontos_bolao.xlsx', col_names = F) %>%
#   rename(jogos = 1)  %>%
#   filter(!str_detect(jogos, 'Rodada|TURNO')) %>%
#   mutate(rodada = rep(c(1:26), each = 7)) %>%
#   separate(jogos, c('jogador_a', 'jogador_b'), ' x ') %>%
#   select(rodada,
#          everything()) %>%
#   saveRDS('./insumos/confrontos.RDS')

jogos[47,"data"]<- as.POSIXct('2020-08-23 16:00:00')
jogos[42,"data"]<- as.POSIXct('2020-08-26 21:30:00')

jogos %>%
  mutate(dia = lubridate::day(data),
         mes = lubridate::month(data),
         hora = format(data, "%H:%M")) %>%
  group_by(Rodada) %>%
  mutate(start = min(data),
         end = max(data)+120*60) %>%
  ungroup() %>%
  mutate(dia_semana = str_to_title(weekdays(data))) %>%
  saveRDS('./insumos/jogos.RDS')
  

# Detectar rodada ---------------------------------------------------------
jogos <- readRDS('./insumos/jogos.RDS')
confrontos <- readRDS('./insumos/confrontos.RDS')
rodada_atual <- min(jogos[jogos$data > Sys.time()-10800, 'Rodada'])

jogos_atual <- jogos[jogos$Rodada==rodada_atual,]


gm_threads(paste0('Rodada ', rodada_atual)) %>%
  gm_id() %>%
  map(.,gm_thread) %>%
  lapply(., function(x){.[[1]]$messages}) %>%
  Reduce(f=append) %>%
  Reduce(f=append) -> x

x[[1]]$messages %>% str()
append(x[[1]]$messages,x[[2]]$messages)

confrontos %>%
  mutate_at(vars(contains('jogador')), ~ifelse(.=='Bernardo', 'B.o', .)) %>%
  mutate_at(vars(contains('jogador')), ~ifelse(.=='Victão', 'Bernardo', .)) %>%
  saveRDS('./insumos/confrontos.RDS')


confrontos %>%
  mutate_at(vars(contains('jogador')), ~ifelse(.=='Victão', 'Bernardo', .)) %>%
  saveRDS('./insumos/confrontos.RDS')


confrontos %>%
  mutate(rodada_bolao = rodada,
         rodada = rodada+1) %>%
  saveRDS('./insumos/confrontos.RDS')


# Atualização horario dos jogos -------------------------------------------
cbf <- data.frame(mandante = NA,
                  visitante = NA,
                  data = NA,
                  hora = NA)
cbf <- cbf[-1,]
i<-1
for (i in i:380){
  link <- paste0('https://www.cbf.com.br/futebol-brasileiro/competicoes/campeonato-brasileiro-serie-a/2020/',i,'?ref=linha')
  
    cbf[i,]<- tibble(mandante = link %>%
           read_html() %>%
           html_node(xpath = '//*[@id="menu-panel"]/article/section[1]/div/div/div/div/div/div[1]/h3') %>%
           html_text(),
         visitante = link %>%
           read_html() %>%
           html_node(xpath = '//*[@id="menu-panel"]/article/section[1]/div/div/div/div/div/div[5]/h3') %>%
           html_text(),
         data = link %>%
           read_html() %>%
           html_node(xpath = '//*[@id="menu-panel"]/article/section[1]/div/div/div/header/div/div[1]/span[2]') %>%
           html_text(),
         hora = link %>%
           read_html() %>%
           html_node(xpath = '//*[@id="menu-panel"]/article/section[1]/div/div/div/header/div/div[1]/span[3]') %>%
           html_text())
    
    print(paste0('Linha ',i,' pronta.'))
}

cbf2 <- cbf %>%
  mutate_all(trimws) %>%
  separate('data', c('dia', 'data'), ', ') %>%
  separate('data', c('dia', 'x1', 'mes', 'x2', 'ano'), ' ') %>%
  select(-x1,
         -x2) %>%
  mutate(mes = case_when(mes == 'Janeiro' ~ 'January',
                         mes == 'Fevereiro' ~ 'February',
                         mes == 'Março' ~ 'March',
                         mes == 'Abril' ~ 'Abril',
                         mes == 'Maio' ~ 'May',
                         mes == 'Junho' ~ 'June',
                         mes == 'Julho' ~ 'July',
                         mes == 'Agosto' ~ 'August',
                         mes == 'Setembro' ~ 'September',
                         mes == 'Outubro' ~ 'October',
                         mes == 'Novembro' ~ 'November',
                         mes == 'Dezembro' ~ 'December'),
         mes = match(mes, month.name),
         data = paste(ano, mes, dia, sep = '-'),
         data = paste0(data,' ', hora,':00'),
         data = case_when(str_detect(data, 'NA') ~ NA_character_,
                          TRUE ~ data),
         data = as.POSIXct(data, format = '%Y-%m-%d %H:%M:%S')) %>%
  mutate_at(vars(c('mandante', 'visitante')),
            ~case_when(. == 'Flamengo - RJ' ~ 'Flamengo',
                       . == 'Botafogo - RJ' ~ 'Botafogo',
                       . == 'Palmeiras - SP' ~ 'Palmeiras',
                       . == 'Santos - SP' ~ 'Santos',
                       . == 'Corinthians - SP' ~ 'Corinthians',
                       . == 'Grêmio - RS' ~ 'Grêmio',
                       . == 'Sport - PE' ~ 'Sport Recife',
                       . == 'Coritiba - PR' ~ 'Coritiba',
                       . == 'Fortaleza - CE' ~ 'Fortaleza',
                       . == 'Goiás - GO' ~ 'Goiás',
                       . == 'Fluminense - RJ' ~ 'Fluminense',
                       . == 'Vasco da Gama - RJ' ~ 'Vasco',
                       . == 'São Paulo - SP' ~ 'São Paulo',
                       . == "Red Bull Bragantino - SP" ~ "RB Bragantino",
                       . == "Atlético - MG" ~ "Atlético-MG",
                       . == "Internacional - RS" ~ 'Internacional',
                       . == "Bahia - BA" ~ 'Bahia',
                       . == "Athletico Paranaense - PR" ~ "Athletico-PR",
                       . == 'Ceará - CE' ~ 'Ceará',
                       . == "Atlético - GO" ~ 'Atlético-GO',
                       TRUE ~ .))

cbf2 %>%
  mutate(Rodada = rep(1:38,each = 10)) %>%
  group_by(Rodada) %>%
  summarise(start = min(data, na.rm = T),
            end = max(data, na.rm = T)+120*60) %>%
  right_join(cbf2 %>%
               mutate(Rodada = rep(1:38,each = 10)), by = 'Rodada') %>%
  mutate(valido = 1) %>%
  saveRDS('./insumos/jogos2.RDS')

# cbf <- readRDS('./insumos/jogos2.RDS')

# cbf2 <- cbf %>%
#   mutate(data = paste(ano, mes, dia, sep = '-'),
#          data = paste0(data,' ', hora,':00'),
#          data = as.POSIXct(data, format = '%Y-%m-%d %H:%M:%S'))

jogos %>%
  left_join(cbf2 %>%
              mutate(Rodada = rep(1:38,each = 10)) %>%
              group_by(Rodada) %>%
              summarise(start = min(data, na.rm = T),
                        end = max(data, na.rm = T)+120*60) %>%
              right_join(cbf2 %>%
                           mutate(Rodada = rep(1:38,each = 10)), by = 'Rodada') %>%
              mutate(valido = 1),
            by = c('mandante', 'visitante')) %>%
  select(-contains('start'),
         -contains('end'),
         -contains('dia'),
         -contains('mes'),
         -contains('ano'),
         -contains('hora')) %>%
  mutate(Rodada = ifelse(is.na(Rodada.y), Rodada.x, Rodada.y),
         data = ifelse(is.na(data.y), data.x, data.y)) %>%
  mutate(data = as.POSIXct(data, origin = "1970-01-01"),
         ano = year(data),
         mes = month(data),
         dia = day(data),
         dia_semana = str_to_title(weekdays(data)),
         hora =  paste0(hour(data), ":",
                        ifelse(minute(data)==0,"00", as.character(minute(data))))) %>%
  select(-contains('.')) %>%
  left_join(read_csv('./insumos/jogos_invalidos.csv') %>%
              mutate(valido = 0), 
            by = c('mandante', 'visitante')) %>%
  mutate(valido = ifelse(is.na(valido), 1,0)) %>%
  left_join(.,   group_by(., Rodada, valido) %>%
              summarise(start = min(data, na.rm = T),
                        end = max(data, na.rm = T)+120*60) %>%
              filter(valido == 1) %>%
              select(-valido),
            by = 'Rodada') %>% 
  saveRDS('./insumos/jogos.RDS')

# Segunda fase ------------------------------------------------------------

# Venc_1: melhor vencedor
# Venc_2: pior vencedor
# perd_1: melhor perdedor
# perd_2: pior perdedor

venc_1 <- 'Cupim'
venc_2 <- 'Graja'
perd_1 <- 'Marinho'
perd_2 <- 'Rwy'


list.files('./insumos/palpites', pattern = '*.RDS') %>%
  .[!str_detect(.,'especial')] %>%
  lapply(function(x){
    readRDS(paste0('./insumos/palpites/',x)) %>%
      mutate(rodada_br = str_remove(x, 'palpites_rodada_'),
             rodada_br = str_remove(rodada_br, '.RDS'),
             rodada_bolao = as.numeric(rodada_br) - 1) %>%
      select(contains('rodada'),
             everything())
  }) %>%
  Reduce(f=rbind)  %>%
  mutate(vencedor = case_when(mandante_placar > visitante_placar ~ mandante,
                              mandante_placar < visitante_placar ~ visitante,
                              mandante_placar == visitante_placar ~ 'Empate')) -> palpites

gm_threads('Placar Real', num_results = 100) %>%
  gm_id() %>%
  lapply(function(x){
    bind_cols(subject = x %>%
                gm_message() %>%
                gm_subject(),
              data = x %>%
                gm_message() %>%
                gm_date() %>%
                lubridate::dmy_hms(quiet = T,
                                   tz = "America/Sao_Paulo"),
              body = x %>%
                gm_message() %>%
                gm_body() %>%
                .[[1]])  
  }) %>%
  Reduce(f=rbind) %>%
  separate(subject, c('jogador','rodada'), ' - Rodada ') %>%
  group_by(rodada) %>%
  top_n(1, wt = data) %>%
  ungroup() %>%
  mutate(rodada = as.numeric(rodada)) %>%
  apply(., 1, function(x){
    x['body'] %>%
      xml2::read_html() %>%
      rvest::html_table() %>%
      .[[1]] %>%
      janitor::clean_names() %>%
      rename(confronto = x) %>%
      mutate(jogador = x['jogador'],
             rodada_br = x['rodada']) %>%
      select(jogador,
             everything())
  }) %>%
  Reduce(f=rbind) %>%
  mutate(vencedor = case_when(mandante_placar > visitante_placar ~ mandante,
                              mandante_placar < visitante_placar ~ visitante,
                              mandante_placar == visitante_placar ~ 'Empate')) -> gabarito


palpites <- palpites %>%
  left_join(gabarito %>%
              rename_all(~paste0(.,'_gaba')),
            by = c('mandante' = 'mandante_gaba', 'visitante' = 'visitante_gaba')) %>%
  left_join(jogos %>%
              filter(as.numeric(Rodada) <= 27) %>%
              select(mandante,
                     visitante,
                     valido),
            by = c('mandante', 'visitante')) %>%
  filter(valido == 1) %>%
  mutate(cravada = case_when(vencedor == vencedor_gaba &
                               mandante_placar == mandante_placar_gaba &
                               visitante_placar == visitante_placar_gaba ~ 1,
                             TRUE ~0),
         pontos = case_when(cravada == 1 ~ 25,
                            cravada == 0 & vencedor == vencedor_gaba & vencedor_gaba == 'Empate' ~ 15 ,
                            vencedor == vencedor_gaba & vencedor_gaba != 'Empate' &
                              ((mandante_placar == mandante_placar_gaba &
                                  visitante_placar != visitante_placar_gaba)|
                                 (mandante_placar != mandante_placar_gaba &
                                    visitante_placar == visitante_placar_gaba)) ~ 15,
                            vencedor == vencedor_gaba & vencedor_gaba != 'Empate' &
                              visitante_placar != visitante_placar_gaba &
                              mandante_placar != mandante_placar_gaba ~ 10,
                            TRUE ~ 0)) %>%
  select(-contains('_gaba'))

confrontos %>%
  filter(rodada <= 27) %>%
  mutate(confronto = rep(1:7, 26),
         rodada = as.character(rodada)) %>%
  gather(key = key, value = jogador, -rodada, -rodada_bolao, -confronto) %>%
  arrange(rodada_bolao,key,confronto) %>%
  bind_cols(confrontos %>%
              filter(rodada <= 27) %>%
              mutate(confronto = rep(1:7, 26),
                     rodada = as.character(rodada)) %>%
              gather(key = key, value = jogador, -rodada, -rodada_bolao, -confronto) %>%
              arrange(rodada_bolao,desc(key),confronto) %>%
              select(jogador) %>%
              rename(oponente = jogador)) %>%
  left_join(palpites %>%
              group_by(jogador, rodada_br) %>%
              summarise(pontos_pro = sum(pontos),
                        cravadas_pro = sum(cravada),
                        jogos_pontuados = sum(pontos>0)),
            by = c('jogador', 'rodada'='rodada_br')) %>%
  left_join(palpites %>%
              group_by(jogador, rodada_br) %>%
              summarise(pontos_contra = sum(pontos),
                        cravadas_contra = sum(cravada)),
            by = c('oponente' = 'jogador', 'rodada'='rodada_br')) %>%
  filter(as.numeric(rodada)<=27) %>%
  mutate_all(~ifelse(is.na(.),0,.)) %>%
  mutate(resultado = case_when(pontos_pro - pontos_contra>=10 ~ 'V',
                               abs(pontos_pro - pontos_contra) < 10 ~ 'E',
                               TRUE ~ 'D'),
         pontos = case_when(resultado == 'V' ~ 3,
                            resultado == 'E' ~ 1,
                            resultado == 'D' ~ 0),
         saldo = pontos_pro - pontos_contra) %>%
  filter(as.numeric(rodada)<=27,
         complete.cases(.)) %>%
  left_join(x = group_by(.,jogador) %>%
              summarise(Jogos = last(rodada_bolao),
                        Pontos = sum(pontos),
                        Cravadas = sum(cravadas_pro),
                        Vitorias = sum(resultado == 'V'),
                        Empates = sum(resultado == 'E'),
                        Derrotas = sum(resultado == 'D'),
                        Pontos_Pro = sum(pontos_pro),
                        Pontos_Contra = sum(pontos_contra),
                        Cravadas_Contra = sum(cravadas_contra),
                        Jogos_Pontuados = sum(jogos_pontuados),
                        Saldo = sum(saldo)) %>%
              ungroup(),
            y = group_by(.,jogador, rodada_bolao) %>%
              summarise(seq = paste(resultado)) %>%
              spread(key=rodada_bolao, value = seq) %>%
              unite(col = seq, paste0(colnames(.)[-1]), colnames(.)[-1], sep ='') %>%
              ungroup() %>%
              mutate(seq = apply(.,1, function(x){
                string_split = strsplit(x['seq'], split = "")
                # reverse order
                rev_order = nchar(x['seq']):1
                # reversed characters
                reversed_chars = string_split[[1]][rev_order]
                # collapse reversed characters
                paste(reversed_chars, collapse = "")
              })),
            by = 'jogador') %>%
  arrange(desc(Pontos),
          desc(Vitorias),
          desc(Pontos_Pro),
          desc(Cravadas)) %>%
  mutate(seq = substr(seq,1,5)) %>%
  rename(Jogador = jogador,
         Sequencia = seq) %>%
  select(Jogador,
         Jogos,
         Pontos,
         Cravadas,
         Vitorias,
         Empates,
         Derrotas,
         Pontos_Pro,
         Pontos_Contra,
         Cravadas_Contra,
         Jogos_Pontuados,
         Saldo,
         Sequencia)->classificacao

confrontos %>%
  filter(rodada <= 27) %>%
  bind_rows(
  tibble(rodada = 28,
         jogador_a = c(pull(classificacao[7,"Jogador"]),
                       pull(classificacao[8,"Jogador"]),
                       pull(classificacao[1,"Jogador"]),
                       pull(classificacao[2,"Jogador"]),
                       pull(classificacao[3,"Jogador"]),
                       pull(classificacao[4,"Jogador"]),
                       pull(classificacao[5,"Jogador"])),
         jogador_b = c(pull(classificacao[10,"Jogador"]),
                       pull(classificacao[9,"Jogador"]),
                       pull(classificacao[6,"Jogador"]),
                       pull(classificacao[11,"Jogador"]),
                       pull(classificacao[12,"Jogador"]),
                       pull(classificacao[13,"Jogador"]),
                       pull(classificacao[14,"Jogador"])),
         rodada_bolao = 27)
) %>%
  bind_rows(
    tibble(rodada = 29,
       jogador_a = c(pull(classificacao[1,"Jogador"]),
                     pull(classificacao[4,"Jogador"]),
                     pull(classificacao[2,"Jogador"]),
                     pull(classificacao[3,"Jogador"]),
                     perd_1,
                     perd_2,
                     pull(classificacao[11,"Jogador"])),
       jogador_b = c(venc_2,
                     pull(classificacao[5,"Jogador"]),
                     venc_1,
                     pull(classificacao[6,"Jogador"]),
                     pull(classificacao[14,"Jogador"]),
                     pull(classificacao[12,"Jogador"]),
                     pull(classificacao[13,"Jogador"])),
       rodada_bolao = 28)
    ) %>%
  bind_rows(
    tibble(rodada = 30,
       jogador_a = c(pull(classificacao[1,"Jogador"]),
                     pull(classificacao[4,"Jogador"]),
                     pull(classificacao[2,"Jogador"]),
                     pull(classificacao[3,"Jogador"]),
                     perd_1,
                     perd_2,
                     pull(classificacao[13,"Jogador"])),
       jogador_b = c(pull(classificacao[5,"Jogador"]),
                     venc_2,
                     pull(classificacao[6,"Jogador"]),
                     venc_1,
                     pull(classificacao[12,"Jogador"]),
                     pull(classificacao[11,"Jogador"]),
                     pull(classificacao[14,"Jogador"])),
       rodada_bolao = 29)
  ) %>%
  bind_rows(
    tibble(rodada = 31,
       jogador_a = c(pull(classificacao[1,"Jogador"]),
                     pull(classificacao[5,"Jogador"]),
                     pull(classificacao[2,"Jogador"]),
                     pull(classificacao[6,"Jogador"]),
                     perd_1,
                     pull(classificacao[11,"Jogador"]),
                     pull(classificacao[12,"Jogador"])),
       jogador_b = c(pull(classificacao[4,"Jogador"]),
                     venc_2,
                     pull(classificacao[3,"Jogador"]),
                     venc_1,
                     perd_2,
                     pull(classificacao[14,"Jogador"]),
                     pull(classificacao[13,"Jogador"])),
       rodada_bolao = 30)
  ) %>%
  bind_rows(
    tibble(rodada = 32,
       jogador_a = c(venc_2,
                     pull(classificacao[5,"Jogador"]),
                     venc_1,
                     pull(classificacao[6,"Jogador"]),
                     perd_1,
                     perd_2,
                     pull(classificacao[12,"Jogador"])),
       jogador_b = c(pull(classificacao[1,"Jogador"]),
                     pull(classificacao[4,"Jogador"]),
                     pull(classificacao[2,"Jogador"]),
                     pull(classificacao[3,"Jogador"]),
                     pull(classificacao[11,"Jogador"]),
                     pull(classificacao[13,"Jogador"]),
                     pull(classificacao[14,"Jogador"])),
       rodada_bolao = 31)
  ) %>%
  bind_rows(
    tibble(rodada = 33,
       jogador_a = c(pull(classificacao[5,"Jogador"]),
                     venc_2,
                     pull(classificacao[6,"Jogador"]),
                     venc_1,
                     perd_1,
                     perd_2,
                     pull(classificacao[11,"Jogador"])),
       jogador_b = c(pull(classificacao[1,"Jogador"]),
                     pull(classificacao[4,"Jogador"]),
                     pull(classificacao[2,"Jogador"]),
                     pull(classificacao[3,"Jogador"]),
                     pull(classificacao[13,"Jogador"]),
                     pull(classificacao[14,"Jogador"]),
                     pull(classificacao[12,"Jogador"])),
       rodada_bolao = 32)
  ) %>%
  bind_rows(
    tibble(rodada = 34,
       jogador_a = c(pull(classificacao[4,"Jogador"]),
                     venc_2,
                     pull(classificacao[3,"Jogador"]),
                     venc_1,
                     perd_1,
                     pull(classificacao[11,"Jogador"]),
                     pull(classificacao[12,"Jogador"])),
       jogador_b = c(pull(classificacao[1,"Jogador"]),
                     pull(classificacao[5,"Jogador"]),
                     pull(classificacao[2,"Jogador"]),
                     pull(classificacao[6,"Jogador"]),
                     perd_2,
                     pull(classificacao[14,"Jogador"]),
                     pull(classificacao[13,"Jogador"])),
       rodada_bolao = 33)
  ) %>% 
  saveRDS('./insumos/confrontos.RDS')

# Terceira fase ------------------------------------------------------------
source('classificacao.R')
venc_1 <- 'Cupim'
venc_2 <- 'Graja'
perd_1 <- 'Marinho'
perd_2 <- 'Rwy'

confrontos <- readRDS('./insumos/confrontos.RDS')

confrontos %>%
  filter(rodada < 34) %>%
  bind_rows(
  tibble(rodada = 34,
         jogador_a = c(pull(classificacao[4,"Jogador"]),
                       venc_2,
                       pull(classificacao[3,"Jogador"]),
                       venc_1,
                       pull(classificacao_hexagonal[3,"Jogador"]),
                       pull(classificacao_hexagonal[4,"Jogador"]),
                       pull(classificacao_hexagonal[1,"Jogador"])),
         jogador_b = c(pull(classificacao[1,"Jogador"]),
                       pull(classificacao[5,"Jogador"]),
                       pull(classificacao[2,"Jogador"]),
                       pull(classificacao[6,"Jogador"]),
                       pull(classificacao_hexagonal[6,"Jogador"]),
                       pull(classificacao_hexagonal[5,"Jogador"]),
                       pull(classificacao_hexagonal[2,"Jogador"])),
         rodada_bolao = 33)
) %>%
  bind_rows(
  tibble(rodada = 35,
         jogador_a = c(pull(classificacao_grupo_a[1,"Jogador"]),
                       pull(classificacao_grupo_b[1,"Jogador"]),
                       pull(classificacao_grupo_a[3,"Jogador"]),
                       pull(classificacao_grupo_b[3,"Jogador"]),
                       pull(classificacao_hexagonal[3,"Jogador"]),
                       pull(classificacao_hexagonal[4,"Jogador"]),
                       pull(classificacao_hexagonal[1,"Jogador"])),
         jogador_b = c(pull(classificacao_grupo_b[2,"Jogador"]),
                       pull(classificacao_grupo_a[2,"Jogador"]),
                       pull(classificacao_grupo_b[4,"Jogador"]),
                       pull(classificacao_grupo_a[4,"Jogador"]),
                       pull(classificacao_hexagonal[5,"Jogador"]),
                       pull(classificacao_hexagonal[6,"Jogador"]),
                       pull(classificacao_hexagonal[2,"Jogador"])),
         rodada_bolao = 34)
) %>%
  bind_rows(
    tibble(rodada = 36,
           jogador_a = c('Bara',
                         'Prison',
                         'Kudsi',
                         'Cupim',
                         pull(classificacao_hexagonal[3,"Jogador"]),
                         pull(classificacao_hexagonal[4,"Jogador"]),
                         pull(classificacao_hexagonal[1,"Jogador"])),
           jogador_b = c('Piss',
                         'Bernardo',
                         'B.o',
                         'Graja',
                         pull(classificacao_hexagonal[5,"Jogador"]),
                         pull(classificacao_hexagonal[6,"Jogador"]),
                         pull(classificacao_hexagonal[2,"Jogador"])),
           rodada_bolao = 35)
  ) %>% 
  saveRDS('./insumos/confrontos.RDS')
