# Detectar rodada ---------------------------------------------------------
jogos <- readRDS('./insumos/jogos.RDS')
confrontos <- readRDS('./insumos/confrontos.RDS')
# rodada_atual <- min(jogos %>%
#                       filter(valido == 1,
#                              end > Sys.time()  %>%
#                                as.POSIXct(tz = "America/Sao_Paulo") - 3600) %>%
#                       select(Rodada) %>%
#                       pull())

# Campeonato acabou, então irei fixar na rodada 27, para manter o app online
rodada_atual <- 36

jogos_atual <- jogos[jogos$Rodada==rodada_atual,]
jogos_atual <- jogos_atual %>%
  arrange(data)


confrontos_atual <- confrontos %>%
  filter(rodada == rodada_atual) %>%
  mutate(confronto = paste(jogador_a,'x',jogador_b))

confrontos_atual <- if (dim(confrontos_atual)[1] == 0) {
  tibble(rodada = rodada_atual,
         jogador_a = c('B.o','Cupim','Prison','Silas','Poke', 'Bara','Rwy')[sample(1:7,7)],
         jogador_b = c('Bernardo','Piss','PH','Kudsi','Giovano', 'Marinho','Graja')[sample(1:7,7)],
         rodada_bolao = rodada_atual-1) %>%
    mutate(confronto = paste(jogador_a,'x',jogador_b))
} else {confrontos_atual}

confrontos_hist <- read_excel('./insumos/Painel_Confrontos_Integrado.xlsx')

confrontos_hist$Cravadas_Op <- apply(confrontos_hist,1, function(x){
  confrontos_hist %>%
    filter(Jogador == x['Oponente'],
           Oponente == x['Jogador'],
           Edicao == x['Edicao'],
           Rodada == x['Rodada']) %>%
    select(Cravadas) %>%
    pull()
})

# andamento <- between(Sys.time()  %>%
#                        as.POSIXct(tz = "America/Sao_Paulo") - 3600,
#                      first(jogos_atual$start) %>%
#                        as.POSIXct(tz = "America/Sao_Paulo"),
#                      first(jogos_atual$end)  %>%
#                        as.POSIXct(tz = "America/Sao_Paulo"))

andamento <- TRUE
