jogos <- readRDS('./insumos/jogos.RDS')
confrontos <- readRDS('./insumos/confrontos.RDS')

grupo_a <- c('Bara','Kudsi', 'Piss', 'Graja')
grupo_b <- c('B.o', 'Prison', 'Bernardo', 'Cupim')
hexagonal <- c('Marinho', 'Rwy','Silas','PH','Poke','Giovano')

# Setup -----------------------------------------------------------
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
              filter(as.numeric(Rodada) < rodada_atual) %>%
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

# Classificação geral -----------------------------------------------------
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
  filter(as.numeric(rodada)<rodada_atual) %>%
  mutate_all(~ifelse(is.na(.),0,.)) %>%
  mutate(resultado = case_when(pontos_pro - pontos_contra>=10 ~ 'V',
                               abs(pontos_pro - pontos_contra) < 10 ~ 'E',
                               TRUE ~ 'D'),
         pontos = case_when(resultado == 'V' ~ 3,
                            resultado == 'E' ~ 1,
                            resultado == 'D' ~ 0),
         saldo = pontos_pro - pontos_contra) %>%
  filter(as.numeric(rodada)<rodada_atual,
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


# Classificação 1º t ------------------------------------------------------
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
  filter(as.numeric(rodada)<rodada_atual) %>%
  mutate_all(~ifelse(is.na(.),0,.)) %>%
  mutate(resultado = case_when(pontos_pro - pontos_contra>=10 ~ 'V',
                               abs(pontos_pro - pontos_contra) < 10 ~ 'E',
                               TRUE ~ 'D'),
         pontos = case_when(resultado == 'V' ~ 3,
                            resultado == 'E' ~ 1,
                            resultado == 'D' ~ 0),
         saldo = pontos_pro - pontos_contra) %>%
  filter(as.numeric(rodada)<rodada_atual,
         complete.cases(.),
         as.numeric(rodada) %in% 2:14) %>%
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
         Sequencia) -> classificacao_1t

# Classificação 2ºt -------------------------------------------------------
if (rodada_atual>15){
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
    filter(as.numeric(rodada)<rodada_atual) %>%
    mutate_all(~ifelse(is.na(.),0,.)) %>%
    mutate(resultado = case_when(pontos_pro - pontos_contra>=10 ~ 'V',
                                 abs(pontos_pro - pontos_contra) < 10 ~ 'E',
                                 TRUE ~ 'D'),
           pontos = case_when(resultado == 'V' ~ 3,
                              resultado == 'E' ~ 1,
                              resultado == 'D' ~ 0),
           saldo = pontos_pro - pontos_contra) %>%
    filter(as.numeric(rodada)<rodada_atual,
           complete.cases(.),
           as.numeric(rodada) %in% 15:27) %>%
    left_join(x = group_by(.,jogador) %>%
                summarise(Jogos = last(rodada_bolao)-13,
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
           Sequencia) -> classificacao_2t 
} else {
  confrontos %>%
    gather(key = key,
           value = jogador,
           -rodada, -rodada_bolao) %>%
    mutate(jogador = trimws(jogador)) %>%
    filter(!duplicated(jogador)) %>%
    arrange(jogador) %>%
    select(jogador) %>%
    rename(Jogador = jogador) %>%
    mutate(Jogos = 0,
           Pontos = 0,
           Cravadas = 0,
           Vitorias = 0,
           Empates = 0,
           Derrotas = 0,
           Pontos_Pro = 0,
           Pontos_Contra = 0,
           Cravadas_Contra = 0,
           Jogos_Pontuados = 0,
           Saldo = 0,
           Sequencia = '-')  -> classificacao_2t 
}

# Pontos pró --------------------------------------------------------------
confrontos %>%
  filter(rodada != 28 & rodada < rodada_atual) %>%
  mutate(confronto = rep(1:7, rodada_atual-3),
         rodada = as.character(rodada)) %>%
  gather(key = key, value = jogador, -rodada, -rodada_bolao, -confronto) %>%
  arrange(rodada_bolao,key,confronto) %>%
  bind_cols(confrontos %>%
              filter(rodada != 27 & rodada < rodada_atual) %>%
              mutate(confronto = rep(1:7, rodada_atual-3),
                     rodada = as.character(rodada)) %>%
              gather(key = key, value = jogador, -rodada, -rodada_bolao, -confronto) %>%
              arrange(rodada_bolao,desc(key),confronto) %>%
              select(jogador) %>%
              rename(oponente = jogador)) %>%
  left_join(palpites %>%
              group_by(jogador, rodada_br) %>%
              summarise(pontos_pro = sum(pontos),
                        cravadas_pro = sum(cravada),
                        jogos_pontuados = sum(pontos>0),
                        palpites = n(),
                        palpites_ponto = sum(pontos>0)),
            by = c('jogador', 'rodada'='rodada_br')) %>%
  left_join(palpites %>%
              group_by(jogador, rodada_br) %>%
              summarise(pontos_contra = sum(pontos),
                        cravadas_contra = sum(cravada)),
            by = c('oponente' = 'jogador', 'rodada'='rodada_br')) %>%
  filter(as.numeric(rodada)<rodada_atual) %>%
  mutate_all(~ifelse(is.na(.),0,.)) %>%
  mutate(resultado = case_when(pontos_pro - pontos_contra>=10 ~ 'V',
                               abs(pontos_pro - pontos_contra) < 10 ~ 'E',
                               TRUE ~ 'D'),
         pontos = case_when(resultado == 'V' ~ 3,
                            resultado == 'E' ~ 1,
                            resultado == 'D' ~ 0),
         saldo = pontos_pro - pontos_contra) %>%
  filter(as.numeric(rodada)<rodada_atual,
         complete.cases(.)) %>%
  group_by(jogador) %>%
  summarise(Jogos = last(rodada_bolao),
            Pontos_Pro = sum(pontos_pro),
            Palpites = sum(palpites),
            Palpites_ponto = sum(palpites_ponto)) %>%
  ungroup() %>%
  mutate(Aproveitamento = scales::percent(Pontos_Pro/(25*Palpites)),
         Pontos_por_rodada = round(Pontos_Pro/Jogos,3),
         Pontos_por_palpite_pontuados = round(Pontos_Pro/Palpites_ponto,2)) %>%
  arrange(desc(Pontos_Pro),
          desc(Aproveitamento)) %>%
  setNames(c('Jogador',
             'Rodadas',
             'Pontos Pró',
             'Palpites',
             'Palpites pontuados',
             'Aproveitamento',
             'Pontos/Rodada',
             'Pontos por palpites pontuados')) -> pontos_pro

# Cravadas ----------------------------------------------------------------
confrontos %>%
  filter(rodada != 28 & rodada < rodada_atual) %>%
  mutate(confronto = rep(1:7, rodada_atual-3),
         rodada = as.character(rodada)) %>%
  gather(key = key, value = jogador, -rodada, -rodada_bolao, -confronto) %>%
  arrange(rodada_bolao,key,confronto) %>%
  bind_cols(confrontos %>%
              filter(rodada != 27 & rodada < rodada_atual) %>%
              mutate(confronto = rep(1:7, rodada_atual-3),
                     rodada = as.character(rodada)) %>%
              gather(key = key, value = jogador, -rodada, -rodada_bolao, -confronto) %>%
              arrange(rodada_bolao,desc(key),confronto) %>%
              select(jogador) %>%
              rename(oponente = jogador)) %>%
  left_join(palpites %>%
              group_by(jogador, rodada_br) %>%
              summarise(pontos_pro = sum(pontos),
                        cravadas_pro = sum(cravada),
                        jogos_pontuados = sum(pontos>0),
                        palpites = n(),
                        palpites_ponto = sum(pontos>0)),
            by = c('jogador', 'rodada'='rodada_br')) %>%
  left_join(palpites %>%
              group_by(jogador, rodada_br) %>%
              summarise(pontos_contra = sum(pontos),
                        cravadas_contra = sum(cravada)),
            by = c('oponente' = 'jogador', 'rodada'='rodada_br')) %>%
  filter(as.numeric(rodada)<rodada_atual) %>%
  mutate_all(~ifelse(is.na(.),0,.)) %>%
  mutate(resultado = case_when(pontos_pro - pontos_contra>=10 ~ 'V',
                               abs(pontos_pro - pontos_contra) < 10 ~ 'E',
                               TRUE ~ 'D'),
         pontos = case_when(resultado == 'V' ~ 3,
                            resultado == 'E' ~ 1,
                            resultado == 'D' ~ 0),
         saldo = pontos_pro - pontos_contra) %>%
  filter(as.numeric(rodada)<rodada_atual,
         complete.cases(.)) %>%
  group_by(jogador) %>%
  summarise(Jogos = last(rodada_bolao),
            Cravadas = sum(cravadas_pro),
            Palpites = sum(palpites)) %>%
  ungroup() %>%
  mutate(Aproveitamento = scales::percent(Cravadas/(Palpites)),
         Cravadas_por_rodada = round(Cravadas/Jogos,2)) %>%
  arrange(desc(Cravadas),
          desc(Cravadas_por_rodada)) %>%
  setNames(c('Jogador',
             'Rodadas',
             'Cravadas',
             'Palpites',
             'Aproveitamento',
             'Cravadas/Rodada')) -> cravadas

# Grupo A -----------------------------------------------------------------
confrontos %>%
  filter(rodada > 27,
         rodada <= 34,
         jogador_a %in% grupo_a,
         jogador_b %in% grupo_a) %>%
  mutate(confronto = rep(1:2, 6),
         rodada = as.character(rodada)) %>%
  gather(key = key, value = jogador, -rodada, -rodada_bolao, -confronto) %>%
  arrange(rodada_bolao,key,confronto) %>%
  bind_cols(confrontos %>%
              filter(rodada > 27,
                     rodada <= 34,
                     jogador_a %in% grupo_a,
                     jogador_b %in% grupo_a) %>%
              mutate(confronto = rep(1:2, 6),
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
  filter(as.numeric(rodada)<rodada_atual) %>%
  mutate_all(~ifelse(is.na(.),0,.)) %>%
  mutate(resultado = case_when(pontos_pro - pontos_contra>=10 ~ 'V',
                               abs(pontos_pro - pontos_contra) < 10 ~ 'E',
                               TRUE ~ 'D'),
         pontos = case_when(resultado == 'V' ~ 3,
                            resultado == 'E' ~ 1,
                            resultado == 'D' ~ 0),
         saldo = pontos_pro - pontos_contra) %>%
  filter(as.numeric(rodada)<rodada_atual) %>%
  filter(complete.cases(.)) %>%
  left_join(x = group_by(.,jogador) %>%
              summarise(Pontos = sum(pontos),
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
  mutate(Pontos = case_when(jogador == 'Bara' ~ Pontos + 3,
                            jogador == 'Kudsi' ~ Pontos + 2,
                            jogador == 'Piss' ~ Pontos + 1,
                            TRUE ~ Pontos)) %>%
  arrange(desc(Pontos),
          desc(Vitorias),
          desc(Pontos_Pro),
          desc(Cravadas)) %>%
  mutate(seq = substr(seq,1,5),
         Jogos = Vitorias + Derrotas + Empates) %>%
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
         Sequencia) -> classificacao_grupo_a


# Grupo B -----------------------------------------------------------------
confrontos %>%
  filter(rodada > 27,
         rodada <= 34,
         jogador_a %in% grupo_b,
         jogador_b %in% grupo_b) %>%
  mutate(confronto = rep(1:2, 6),
         rodada = as.character(rodada)) %>%
  gather(key = key, value = jogador, -rodada, -rodada_bolao, -confronto) %>%
  arrange(rodada_bolao,key,confronto) %>%
  bind_cols(confrontos %>%
              filter(rodada > 27,
                     rodada <= 34,
                     jogador_a %in% grupo_b,
                     jogador_b %in% grupo_b) %>%
              mutate(confronto = rep(1:2, 6),
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
  filter(as.numeric(rodada)<rodada_atual) %>%
  mutate_all(~ifelse(is.na(.),0,.)) %>%
  mutate(resultado = case_when(pontos_pro - pontos_contra>=10 ~ 'V',
                               abs(pontos_pro - pontos_contra) < 10 ~ 'E',
                               TRUE ~ 'D'),
         pontos = case_when(resultado == 'V' ~ 3,
                            resultado == 'E' ~ 1,
                            resultado == 'D' ~ 0),
         saldo = pontos_pro - pontos_contra) %>%
  filter(as.numeric(rodada)<rodada_atual) %>%
  filter(complete.cases(.)) %>%
  left_join(x = group_by(.,jogador) %>%
              summarise(Pontos = sum(pontos),
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
  mutate(Pontos = case_when(jogador == 'B.o' ~ Pontos + 3,
                            jogador == 'Prison' ~ Pontos + 2,
                            jogador == 'Bernardo' ~ Pontos + 1,
                            TRUE ~ Pontos)) %>%
  arrange(desc(Pontos),
          desc(Vitorias),
          desc(Pontos_Pro),
          desc(Cravadas)) %>%
  mutate(seq = substr(seq,1,5),
         Jogos = Vitorias + Derrotas + Empates) %>%
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
         Sequencia) -> classificacao_grupo_b


# Hexagonal do rebaixamento -----------------------------------------------
confrontos %>%
  filter(rodada > 27,
         rodada <= 33,
         jogador_a %in% hexagonal,
         jogador_b %in% hexagonal) %>%
  mutate(confronto = rep(1:3, 5),
         rodada = as.character(rodada)) %>%
  gather(key = key, value = jogador, -rodada, -rodada_bolao, -confronto) %>%
  arrange(rodada_bolao,key,confronto) %>%
  bind_cols(confrontos %>%
              filter(rodada > 27,
                     rodada <= 33,
                     jogador_a %in% hexagonal,
                     jogador_b %in% hexagonal) %>%
              mutate(confronto = rep(1:3, 5),
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
  filter(as.numeric(rodada)<rodada_atual) %>%
  mutate_all(~ifelse(is.na(.),0,.)) %>%
  mutate(resultado = case_when(pontos_pro - pontos_contra>=10 ~ 'V',
                               abs(pontos_pro - pontos_contra) < 10 ~ 'E',
                               TRUE ~ 'D'),
         pontos = case_when(resultado == 'V' ~ 3,
                            resultado == 'E' ~ 1,
                            resultado == 'D' ~ 0),
         saldo = pontos_pro - pontos_contra) %>%
  filter(as.numeric(rodada)<rodada_atual) %>%
  filter(complete.cases(.)) %>%
  left_join(x = group_by(.,jogador) %>%
              summarise(Pontos = sum(pontos),
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
  mutate(seq = substr(seq,1,5),
         Jogos = Vitorias + Derrotas + Empates) %>%
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
         Sequencia) -> classificacao_hexagonal

# Campeão e artilheiro ----------------------------------------------------
artilheiro <- tibble(jogador = 'https://globoesporte.globo.com/futebol/brasileirao-serie-a/' %>%
                       read_html() %>%
                       html_nodes('.jogador-nome') %>%
                       html_text(),
                     Gols = 'https://globoesporte.globo.com/futebol/brasileirao-serie-a/' %>%
                       read_html() %>%
                       html_nodes('.jogador-gols') %>%
                       html_text(),
                     Time = 'https://globoesporte.globo.com/futebol/brasileirao-serie-a/' %>%
                       read_html() %>%
                       html_nodes('.jogador-escudo') %>%
                       html_nodes("img") %>%
                       html_attr('alt')) %>%
  mutate(Gols = as.numeric(Gols),
         rank = min_rank(desc(Gols)))


campeao_palpite <- 'https://globoesporte.globo.com/futebol/brasileirao-serie-a/' %>%
  readLines() %>%
  .[str_detect(., 'const classificacao ')] %>%
  str_extract('\\{"classificacao":\\[.*.\\}\\],"edicao"') %>%
  str_remove('\\],"edicao"') %>%
  paste0(., ']}') %>%
  jsonlite::fromJSON(.) %>%
  .$classificacao %>%
  select(nome_popular,
         ordem)


camp_art <- read_csv('./insumos/camp_art.csv')


camp_art %>%
  left_join(campeao_palpite,
            by = c('Campeao' = 'nome_popular')) %>%
  left_join(artilheiro,
            by = c('Artilheiro' = 'jogador',
                   'artilheiro_time' = 'Time')) %>%
  mutate(Gols = ifelse(is.na(Gols),0,Gols),
         pontos_campeao = case_when(ordem == 1 ~ 10,
                                    ordem == 2 ~ 6,
                                    ordem == 3 ~ 3,
                                    ordem == 4 ~ 1,
                                    TRUE ~ 0),
         pontos_artilheiro = case_when(rank == 1 ~ 15,
                                       rank == 2 ~ 9,
                                       rank == 3 ~ 6,
                                       rank == 4 ~ 2,
                                       TRUE ~ 0),
         Total = pontos_campeao + pontos_artilheiro,
         Artilheiro = paste0(Artilheiro, ' (',artilheiro_time,')'),
         rank = as.character(rank),
         rank = case_when(is.na(rank) ~ '-',
                          TRUE ~ rank)) %>%
  arrange(desc(Total)) %>%
  select(Jogador,
         Campeao,
         ordem,
         pontos_campeao,
         Artilheiro,
         Gols,
         rank,
         pontos_artilheiro,
         Total) %>%
  setNames(c('Jogador',
             'Palpite campeão',
             'Colocação - Campeão',
             'Pontos',
             'Palpite artilheiro',
             'Gols',
             'Colocação - Artilheiro',
             'Pontos',
             'Total')) -> campeao_artilheiro


# Confrontos --------------------------------------------------------------
for (i in 1:max(confrontos$rodada_bolao)){
  if(i<rodada_atual-1){
    
    df_temp <- palpites %>%
      filter(rodada_bolao == i)
    
    df_temp <- confrontos %>%
      filter(rodada_bolao == i) %>%
      mutate(confronto = paste(jogador_a,'x',jogador_b)) %>%
      gather(key = key, value = jogador, -rodada, -rodada_bolao, -confronto) %>%
      arrange(rodada_bolao,key,confronto) %>%
      left_join(df_temp %>%
                  group_by(jogador) %>%
                  summarise(pontos_pro = sum(pontos),
                            cravadas_pro = sum(cravada)),
                by = c('jogador')) %>%
      select(-jogador) %>%
      pivot_wider(names_from = c('key'),
                  values_from = c('pontos_pro',
                                  'cravadas_pro')) %>%
      separate(confronto, c('jogador_a', 'jogador_b'), ' x ') %>%
      mutate_all(~ifelse(is.na(.),0,.))
      
    
    paste0('<b>*Resultado final*</b><br/><br/>',
           ifelse(df_temp$pontos_pro_jogador_a[1]-df_temp$pontos_pro_jogador_b[1]>=10,
                  paste0('<b>*',df_temp$jogador_a[1],' ','(',df_temp$cravadas_pro_jogador_a[1],')',' ',df_temp$pontos_pro_jogador_a[1],'*</b>'),
                  paste0(df_temp$jogador_a[1],' ','(',df_temp$cravadas_pro_jogador_a[1],')',' ',df_temp$pontos_pro_jogador_a[1])),
           ' x ',
           ifelse(df_temp$pontos_pro_jogador_b[1]-df_temp$pontos_pro_jogador_a[1]>=10,
                  paste0('<b>*',df_temp$pontos_pro_jogador_b[1],' ','(',df_temp$cravadas_pro_jogador_b[1],') ',df_temp$jogador_b[1],'*</b>'),
                  paste0(df_temp$pontos_pro_jogador_b[1],' ','(',df_temp$cravadas_pro_jogador_b[1],') ',df_temp$jogador_b[1])),
           '<br/>',
           ifelse(df_temp$pontos_pro_jogador_a[2]-df_temp$pontos_pro_jogador_b[2]>=10,
                  paste0('<b>*',df_temp$jogador_a[2],' ','(',df_temp$cravadas_pro_jogador_a[2],')',' ',df_temp$pontos_pro_jogador_a[2],'*</b>'),
                  paste0(df_temp$jogador_a[2],' ','(',df_temp$cravadas_pro_jogador_a[2],')',' ',df_temp$pontos_pro_jogador_a[2])),
           ' x ',
           ifelse(df_temp$pontos_pro_jogador_b[2]-df_temp$pontos_pro_jogador_a[2]>=10,
                  paste0('<b>*',df_temp$pontos_pro_jogador_b[2],' ','(',df_temp$cravadas_pro_jogador_b[2],') ',df_temp$jogador_b[2],'*</b>'),
                  paste0(df_temp$pontos_pro_jogador_b[2],' ','(',df_temp$cravadas_pro_jogador_b[2],') ',df_temp$jogador_b[2])),
           '<br/>',
           ifelse(df_temp$pontos_pro_jogador_a[3]-df_temp$pontos_pro_jogador_b[3]>=10,
                  paste0('<b>*',df_temp$jogador_a[3],' ','(',df_temp$cravadas_pro_jogador_a[3],')',' ',df_temp$pontos_pro_jogador_a[3],'*</b>'),
                  paste0(df_temp$jogador_a[3],' ','(',df_temp$cravadas_pro_jogador_a[3],')',' ',df_temp$pontos_pro_jogador_a[3])),
           ' x ',
           ifelse(df_temp$pontos_pro_jogador_b[3]-df_temp$pontos_pro_jogador_a[3]>=10,
                  paste0('<b>*',df_temp$pontos_pro_jogador_b[3],' ','(',df_temp$cravadas_pro_jogador_b[3],') ',df_temp$jogador_b[3],'*</b>'),
                  paste0(df_temp$pontos_pro_jogador_b[3],' ','(',df_temp$cravadas_pro_jogador_b[3],') ',df_temp$jogador_b[3])),
           '<br/>',
           ifelse(df_temp$pontos_pro_jogador_a[4]-df_temp$pontos_pro_jogador_b[4]>=10,
                  paste0('<b>*',df_temp$jogador_a[4],' ','(',df_temp$cravadas_pro_jogador_a[4],')',' ',df_temp$pontos_pro_jogador_a[4],'*</b>'),
                  paste0(df_temp$jogador_a[4],' ','(',df_temp$cravadas_pro_jogador_a[4],')',' ',df_temp$pontos_pro_jogador_a[4])),
           ' x ',
           ifelse(df_temp$pontos_pro_jogador_b[4]-df_temp$pontos_pro_jogador_a[4]>=10,
                  paste0('<b>*',df_temp$pontos_pro_jogador_b[4],' ','(',df_temp$cravadas_pro_jogador_b[4],') ',df_temp$jogador_b[4],'*</b>'),
                  paste0(df_temp$pontos_pro_jogador_b[4],' ','(',df_temp$cravadas_pro_jogador_b[4],') ',df_temp$jogador_b[4])),
           '<br/>',
           ifelse(df_temp$pontos_pro_jogador_a[5]-df_temp$pontos_pro_jogador_b[5]>=10,
                  paste0('<b>*',df_temp$jogador_a[5],' ','(',df_temp$cravadas_pro_jogador_a[5],')',' ',df_temp$pontos_pro_jogador_a[5],'*</b>'),
                  paste0(df_temp$jogador_a[5],' ','(',df_temp$cravadas_pro_jogador_a[5],')',' ',df_temp$pontos_pro_jogador_a[5])),
           ' x ',
           ifelse(df_temp$pontos_pro_jogador_b[5]-df_temp$pontos_pro_jogador_a[5]>=10,
                  paste0('<b>*',df_temp$pontos_pro_jogador_b[5],' ','(',df_temp$cravadas_pro_jogador_b[5],') ',df_temp$jogador_b[5],'*</b>'),
                  paste0(df_temp$pontos_pro_jogador_b[5],' ','(',df_temp$cravadas_pro_jogador_b[5],') ',df_temp$jogador_b[5])),
           '<br/>',
           ifelse(df_temp$pontos_pro_jogador_a[6]-df_temp$pontos_pro_jogador_b[6]>=10,
                  paste0('<b>*',df_temp$jogador_a[6],' ','(',df_temp$cravadas_pro_jogador_a[6],')',' ',df_temp$pontos_pro_jogador_a[6],'*</b>'),
                  paste0(df_temp$jogador_a[6],' ','(',df_temp$cravadas_pro_jogador_a[6],')',' ',df_temp$pontos_pro_jogador_a[6])),
           ' x ',
           ifelse(df_temp$pontos_pro_jogador_b[6]-df_temp$pontos_pro_jogador_a[6]>=10,
                  paste0('<b>*',df_temp$pontos_pro_jogador_b[6],' ','(',df_temp$cravadas_pro_jogador_b[6],') ',df_temp$jogador_b[6],'*</b>'),
                  paste0(df_temp$pontos_pro_jogador_b[6],' ','(',df_temp$cravadas_pro_jogador_b[6],') ',df_temp$jogador_b[6])),
           '<br/>',
           ifelse(df_temp$pontos_pro_jogador_a[7]-df_temp$pontos_pro_jogador_b[7]>=10,
                  paste0('<b>*',df_temp$jogador_a[7],' ','(',df_temp$cravadas_pro_jogador_a[7],')',' ',df_temp$pontos_pro_jogador_a[7],'*</b>'),
                  paste0(df_temp$jogador_a[7],' ','(',df_temp$cravadas_pro_jogador_a[7],')',' ',df_temp$pontos_pro_jogador_a[7])),
           ' x ',
           ifelse(df_temp$pontos_pro_jogador_b[7]-df_temp$pontos_pro_jogador_a[7]>=10,
                  paste0('<b>*',df_temp$pontos_pro_jogador_b[7],' ','(',df_temp$cravadas_pro_jogador_b[7],') ',df_temp$jogador_b[7],'*</b>'),
                  paste0(df_temp$pontos_pro_jogador_b[7],' ','(',df_temp$cravadas_pro_jogador_b[7],') ',df_temp$jogador_b[7]))) %>%
           writeLines(.,
                 paste0('./insumos/confrontos/confronto_',i,'.html'))
    
  } else {
    df_temp <- confrontos %>%
      filter(rodada_bolao == i)
    
    paste0('<b>*Confrontos*</b><br/><br/>',
           df_temp$jogador_a[1],' x ',df_temp$jogador_b[1],'<br/>',
           df_temp$jogador_a[2],' x ',df_temp$jogador_b[2],'<br/>',
           df_temp$jogador_a[3],' x ',df_temp$jogador_b[3],'<br/>',
           df_temp$jogador_a[4],' x ',df_temp$jogador_b[4],'<br/>',
           df_temp$jogador_a[5],' x ',df_temp$jogador_b[5],'<br/>',
           df_temp$jogador_a[6],' x ',df_temp$jogador_b[6],'<br/>',
           df_temp$jogador_a[7],' x ',df_temp$jogador_b[7]) %>%
      writeLines(.,
                 paste0('./insumos/confrontos/confronto_',i,'.html'))
  }
}

confrontos %>%
  mutate(confronto = rep(1:7, max(confrontos$rodada_bolao)),
         rodada = as.character(rodada)) %>%
  gather(key = key, value = jogador, -rodada, -rodada_bolao, -confronto) %>%
  arrange(rodada_bolao,key,confronto) %>%
  bind_cols(confrontos %>%
              mutate(confronto = rep(1:7,max(confrontos$rodada_bolao)),
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
  filter(as.numeric(rodada)<rodada_atual) %>%
  mutate_all(~ifelse(is.na(.),0,.)) %>%
  mutate(resultado = case_when(pontos_pro - pontos_contra>=10 ~ 'V',
                               abs(pontos_pro - pontos_contra) < 10 ~ 'E',
                               TRUE ~ 'D'),
         pontos = case_when(resultado == 'V' ~ 3,
                            resultado == 'E' ~ 1,
                            resultado == 'D' ~ 0),
         saldo = pontos_pro - pontos_contra) %>%
  filter(as.numeric(rodada)<rodada_atual,
         complete.cases(.),
         as.numeric(rodada) < rodada_atual) %>%
  group_by(jogador, rodada_bolao) %>%
  summarise(seq = paste(resultado)) %>%
  spread(key=rodada_bolao, value = seq) %>%
  rename(Jogador = jogador) -> sequencia
