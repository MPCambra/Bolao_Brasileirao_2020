jogos <- readRDS('./insumos/jogos.RDS')
confrontos <- readRDS('./insumos/confrontos.RDS')

# Palpites da rodada
gm_threads(paste0('"Rodada ', rodada_atual,'"')) %>%
  gm_id() %>%
  lapply(function(x){
    bind_cols(subject = x %>%
                gm_message() %>%
                gm_subject(),
              data = x %>%
                gm_message() %>%
                gm_date() %>%
                lubridate::dmy_hms(quiet = T,
                                   tz = "America/Sao_Paulo") - 3600,
              body = x %>%
                gm_message() %>%
                gm_body() %>%
                .[[1]])
  }) %>%
  Reduce(f=rbind) %>%
  # filter(str_detect(subject,paste0('Rodada ', rodada_atual)),
  #        data <= first(jogos_atual$start) %>%
  #          as.POSIXct(tz = "America/Sao_Paulo")) %>%
  filter(str_detect(subject,paste0('Rodada ', rodada_atual))) %>%
  separate(subject, c('jogador','rodada'), ' - Rodada ') %>%
  group_by(jogador) %>%
  top_n(1, wt = data) %>%
  filter(!duplicated(jogador),
         jogador != 'Placar Real') %>%
  ungroup() %>%
  mutate(rodada = as.numeric(rodada)) %>%
  right_join(confrontos %>%
               filter(rodada == rodada_atual) %>%
               select(-rodada_bolao)%>%
               gather(key=jjj,value = jogador, -rodada) %>%
               select(-jjj),
             by = c('jogador', 'rodada')) %>%
  mutate(esqueceu = as.numeric(is.na(body))) %>%
  apply(., 1, function(x){
    if(x['esqueceu'] == 1){
      jogos_atual %>%
        select(mandante,
               visitante) %>%
        mutate(confronto = 1:10,
               jogador = x['jogador'],
               mandante_placar = NA,
               visitante_placar = NA)
    } else {
      x['body'] %>%
        xml2::read_html() %>%
        rvest::html_table() %>%
        .[[1]] %>%
        janitor::clean_names() %>%
        rename(confronto = x) %>%
        mutate(jogador = x['jogador']) %>%
        select(jogador,
               everything())
    }

  }) %>%
  Reduce(f=rbind) %>%
  mutate(rodada = rodada_atual) %>%
  mutate(vencedor = case_when(mandante_placar > visitante_placar ~ mandante,
                              mandante_placar < visitante_placar ~ visitante,
                              mandante_placar == visitante_placar ~ 'Empate')) -> df


# readRDS(paste0('insumos/palpites/palpites_rodada_',rodada_atual,'.RDS')) %>%
#   mutate(rodada = rodada_atual) %>%
#   mutate(vencedor = case_when(mandante_placar > visitante_placar ~ mandante,
#                               mandante_placar < visitante_placar ~ visitante,
#                               mandante_placar == visitante_placar ~ 'Empate')) -> df

# Resultados em tempo real
'https://globoesporte.globo.com/futebol/brasileirao-serie-a/' %>%
  readLines() %>%
  .[str_detect(., 'const listaJogos ')] %>%
  str_remove('    const listaJogos = ') %>%
  substr(1,nchar(.)-1) %>%
  jsonlite::fromJSON(.) %>%
  janitor::clean_names() -> x

x$equipes$mandante$nome_popular %>%
  bind_cols(mandante =.,
            visitante = x$equipes$visitante$nome_popular,
            mandante_placar = x$placar_oficial_mandante,
            visitante_placar = x$placar_oficial_visitante,
            label = x$transmissao$label) %>%
  filter(!is.na(mandante_placar),
         !is.na(visitante_placar)) %>%
  mutate(mandante = case_when(mandante == 'Sport' ~ 'Sport Recife',
                              mandante == 'Bragantino' ~ 'RB Bragantino',
                              TRUE ~ mandante),
         visitante = case_when(visitante == 'Sport' ~ 'Sport Recife',
                               visitante == 'Bragantino' ~ 'RB Bragantino',
                              TRUE ~ visitante)) %>%
  mutate(vencedor = case_when(mandante_placar > visitante_placar ~ mandante,
                              mandante_placar < visitante_placar ~ visitante,
                              mandante_placar == visitante_placar ~ 'Empate'))-> resultados


df <- df %>%
  left_join(resultados %>%
              rename_all(~paste0(.,'_gaba')),
            by = c('mandante' = 'mandante_gaba', 'visitante' = 'visitante_gaba')) %>%
  filter(!is.na(label_gaba)) %>%
  left_join(jogos_atual %>%
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
  rename(label = label_gaba) %>%
  select(-contains('_gaba'))


confrontos_atual %>%
  gather(key = key, value = jogador, -rodada, -rodada_bolao, -confronto) %>%
  arrange(rodada_bolao,key,confronto) %>%
  left_join(df %>%
              filter(label %in% c('veja como foi',
                                  'acompanhe em tempo real')) %>%
              group_by(jogador) %>%
              summarise(pontos_pro = sum(pontos),
                        cravadas_pro = sum(cravada)),
            by = c('jogador')) %>%
  select(-jogador) %>%
  pivot_wider(names_from = c('key'),
              values_from = c('pontos_pro',
                              'cravadas_pro')) %>%
  separate(confronto, c('jogador_a', 'jogador_b'), ' x ') %>%
  left_join(classificacao %>%
              mutate(posicao_a = row_number()) %>%
              select(Jogador,
                     posicao_a),
            by = c('jogador_a' = 'Jogador')) %>%
  left_join(classificacao %>%
              mutate(posicao_b = row_number()) %>%
              select(Jogador,
                     posicao_b),
            by = c('jogador_b' = 'Jogador')) %>%
  mutate(pontos_pro_jogador_a = ifelse(is.na(pontos_pro_jogador_a),0,pontos_pro_jogador_a),
         pontos_pro_jogador_b = ifelse(is.na(pontos_pro_jogador_b),0,pontos_pro_jogador_b),
         cravadas_pro_jogador_a = ifelse(is.na(cravadas_pro_jogador_a),0,cravadas_pro_jogador_a),
         cravadas_pro_jogador_b = ifelse(is.na(cravadas_pro_jogador_b),0,cravadas_pro_jogador_b))-> parcial_andamento

confrontos_atual %>%
  gather(key = key, value = jogador, -rodada, -rodada_bolao, -confronto) %>%
  arrange(rodada_bolao,key,confronto) %>%
  left_join(df %>%
              filter(label %in% c('veja como foi')) %>%
              group_by(jogador) %>%
              summarise(pontos_pro = sum(pontos),
                        cravadas_pro = sum(cravada)),
            by = c('jogador')) %>%
  select(-jogador) %>%
  pivot_wider(names_from = c('key'),
              values_from = c('pontos_pro',
                              'cravadas_pro')) %>%
  separate(confronto, c('jogador_a', 'jogador_b'), ' x ') %>%
  left_join(classificacao %>%
              mutate(posicao_a = row_number()) %>%
              select(Jogador,
                     posicao_a),
            by = c('jogador_a' = 'Jogador')) %>%
  left_join(classificacao %>%
              mutate(posicao_b = row_number()) %>%
              select(Jogador,
                     posicao_b),
            by = c('jogador_b' = 'Jogador')) %>%
  mutate(pontos_pro_jogador_a = ifelse(is.na(pontos_pro_jogador_a),0,pontos_pro_jogador_a),
         pontos_pro_jogador_b = ifelse(is.na(pontos_pro_jogador_b),0,pontos_pro_jogador_b),
         cravadas_pro_jogador_a = ifelse(is.na(cravadas_pro_jogador_a),0,cravadas_pro_jogador_a),
         cravadas_pro_jogador_b = ifelse(is.na(cravadas_pro_jogador_b),0,cravadas_pro_jogador_b)) -> parcial_consolidada


paste0('<b>*Parciais em andamento*</b><br/><br/>',
       '[',parcial_andamento$posicao_a[1],'] ',
       parcial_andamento$jogador_a[1],' ','(',parcial_andamento$cravadas_pro_jogador_a[1],')',' ',parcial_andamento$pontos_pro_jogador_a[1],
       ' x ',
       parcial_andamento$pontos_pro_jogador_b[1],' ','(',parcial_andamento$cravadas_pro_jogador_b[1],') ',parcial_andamento$jogador_b[1],
       ' [',parcial_andamento$posicao_b[1],']',
       '<br/>',
       '[',parcial_andamento$posicao_a[2],'] ',
       parcial_andamento$jogador_a[2],' ','(',parcial_andamento$cravadas_pro_jogador_a[2],')',' ',parcial_andamento$pontos_pro_jogador_a[2],
       ' x ',
       parcial_andamento$pontos_pro_jogador_b[2],' ','(',parcial_andamento$cravadas_pro_jogador_b[2],') ',parcial_andamento$jogador_b[2],
       ' [',parcial_andamento$posicao_b[2],']',
       '<br/>',
       '[',parcial_andamento$posicao_a[3],'] ',
       parcial_andamento$jogador_a[3],' ','(',parcial_andamento$cravadas_pro_jogador_a[3],')',' ',parcial_andamento$pontos_pro_jogador_a[3],
       ' x ',
       parcial_andamento$pontos_pro_jogador_b[3],' ','(',parcial_andamento$cravadas_pro_jogador_b[3],') ',parcial_andamento$jogador_b[3],
       ' [',parcial_andamento$posicao_b[3],']',
       '<br/>',
       '[',parcial_andamento$posicao_a[4],'] ',
       parcial_andamento$jogador_a[4],' ','(',parcial_andamento$cravadas_pro_jogador_a[4],')',' ',parcial_andamento$pontos_pro_jogador_a[4],
       ' x ',
       parcial_andamento$pontos_pro_jogador_b[4],' ','(',parcial_andamento$cravadas_pro_jogador_b[4],') ',parcial_andamento$jogador_b[4],
       ' [',parcial_andamento$posicao_b[4],']',
       '<br/>',
       '[',parcial_andamento$posicao_a[5],'] ',
       parcial_andamento$jogador_a[5],' ','(',parcial_andamento$cravadas_pro_jogador_a[5],')',' ',parcial_andamento$pontos_pro_jogador_a[5],
       ' x ',
       parcial_andamento$pontos_pro_jogador_b[5],' ','(',parcial_andamento$cravadas_pro_jogador_b[5],') ',parcial_andamento$jogador_b[5],
       ' [',parcial_andamento$posicao_b[5],']',
       '<br/>',
       '[',parcial_andamento$posicao_a[6],'] ',
       parcial_andamento$jogador_a[6],' ','(',parcial_andamento$cravadas_pro_jogador_a[6],')',' ',parcial_andamento$pontos_pro_jogador_a[6],
       ' x ',
       parcial_andamento$pontos_pro_jogador_b[6],' ','(',parcial_andamento$cravadas_pro_jogador_b[6],') ',parcial_andamento$jogador_b[6],
       ' [',parcial_andamento$posicao_b[6],']',
       '<br/>',
       '[',parcial_andamento$posicao_a[7],'] ',
       parcial_andamento$jogador_a[7],' ','(',parcial_andamento$cravadas_pro_jogador_a[7],')',' ',parcial_andamento$pontos_pro_jogador_a[7],
       ' x ',
       parcial_andamento$pontos_pro_jogador_b[7],' ','(',parcial_andamento$cravadas_pro_jogador_b[7],') ',parcial_andamento$jogador_b[7],
       ' [',parcial_andamento$posicao_b[7],']') %>%
  writeLines(.,
             paste0('./insumos/parciais_andamento.html'))

paste0('<b>*Parciais consolidadas*</b><br/><br/>',
       '[',parcial_consolidada$posicao_a[1],'] ',
       parcial_consolidada$jogador_a[1],' ','(',parcial_consolidada$cravadas_pro_jogador_a[1],')',' ',parcial_consolidada$pontos_pro_jogador_a[1],
       ' x ',
       parcial_consolidada$pontos_pro_jogador_b[1],' ','(',parcial_consolidada$cravadas_pro_jogador_b[1],') ',parcial_consolidada$jogador_b[1],
       ' [',parcial_consolidada$posicao_b[1],']',
       '<br/>',
       '[',parcial_consolidada$posicao_a[2],'] ',
       parcial_consolidada$jogador_a[2],' ','(',parcial_consolidada$cravadas_pro_jogador_a[2],')',' ',parcial_consolidada$pontos_pro_jogador_a[2],
       ' x ',
       parcial_consolidada$pontos_pro_jogador_b[2],' ','(',parcial_consolidada$cravadas_pro_jogador_b[2],') ',parcial_consolidada$jogador_b[2],
       ' [',parcial_consolidada$posicao_b[2],']',
       '<br/>',
       '[',parcial_consolidada$posicao_a[3],'] ',
       parcial_consolidada$jogador_a[3],' ','(',parcial_consolidada$cravadas_pro_jogador_a[3],')',' ',parcial_consolidada$pontos_pro_jogador_a[3],
       ' x ',
       parcial_consolidada$pontos_pro_jogador_b[3],' ','(',parcial_consolidada$cravadas_pro_jogador_b[3],') ',parcial_consolidada$jogador_b[3],
       ' [',parcial_consolidada$posicao_b[3],']',
       '<br/>',
       '[',parcial_consolidada$posicao_a[4],'] ',
       parcial_consolidada$jogador_a[4],' ','(',parcial_consolidada$cravadas_pro_jogador_a[4],')',' ',parcial_consolidada$pontos_pro_jogador_a[4],
       ' x ',
       parcial_consolidada$pontos_pro_jogador_b[4],' ','(',parcial_consolidada$cravadas_pro_jogador_b[4],') ',parcial_consolidada$jogador_b[4],
       ' [',parcial_consolidada$posicao_b[4],']',
       '<br/>',
       '[',parcial_consolidada$posicao_a[5],'] ',
       parcial_consolidada$jogador_a[5],' ','(',parcial_consolidada$cravadas_pro_jogador_a[5],')',' ',parcial_consolidada$pontos_pro_jogador_a[5],
       ' x ',
       parcial_consolidada$pontos_pro_jogador_b[5],' ','(',parcial_consolidada$cravadas_pro_jogador_b[5],') ',parcial_consolidada$jogador_b[5],
       ' [',parcial_consolidada$posicao_b[5],']',
       '<br/>',
       '[',parcial_consolidada$posicao_a[6],'] ',
       parcial_consolidada$jogador_a[6],' ','(',parcial_consolidada$cravadas_pro_jogador_a[6],')',' ',parcial_consolidada$pontos_pro_jogador_a[6],
       ' x ',
       parcial_consolidada$pontos_pro_jogador_b[6],' ','(',parcial_consolidada$cravadas_pro_jogador_b[6],') ',parcial_consolidada$jogador_b[6],
       ' [',parcial_consolidada$posicao_b[6],']',
       '<br/>',
       '[',parcial_consolidada$posicao_a[7],'] ',
       parcial_consolidada$jogador_a[7],' ','(',parcial_consolidada$cravadas_pro_jogador_a[7],')',' ',parcial_consolidada$pontos_pro_jogador_a[7],
       ' x ',
       parcial_consolidada$pontos_pro_jogador_b[7],' ','(',parcial_consolidada$cravadas_pro_jogador_b[7],') ',parcial_consolidada$jogador_b[7],
       ' [',parcial_consolidada$posicao_b[7],']') %>%
  writeLines(.,
             paste0('./insumos/parciais_consolidadas.html'))
