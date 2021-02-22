jogos <- readRDS('./insumos/jogos.RDS')
confrontos <- readRDS('./insumos/confrontos.RDS')

# Classificação -----------------------------------------------------------
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


palpites %>%
  select(rodada_bolao,
         jogador,
         mandante,
         visitante,
         cravada) %>%
  gather(key = mando, value = time, mandante, visitante) %>%
  group_by(jogador, time) %>%
  summarise(cravada = sum(cravada, na.rm = T)) %>%
  ungroup() %>%
  plot_ly(data = ., type = 'bar', x=~jogador, y=~cravada, color = ~time) %>%
  layout(barmode = 'stack',
         yaxis = list(title = 'Cravadas'),
         xaxis = list(title='Jogadores')) -> cravadas_jogador


palpites %>%
  select(rodada_bolao,
         jogador,
         mandante,
         visitante,
         cravada) %>%
  gather(key = mando, value = time, mandante, visitante) %>%
  group_by(jogador, time) %>%
  summarise(cravada = sum(cravada)) %>%
  plot_ly(data = ., type = 'bar', x=~time, y=~cravada, color = ~jogador) %>%
  layout(barmode = 'stack',
         yaxis = list(title = 'Cravadas'),
         xaxis = list(title='Jogadores')) -> cravadas_time

palpites %>%
  select(rodada_bolao,
         jogador,
         mandante,
         visitante,
         pontos) %>%
  gather(key = mando, value = time, mandante, visitante) %>%
  group_by(jogador, time) %>%
  summarise(pontos = sum(pontos)) %>%
  ungroup() %>%
  plot_ly(data = ., type = 'bar', x=~jogador, y=~pontos, color = ~time) %>%
  layout(barmode = 'stack',
         yaxis = list(title = 'Pontos'),
         xaxis = list(title='Jogadores')) -> pontos_jogador

palpites %>%
  select(rodada_bolao,
         jogador,
         mandante,
         visitante,
         pontos) %>%
  gather(key = mando, value = time, mandante, visitante) %>%
  group_by(jogador, time) %>%
  summarise(pontos = sum(pontos)) %>%
  plot_ly(data = ., type = 'bar', x=~time, y=~pontos, color = ~jogador) %>%
  layout(barmode = 'stack',
         yaxis = list(title = 'Pontos'),
         xaxis = list(title='Jogadores')) -> pontos_time

palpites %>%
  select(rodada_bolao,
         jogador,
         mandante,
         visitante,
         pontos) %>%
  gather(key = mando, value = time, mandante, visitante) %>%
  group_by(jogador, time) %>%
  summarise(pontos = sum(pontos)) %>%
  ungroup() %>%
  group_by(time) %>%
  top_n(1, wt = pontos) %>%
  rename_all(~str_to_title(.)) %>%
  ggplot(aes(x=Time, y = Pontos, fill = Jogador)) +
  geom_bar(position='dodge', stat = 'identity') +
  ggrepel::geom_label_repel(aes(y=Pontos, label = Jogador),
                            position = 'dodge', show.legend = FALSE) +
  ggpubr::theme_pubclean() +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(x = 'Times', y = 'Pontos', fill = 'Jogadores') -> maior_pontuador_por_time


# palpites %>%
#   select(rodada_bolao,
#          jogador,
#          mandante,
#          visitante,
#          pontos) %>%
#   gather(key = mando, value = time, mandante, visitante) %>%
#   group_by(jogador, time, mando) %>%
#   summarise(pontos = sum(pontos)) %>%
#   ungroup() %>%
#   filter(mando == 'visitante') %>%
#   group_by(time) %>%
#   top_n(1, wt = pontos) %>%
#   ggplot(aes(x=time, y = pontos, fill = jogador)) +
#   geom_bar(position='dodge', stat = 'identity') +
#   ggrepel::geom_label_repel(aes(y=pontos, label = jogador),
#                             position = 'dodge', show.legend = FALSE) +
#   ggpubr::theme_pubclean() +
#   theme(axis.text.x = element_text(angle = 45)) +
#   labs(x = 'Times', y = 'Pontos', fill = 'Jogadores')

palpites %>%
  select(rodada_bolao,
         jogador,
         mandante,
         visitante,
         pontos) %>%
  gather(key = mando, value = time, mandante, visitante) %>%
  group_by(jogador, time) %>%
  summarise(pontos = sum(pontos)) %>%
  ungroup() %>%
  group_by(jogador) %>%
  top_n(1, wt = pontos) %>%
  rename_all(~str_to_title(.)) %>%
  ggplot(aes(x=Jogador, y = Pontos, fill = Time)) +
  geom_bar(position='dodge', stat = 'identity') +
  ggrepel::geom_label_repel(aes(y=Pontos, label = Time),
                            position = 'dodge', show.legend = FALSE) +
  ggpubr::theme_pubclean() +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(x = 'Jogadores', y = 'Pontos', fill = 'Times') -> maior_time_por_jogador

# palpites %>%
#   select(rodada_bolao,
#          jogador,
#          mandante,
#          visitante,
#          pontos) %>%
#   gather(key = mando, value = time, mandante, visitante) %>%
#   group_by(jogador, time) %>%
#   summarise(pontos = sum(pontos)) %>%
#   ungroup() %>%
#   group_by(jogador) %>%
#   mutate(total = sum(pontos),
#          pontos = pontos/total) %>%
#   mutate(pontos = scales::percent(pontos)) %>%
  # select(-total) %>%
  # spread(key=time, value = pontos) %>%
  # janitor::clean_names() %>%
  # rename_all(~str_replace_all(.,'_','-')) %>%
  # rename_all(~str_to_title(.)) %>%
  # htmlTable::htmlTable(.,
  #                      collapse = 'separate_shiny',
  #                      spacing = '2px')
  # ggplot(aes(y=pontos, x=jogador, fill = time))+
  # geom_bar(stat = 'identity', position = 'stack') +
  # ggpubr::theme_pubclean() +
  # theme(axis.text.x = element_text(angle = 45)) +
  # labs(x = 'Jogadores', y = 'Pontos', fill = 'Times')
  # plot_ly(data = ., type = 'bar', x=~jogador, y=~scales::percent(pontos), color = ~time) %>%
  # layout(barmode = 'stack',
  #        yaxis = list(title = 'Pontos'),
  #        xaxis = list(title='Jogadores'))

ggplotly(confrontos %>%
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
  group_by(rodada_bolao, jogador) %>%
  summarise(pontos = sum(pontos)) %>%
  ungroup() %>%
  group_by(jogador) %>%
  mutate(pontos_total = cumsum(pontos)) %>%
  ungroup() %>%
  ggplot(aes(y=pontos_total, x=rodada_bolao, color = jogador, text = paste('Pontos: ', pontos_total,
                                                                           '<br>Rodada:', rodada_bolao,
                                                                           '<br>Jogador:', jogador))) +
  geom_point() +
  geom_line(aes(group=jogador)) +
  ggpubr::theme_pubclean() +
  theme(legend.position = 'bottom') +
  labs(x = 'Rodada', y = 'Pontos', color = 'Jogadores'), tooltip = c('text')) -> evolucao_pontos

lapply(as.list(3:27), function(x){
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
    filter(as.numeric(rodada)<x,
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
           Sequencia) %>%
    mutate(Rodada = x-2,
           Posicao = row_number())
}) %>%
  Reduce(f=rbind) -> x

# library(gganimate)
# 
# 
# x %>%
#   mutate(Posicao = factor(Posicao, levels = order(Posicao, decreasing = T))) %>%
#   ggplot(aes(y=Posicao, x=Rodada)) +
#   geom_line(aes(group=Jogador, color = Jogador), show.legend = FALSE) +
#   facet_wrap(~Jogador) +
#   ggpubr::theme_pubclean() +
#   transition_reveal(Rodada) -> trajetoria_anim

# anim_save('trejetoria_1t.gif', animation = last_animation())


x %>%
  mutate(Posicao = factor(Posicao, levels = order(Posicao, decreasing = T))) %>%
  ggplot(aes(y=Posicao, x=Rodada)) +
  geom_line(aes(group=Jogador, color = Jogador), show.legend = FALSE) +
  ggpubr::theme_pubclean() -> trajetoria_1t

# (palpites %>%
#   group_by(rodada_bolao,
#            jogador) %>%
#   summarise(pontos_pro = sum(pontos)) %>%
#   ungroup() %>%
#   group_by(jogador) %>%
#   mutate(pontos_pro = cumsum(pontos_pro)) %>%
#   arrange(jogador) %>%
#   filter(jogador %in% c('B.o',
#                         'Bara')) %>%
#   ggplot(aes(y=pontos_pro, x=rodada_bolao)) +
#   geom_line(aes(group=jogador, color = jogador), show.legend = FALSE) +
#   geom_label(aes(y=pontos_pro, label=jogador, x = rodada_bolao)) +
#   ggpubr::theme_pubclean() +
#   labs(y = 'Pontos pró', x = 'Rodada') +
#   transition_reveal(rodada_bolao) + ease_aes() +
#     enter_fade() +
#     exit_fade()) %>%
#   animate(nframes=100)
# 
# anim_save('trajetoria_pp.gif', animation = last_animation())
# 
# 

# (palpites %>%
#   mutate_at(vars('rodada_bolao',
#                  'jogador'), ~as.factor(.)) %>%
#   group_by(rodada_bolao,
#            jogador,.drop = F) %>%
#   summarise(pontos_pro = sum(pontos, na.rm = T)) %>%
#   ungroup() %>%
#   arrange(rodada_bolao) %>%
#   group_by(jogador) %>%
#   mutate(pontos_pro = cumsum(pontos_pro)) %>%
#   ungroup() %>%
#   group_by(rodada_bolao) %>%
#   arrange(rodada_bolao, -pontos_pro) %>%
#   mutate(rank = row_number()) %>%
#   ggplot() +
#   geom_bar(aes(y=pontos_pro, x=desc(rank), fill = jogador),
#            stat = 'identity', show.legend = FALSE)+
#   geom_text(aes(y=pontos_pro+20, label=as.character(pontos_pro),
#                  x = desc(rank)), show.legend = FALSE) +
#   geom_label(aes(y=0, label=jogador,
#                 x = desc(rank), color = jogador),
#              show.legend = FALSE) +
#   labs(y = 'Pontos pró', x = 'Jogador', title = 'Rodada: {closest_state}') +
#   coord_flip(clip = "off", expand = FALSE) +
#   ylim(-200,1400) +
#   guides(color = FALSE, fill = FALSE) +
#   theme(axis.line=element_blank(),
#         axis.text.x=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks=element_blank(),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank(),
#         legend.position="none",
#         panel.background=element_blank(),
#         panel.border=element_blank(),
#         panel.grid.major=element_blank(),
#         panel.grid.minor=element_blank(),
#         panel.grid.major.x = element_line( size=.1, color="grey" ),
#         panel.grid.minor.x = element_line( size=.1, color="grey" ),
#         plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
#         plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
#         plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
#         plot.background=element_blank(),
#         plot.margin = margin(2,2, 2, 4, "cm")) +
#   # facet_wrap(~rodada_bolao)
#   transition_states(rodada_bolao,
#                     transition_length = 4, state_length = 1) +
#   view_follow(fixed_x = TRUE) +
#   enter_fade() +
#   exit_fade()) %>%
#   animate(200, fps = 20)
# 
# anim_save('trajetoria_pp_geral.gif', animation = last_animation())
