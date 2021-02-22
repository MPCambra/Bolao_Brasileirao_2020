# Detectar rodada ---------------------------------------------------------
jogos <- readRDS('./insumos/jogos.RDS')
confrontos <- readRDS('./insumos/confrontos.RDS')
rodada_atual <- min(jogos %>%
                      filter(valido == 1,
                             end > Sys.time()  %>%
                               as.POSIXct(tz = "America/Sao_Paulo")) %>%
                      select(Rodada) %>%
                      pull())

jogos_atual <- jogos[jogos$Rodada==rodada_atual,]

confrontos_atual <- confrontos %>%
  filter(rodada == rodada_atual) %>%
  mutate(confronto = paste(jogador_a,'x',jogador_b))


# Compilar palpites -------------------------------------------------------
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
                                   tz = "America/Sao_Paulo"),
              body = x %>%
                gm_message() %>%
                gm_body() %>%
                .[[1]])  
  }) %>%
  Reduce(f=rbind) %>%
  filter(str_detect(subject,paste0('Rodada ', rodada_atual))) %>%
  separate(subject, c('jogador','rodada'), ' - Rodada ') %>%
  filter(jogador == credentials[which(credentials$username_id == input$userName), 'nome']) %>%
  group_by(jogador) %>%
  top_n(1, wt = data) %>%
  ungroup() %>%
  mutate(rodada = as.numeric(rodada)) %>%
  mutate(esqueceu = as.numeric(is.na(body))) -> df


jogos_a <- if(!identical(df$body, character(0))){
  rvest::html_table(xml2::read_html(df$body))[[1]] %>%
    rename(jogo = 1)
} else {
  as.character('Não enviou ainda.')
}

dias <- unique(jogos_atual %>%
                 filter(valido == 1) %>%
                 arrange(data) %>%
                 .[, "dia_semana"] %>%
                 pull())

list <- c()

if(jogos_a == 'Não enviou ainda.'){
  list <- jogos_a
} else {
  for (j in 1:length(dias)){
    list[[j]] <- jogos_a %>%
      left_join(jogos_atual,
                by = c('mandante', 'visitante')) %>%
      filter(dia_semana == dias[j],
             valido == 1) %>%
      arrange(data) %>%
      apply(1,function(x){
        paste(x['hora'],
              '-',
              x['mandante'],
              x['mandante_placar'],
              'x',
              x['visitante_placar'],
              x['visitante'])
      }) %>%
      paste0(., collapse = '<br/>\n') %>%
      paste0('<b>*', dias[j],'*</b><br/>\n<br/>\n',.,'\n<br/>\n')}}

palp_a <- paste0('<b>*',
                 df$jogador,
                 '*</b><br/>\n<br/>\n',
                 paste(list, collapse = "<br/>\n"))

writeLines(palp_a,
           paste0('./insumos/palpite_atual_',df$jogador,'.html'))
