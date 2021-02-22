jogos <- readRDS('./insumos/jogos.RDS')
confrontos <- readRDS('./insumos/confrontos.RDS')

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
                                   tz = "America/Sao_Paulo")  - 3600,
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
  mutate(jogador = trimws(jogador)) %>%
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
               select(-jjj) %>%
               mutate(jogador = trimws(jogador)),
             by = c('jogador', 'rodada')) %>%
  mutate(esqueceu = as.numeric(is.na(body))) -> df

confrontos_atual <- confrontos %>%
  filter(rodada == rodada_atual)

for (i in 1:dim(confrontos_atual)[1]){
  jogador_a <- df[df$jogador == pull(confrontos_atual[i,"jogador_a"]),]
  jogador_b <- df[df$jogador == pull(confrontos_atual[i,"jogador_b"]),]
  
  cabecalho <- paste0('<b>*Rodada ', rodada_atual,' do Brasileirão 2020*<br/>\n',
                      '<b>*Rodada ', rodada_atual-1,' do Bolão*<br/>\n',
                      '<b>*______*<br/>\n*',
                      jogador_a$jogador,
                      ' x ',
                      jogador_b$jogador, '*<br/>',
                      '\n*______*</b><br/>\n<br/>')
  
  jogos_a <- if(!is.na(df[df$jogador==jogador_a$jogador,]$body)){
    rvest::html_table(xml2::read_html(df[df$jogador==jogador_a$jogador,]$body))[[1]] %>%
      rename(jogo = 1)
  } else {
      as.character('Esqueceu')
    }
  
  jogos_b <-if(!is.na(df[df$jogador==jogador_b$jogador,]$body)){
    rvest::html_table(xml2::read_html(df[df$jogador==jogador_b$jogador,]$body))[[1]] %>%
      rename(jogo = 1)
  } else {
    as.character('Esqueceu')
  }
  
  dias <- unique(jogos_atual %>%
                   filter(valido == 1) %>%
                   arrange(data) %>%
                   .[, "dia_semana"] %>%
                   pull())

# Jogador A
list <- c()

if(jogos_a == 'Esqueceu'){
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
                   jogador_a$jogador,
                   '*</b><br/>\n<br/>\n',
                   paste(list, collapse = "<br/>\n"))
  
  # Jogador B
  list <- c()
  
  if(jogos_b == 'Esqueceu'){
    list <- jogos_b
  } else {
    for (j in 1:length(dias)){
      list[[j]] <- jogos_b %>%
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
  
  palp_b <- paste0('<b>*',
                   jogador_b$jogador,
                   '*</b><br/>\n<br/>\n',
                   paste(list, collapse = "<br/>\n"))  
  
  writeLines(paste(cabecalho, palp_a, '<br/>', palp_b),
             paste0('./insumos/confronto_',i,'.html'))
  
}

apply(df, 1, function(x){
  if(!is.na(x['body'])){
    x['body'] %>%
      xml2::read_html() %>%
      rvest::html_table() %>%
      .[[1]] %>%
      janitor::clean_names() %>%
      rename(confronto = x) %>%
      mutate(jogador = x['jogador']) %>%
      select(jogador,
             everything())
    }}) %>%
  Reduce(f=rbind) %>%
  saveRDS(paste0('./insumos/palpites/palpites_rodada_',rodada_atual,'.RDS'))

