message( paste( rep('-', 100 ), collapse = '' ) )

# Plantilla gráfica --------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )

# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_RTR_inversiones.RData' ) )

#Portafolio de inversiones -------------------------------------------------------------------------
message( '\tGraficando evolución histórica de las inversiones en valor nominal' )

aux <- recurs_adm_biess %>%
  filter( ano > 2011 ) %>%
  mutate( inversiones:=inversiones,
          rendimiento_neto = rendimiento_neto*100,
          periodo = as.Date( paste0( "01/01/",ano), "%d/%m/%Y" ),
          instrumento = 'Inversiones') %>%
  dplyr::select( periodo,
                 inversiones,
                 rendimiento_neto,
                 instrumento )

df_bar <- aux %>% select(-rendimiento_neto)
df_line = aux %>% select(periodo, rendimiento_neto)

scl = 1000000  # escala de millones
hmts = 30 #homotecia

y_lim <- c( 0, 1400000000 )
y_brk <- seq( y_lim[1], y_lim[2], 200000000 )
y_lbl <- formatC( y_brk/scl, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

y_lim_dual <- c( 6, 11 )
ydual_brk <- seq(6,11,1)
ydual_lbl <- paste0(formatC( ydual_brk, 
                              digits = 0, 
                              format = 'f', 
                              big.mark = '.', 
                              decimal.mark = ',' ),"%")


iess_inversiones_rnd_neto <- ggplot(data = df_bar, 
                                       aes(x = periodo,
                                           y = inversiones,
                                           fill = instrumento)) +
                             geom_bar(stat='identity',
                                      colour='black') +
                             geom_line(data = df_line,
                                       aes(x = periodo,
                                           y = rendimiento_neto*hmts*scl*10 - 1800000000,
                                           group = 1,
                                           linetype = 'Rendimiento Neto'),
                                       inherit.aes = FALSE,
                                       size=1 ) +
                             scale_linetype_manual(NULL, values = 1) +
                             scale_x_date(date_breaks = '1 year', date_labels = '%Y')+
                             scale_y_continuous(name = 'Saldo (millones USD)',
                                                labels = y_lbl, breaks = y_brk, limits = y_lim,
                                                sec.axis = sec_axis(~./(scl*hmts*10) + 6,
                                                                   name = 'Rendimiento Neto',
                                                                   labels = ydual_lbl,
                                                                   breaks = ydual_brk)) + 
                             scale_fill_manual(values = c(parametros$iess_green,
                                                          parametros$iess_blue))+
                             theme_bw() +
                             plt_theme+
                             guides(fill = guide_legend(title = NULL,
                                                       label.position = "right", 
                                                       label.hjust = 0))+
                             theme(legend.position='bottom') +
                             labs( x = '', y = '' )+
                             theme(legend.background = element_rect(fill = 'transparent'),
                                   legend.box.background = element_rect(fill = 'transparent', 
                                                                       colour = NA),
                                   legend.key = element_rect(fill = 'transparent'), 
                                   legend.spacing = unit(-1, 'lines') )


ggsave( plot = iess_inversiones_rnd_neto, 
        filename = paste0( parametros$resultado_graficos, 'iess_evol_hist_inver_valor_nominal_rtr', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Evolución histórica de las inversiones en préstamos quirografarios---------------------------------
message( '\tGraficando evolución histórica de las inversiones en préstamos quirografarios ' )
aux <- inv_instrumento %>%
  filter( instrumento == 'Créditos Quirografarios' ) %>% 
  dplyr::select(ano,
                instrumento,
                valor_nominal,
                rdto_prom_pond ) %>%
  mutate( periodo = ymd( paste0(ano, '/01/01') ) )

df_bar <- aux %>% select(-rdto_prom_pond)
df_line = aux %>% select(periodo, rdto_prom_pond)

scl = 10000000  # escala de millones
hmts = 130 #homotecia

y_lim <- c( 0, 600000000 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 8 )
y_lbl <- formatC( y_brk/1000000, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )
ydual_brk<-seq(0.08,0.13,length.out = 6)
ydual_lbl <- paste0(formatC( ydual_brk*100, 
                             digits = 1, 
                             format = 'f', 
                             big.mark = '.', 
                             decimal.mark = ',' ),"%")


iess_prestamos_quirografarios <- ggplot(data = df_bar, 
                                       aes(x = periodo,
                                           y = valor_nominal,
                                           fill = instrumento)) +
                                  geom_bar(stat='identity',
                                           colour='black') +
                                  geom_line(data = df_line,
                                            aes(x = periodo,
                                                y = rdto_prom_pond*hmts*scl*10 - 1100000000,
                                                group = 1,
                                                linetype = 'Rendimiento Neto'),
                                            inherit.aes = FALSE,
                                            size=1 )  +
                                  scale_linetype_manual(NULL, values = 1) +
                                  scale_x_date(date_breaks = '1 year', date_labels = '%Y')+
                                  scale_y_continuous(name = 'Saldo (millones USD)',
                                                     labels = y_lbl, breaks = y_brk, limits = y_lim,
                                                     sec.axis = sec_axis(~./(scl*hmts*10) + 0.08461538,
                                                                         name = 'Rendimiento Promedio Ponderado',
                                                                         labels = ydual_lbl,
                                                                         breaks = ydual_brk)) + 
                                  scale_fill_manual(values = c(parametros$iess_green,
                                                               parametros$iess_blue))+
                                  theme_bw() +
                                  plt_theme+
                                  guides(fill = guide_legend(title = NULL,
                                                             label.position = "right", 
                                                             label.hjust = 0))+
                                  theme(legend.position='bottom') +
                                  labs( x = '', y = '' )+
                                  theme(legend.background = element_rect(fill = 'transparent'),
                                        legend.box.background = element_rect(fill = 'transparent', 
                                                                             colour = NA),
                                        legend.key = element_rect(fill = 'transparent'), 
                                        legend.spacing = unit(-1, 'lines') )
                                
ggsave( plot = iess_prestamos_quirografarios, 
        filename = paste0( parametros$resultado_graficos, 'iess_prestamos_quirografarios_rtr', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Evolución histórica de las inversiones en titularizaciones-----------------------------------------
message( '\tGraficando evolución histórica de las inversiones en titularizaciones' )

aux <- inv_instrumento %>% 
  filter(instrumento=='Titularizaciones') %>% 
  dplyr::select(ano,
                instrumento,
                valor_nominal,
                rdto_prom_pond ) %>%
  mutate( periodo = ymd( paste0(ano, '/01/01') ) )

df_bar <- aux %>% select(-rdto_prom_pond)
df_line = aux %>% select(periodo, rdto_prom_pond)

scl = 1000000  # escala de millones
hmts = 20 #homotecia

y_lim <- c( 0, 60000000 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk/1000000, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )
ydual_brk <- seq(0.07, 0.10,length.out = 6)
ydual_lbl <- paste0(formatC( ydual_brk*100, 
                             digits = 1, 
                             format = 'f', 
                             big.mark = '.', 
                             decimal.mark = ',' ),"%")


iess_titularizaciones <- ggplot(data = df_bar, aes(x = periodo,
                                                            y = valor_nominal, 
                                                            fill = instrumento)) +
                                  geom_bar(stat='identity',
                                           colour='black') +
                                  geom_line(data = df_line,
                                            aes(x = periodo,
                                                y = rdto_prom_pond*hmts*scl*100 - 135000000,
                                                group = 1,
                                                linetype = 'Rendimiento Neto'),
                                            inherit.aes = FALSE,
                                            size=1 )  +
                                  scale_linetype_manual(NULL, values = 1) +
                                  scale_x_date(date_breaks = '1 year', date_labels = '%Y')+
                                  scale_y_continuous(name = 'Saldo (millones USD)',
                                                     labels = y_lbl, breaks = y_brk, limits = y_lim,
                                                     sec.axis = sec_axis(~./(scl*hmts*100) + 0.0675,
                                                                         name = 'Rendimiento Promedio Ponderado',
                                                                         labels = ydual_lbl,
                                                                         breaks = ydual_brk)) + 
                                  scale_fill_manual(values = c(parametros$iess_green, 
                                                               parametros$iess_blue))+
                                  theme_bw() +
                                  plt_theme+
                                  guides( color = guide_colorbar(order = 0),
                                          fill = guide_legend(order = 1) ) + 
                                  theme(legend.position='bottom') +
                                  labs( x = '', y = '' )+
                                  theme(legend.background = element_rect(fill = 'transparent'),
                                        legend.box.background = element_rect(fill = 'transparent', 
                                                                             colour = NA),
                                        legend.key = element_rect(fill = 'transparent'), 
                                        legend.spacing = unit(-1, 'lines') )

ggsave( plot = iess_titularizaciones, 
        filename = paste0( parametros$resultado_graficos, 'iess_inversiones_titularizaciones_rtr', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Evolución histórica de las inversiones en obligaciones---------------------------------------------
message( '\tGraficando evolución histórica de las inversiones en obligaciones' )

aux <- inv_instrumento %>% 
  filter(instrumento=='Obligaciones') %>% 
  dplyr::select(ano,
                instrumento,
                valor_nominal,
                rdto_prom_pond ) %>%
  mutate( periodo = ymd( paste0(ano, '/01/01') ) )

df_bar <- aux %>% select(-rdto_prom_pond)
df_line = aux %>% select(periodo, rdto_prom_pond)

scl = 1000000  # escala de millones
hmts = 30 #homotecia

y_lim <- c( 0, 40000000 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk/1000000, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )
ydual_brk <- seq(0.078, 0.090,0.002 )
ydual_lbl <- paste0(formatC( ydual_brk*100, 
                             digits = 1, 
                             format = 'f', 
                             big.mark = '.', 
                             decimal.mark = ',' ),"%")

iess_inversiones_obligaciones <- ggplot(data = df_bar, aes(x = periodo,
                                                            y = valor_nominal, 
                                                            fill = instrumento)) +
                                geom_bar(stat='identity',colour='black') +
                                theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                                geom_line(data = df_line,
                                          aes(x = periodo,
                                              y = rdto_prom_pond*hmts*scl*100 - 230000000, 
                                              group = 1, linetype = 'Rendimiento Promedio Ponderado'),
                                          inherit.aes = FALSE,
                                          size=1) +
                                scale_linetype_manual(NULL, values = 1) +
                                scale_x_date(date_breaks = '1 year', date_labels = '%Y')+
                                scale_y_continuous(name = 'Saldo (millones USD)',
                                                   labels = y_lbl, breaks = y_brk, limits = y_lim,
                                                   sec.axis = sec_axis(~./(scl*hmts*100) + 0.07666667,
                                                                       name = 'Rendimiento Promedio Ponderado',
                                                                       labels = ydual_lbl,
                                                                       breaks = ydual_brk)) + 
                                scale_fill_manual(values = c(parametros$iess_green, 
                                                             parametros$iess_blue))+
                                theme_bw() +
                                plt_theme+
                                guides( color = guide_colorbar(order = 0),
                                        fill = guide_legend(order = 1) ) + 
                                theme(legend.position='bottom') +
                                labs( x = '', y = '' )+
                                theme(legend.background = element_rect(fill = 'transparent'),
                                      legend.box.background = element_rect(fill = 'transparent', 
                                                                           colour = NA),
                                      legend.key = element_rect(fill = 'transparent'), 
                                      legend.spacing = unit(-1, 'lines') )

ggsave( plot = iess_inversiones_obligaciones, 
        filename = paste0( parametros$resultado_graficos, 'iess_inversiones_obligaciones_rtr', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Evolución histórica de las inversiones en bonos del estado-----------------------------------------
message( '\tGraficando evolución histórica de las inversiones en bonos del estado' )


aux <- inv_instrumento %>% 
  filter(instrumento=='Bonos del Estado') %>% 
  dplyr::select(ano,
                instrumento,
                valor_nominal,
                rdto_prom_pond ) %>%
  mutate( periodo = ymd( paste0(ano, '/01/01') ) )

df_bar <- aux %>% select(-rdto_prom_pond)
df_line = aux %>% select(periodo, rdto_prom_pond)

scl = 10000000  # escala de millones
hmts = 60 #homotecia

y_lim <- c( 0, 600000000 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk/1000000, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )
ydual_brk <- seq(0.06, 0.074,0.001 )
ydual_lbl <- paste0(formatC( ydual_brk*100, 
                             digits = 1, 
                             format = 'f', 
                             big.mark = '.', 
                             decimal.mark = ',' ),"%")

iess_inversiones_bonos <- ggplot(data = df_bar, aes(x = periodo,
                                                           y = valor_nominal, 
                                                           fill = instrumento)) +
                          geom_bar(stat='identity',colour='black') +
                          theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                          geom_line(data = df_line,
                                    aes(x = periodo,
                                        y = rdto_prom_pond*hmts*scl*100 - 1900000000*1.96, 
                                        group = 1, linetype = 'Rendimiento Promedio Ponderado'),
                                    inherit.aes = FALSE,
                                    size=1) +
                          scale_linetype_manual(NULL, values = 1) +
                          scale_x_date(date_breaks = '1 year', date_labels = '%Y')+
                          scale_y_continuous(name = 'Saldo (millones USD)',
                                             labels = y_lbl, breaks = y_brk, limits = y_lim,
                                             sec.axis = sec_axis(~./(scl*hmts*100) + 0.06206667,
                                                                 name = 'Rendimiento Promedio Ponderado',
                                                                 labels = ydual_lbl,
                                                                 breaks = ydual_brk)) + 
                          scale_fill_manual(values = c(parametros$iess_green, 
                                                       parametros$iess_blue))+
                          theme_bw() +
                          plt_theme+
                          guides( color = guide_colorbar(order = 0),
                                  fill = guide_legend(order = 1) ) + 
                          theme(legend.position='bottom') +
                          labs( x = '', y = '' )+
                          theme(legend.background = element_rect(fill = 'transparent'),
                                legend.box.background = element_rect(fill = 'transparent', 
                                                                     colour = NA),
                                legend.key = element_rect(fill = 'transparent'), 
                                legend.spacing = unit(-1, 'lines') )


ggsave( plot = iess_inversiones_bonos, 
        filename = paste0( parametros$resultado_graficos, 'iess_inversiones_bonos_rtr', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Limpiando Ram--------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()

