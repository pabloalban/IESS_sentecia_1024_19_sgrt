message( paste( rep('-', 100 ), collapse = '' ) )

# Plantilla gráfica --------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )

# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_RTR_analisis_financiero.RData' ) )

# Activo Fondo--------------------------------------------------------------------------------------
message( '\tGraficando activo del fondo' )
unidad<-1e6
aux <- as.data.table(activo_del_fondo)
aux[,Activo:=Activo/unidad]
x_lim <- c( 2012, 2021 )
x_brk <- 2012:2021
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 1400)
y_brk <- seq( y_lim[1], y_lim[2], 200 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_activo_fondo <- ggplot( data = aux ) + 
  geom_line( aes( x = Año, 
                  y = Activo, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Millones (USD)' ) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 0, hjust = 0.5 ) )

ggsave( plot = iess_activo_fondo, 
        filename = paste0( parametros$resultado_graficos, 'iess_activo_fondo_rtr',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Cuentas por cobrar del Fondo de RT ----------------------------------------------------------------
message( '\tGraficando Cuentas por cobrar del Fondo de RT ' )
unidad<-1e6
aux <- as.data.table( cuentas_cobrar_fondo )
aux <- aux[,`Cuentas por Cobrar`:=`Cuentas por Cobrar`/unidad]
x_lim <- c( 2012, 2021 )
x_brk <- 2012:2021
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 90)
y_brk <- seq( y_lim[1], y_lim[2], 10 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_cuentas_cobrar_fondo <-  ggplot( data = aux ) + 
  geom_line( aes( x = Año, 
                  y = `Cuentas por Cobrar`, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Millones (USD)' ) +
  scale_color_manual( values =  c( parametros$iess_green, 
                                   parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 0, hjust = 0.5 ) )

ggsave( plot = iess_cuentas_cobrar_fondo, 
        filename = paste0( parametros$resultado_graficos, 'iess_cuentas_cobrar_fondo_rtr', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Pasivo del Fondo de RT-----------------------------------------------------------------------------
message( '\tGraficando Pasivo del Fondo de RT.' )
unidad<-1e6
aux <- as.data.table( pasivos_fondo )
aux <- aux[,Pasivo:=Pasivo/unidad]
x_lim <- c( 2012, 2021 )
x_brk <- 2012:2021
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 50)
y_brk <- seq( y_lim[1], y_lim[2], 10 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_pasivo_fondo <-ggplot( data = aux ) + 
  geom_line( aes( x = Año, 
                  y = Pasivo, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Millones (USD)' ) +
  scale_color_manual( values =  c( parametros$iess_green, 
                                   parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 0, hjust = 0.5  ) )

ggsave( plot = iess_pasivo_fondo, 
        filename = paste0( parametros$resultado_graficos, 'iess_pasivo_fondo_rtr', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Cuentas por pagar del  Fondo de RT-----------------------------------------------------------------
message( '\tGraficando cuentas por pagar del Fondo de RT' )
unidad<-1e6
aux <- as.data.table( cuentas_pagar_fondo )
aux <- aux[,`Cuentas por Pagar`:=`Cuentas por Pagar`/unidad]
x_lim <- c( 2012, 2021 )
x_brk <- 2012:2021
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 50)
y_brk <- seq( y_lim[1], y_lim[2], 10 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_cuentas_pagar_fondo <- ggplot( data = aux ) + 
  geom_line( aes( x = Año, 
                  y = `Cuentas por Pagar`, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Millones (USD)') +
  scale_color_manual( values =  c( parametros$iess_green, 
                                   parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 0, hjust = 0.5  ) )

ggsave( plot = iess_cuentas_pagar_fondo, 
        filename = paste0( parametros$resultado_graficos, 'iess_cuentas_pagar_fondo_rtr', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Patrimonio del  Fondo de RT------------------------------------------------------------------------
message( '\tGraficando patrimonio del Fondo de RT.' )
unidad<-1e6
aux <- as.data.table( patrimonio_fondo )
aux <- aux[,Patrimonio:=Patrimonio/unidad]
x_lim <- c( 2012, 2021 )
x_brk <- 2012:2021
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 1500)
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6  )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_patrimonio_fondo <-  ggplot( data = aux ) + 
  geom_line( aes( x = Año, 
                  y = Patrimonio, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Millones (USD)') +
  scale_color_manual( values =  c( parametros$iess_green,
                                   parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 0, hjust = 0.5  ) )

ggsave( plot = iess_patrimonio_fondo, 
        filename = paste0( parametros$resultado_graficos, 'iess_patrimonio_fondo_rtr',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Ingresos del  Fondo de RT--------------------------------------------------------------------------
message( '\tGraficando ingrensos del Fondo de RT.' )
unidad<-1e6
aux <- as.data.table( ingresos_fondo )
aux <- aux[, Ingresos:=Ingresos/unidad]
x_lim <- c( 2012, 2021 )
x_brk <- 2012:2021
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 250)
y_brk <- seq( y_lim[1], y_lim[2], 50 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_ingresos_fondo <-  ggplot( data = aux ) + 
  geom_line( aes( x = Año, 
                  y = Ingresos, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Millones (USD)') +
  scale_color_manual( values =  c( parametros$iess_green, 
                                   parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 0, hjust = 0.5 ) )

ggsave( plot = iess_ingresos_fondo, 
        filename = paste0( parametros$resultado_graficos, 'iess_ingresos_fondo_rtr', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Ingresos por aporte del  Fondo de RT---------------------------------------------------------------
message( '\tGraficando ingrensos por aporte del Fondo de RT.' )
unidad<-1e6
aux <- as.data.table( ingresos_aportes )
aux <- aux[, `Aportes Afiliados ($)`:=`Aportes Afiliados ($)`/unidad]
x_lim <- c( 2012, 2021 )
x_brk <- 2012:2021
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 200)
y_brk <- seq( y_lim[1], y_lim[2], 50 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_ingresos_aportes_fondo <-  ggplot( data = aux ) + 
  geom_line( aes( x = Año, 
                  y = `Aportes Afiliados ($)`, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Millones (USD)') +
  scale_color_manual( values =  c( parametros$iess_green, 
                                   parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 0, hjust = 0.5 ) )

ggsave( plot = iess_ingresos_aportes_fondo, 
        filename = paste0( parametros$resultado_graficos, 'iess_ingresos_aportes_fondo_rtr', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Contribución del estado al  Fondo de RT-----------------------------------------------------------
message( '\tGraficando contribución del estado al Fondo de RT.' )
unidad<-1e6
aux <- as.data.table( otros_ingresos )
aux <- aux[, `Contribucion del Estado`:=`Contribucion del Estado`/unidad]
aux[ is.na(`Contribucion del Estado`),`Contribucion del Estado`:=0]
x_lim <- c( 2012, 2021 )
x_brk <- 2012:2021
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 24)
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6  )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_cont_estado_fondo <- ggplot( data = aux ) + 
  geom_line( aes( x = Anio, 
                  y = `Contribucion del Estado`, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Millones (USD)') +
  scale_color_manual( values =  c( parametros$iess_green, 
                                   parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 0, hjust = 0.5 ) )

ggsave( plot = iess_cont_estado_fondo, 
        filename = paste0( parametros$resultado_graficos, 'iess_cont_estado_fondo_rtr', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Evolución del gasto del   Fondo de RT--------------------------------------------------------------
message( '\tGraficando evolución del gasto del Fondo de RT.' )
unidad<-1e6
aux <- as.data.table( gastos )
aux <- aux[, `Gastos`:=`Gastos`/unidad]

x_lim <- c( 2012, 2021 )
x_brk <- 2012:2021
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 100)
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_gastos_fondo <-  ggplot( data = aux ) + 
  geom_line( aes( x = Año, 
                  y = Gastos, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Millones (USD)') +
  scale_color_manual( values =  c( parametros$iess_green,
                                   parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 0, hjust = 0.5) )

ggsave( plot = iess_gastos_fondo, 
        filename = paste0( parametros$resultado_graficos, 'iess_gastos_fondo_rtr',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# #Egresos pagados por prestaciones del Fondo de RT---------------------------------------------------
message( '\tGraficando egresos pagados por prestaciones del Fondo de RT.' )
unidad<-1e6
aux <- as.data.table( gastos_prestacionales )
aux <- aux[, `Prestaciones por Pensiones`:=`Prestaciones por Pensiones y subsidios`/unidad]
x_lim <- c( 2012, 2021 )
x_brk <- 2012:2021
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 80)
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_gastos_prestacionales_fondo <- ggplot( data = aux ) +
  geom_line( aes( x = AÑO,
                  y =`Prestaciones por Pensiones`,
                  color = parametros$iess_blue ),
             size = graf_line_size,
             lineend = "round" ) +
  labs( x = 'Año', y = 'Millones (USD)') +
  scale_color_manual( values =  c( parametros$iess_green,
                                   parametros$iess_blue ),
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl,
                      limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl,
                      limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 0, hjust = 0.5 ) )

ggsave( plot = iess_gastos_prestacionales_fondo,
        filename = paste0( parametros$resultado_graficos, 'iess_gastos_prestacionales_fondo_rtr',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Otros gastos del Fondo de RT----------------------------------------------------------------------
message( '\tGraficando otros gastos del Fondo de RT.' )
unidad <-1e6
aux <- as.data.table( otros_gastos_prestacionales )
aux <- aux[AÑO > 2011]
aux <- aux[, TOTAL := TOTAL/unidad]

x_lim <- c( 2012, 2021 )
x_brk <- 2012:2021
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 8)
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_otros_gastos_fondo <- ggplot( data = aux ) + 
  geom_line( aes( x = AÑO, 
                  y = TOTAL, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Millones (USD)') +
  scale_color_manual( values =  c( parametros$iess_green, 
                                   parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 0, hjust = 0.5 ) )

ggsave( plot = iess_otros_gastos_fondo, 
        filename = paste0( parametros$resultado_graficos, 'iess_otros_gastos_fondo_rtr', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )



#Otros gastos del Fondo de RT.----------------------------------------------------------------------
message( '\tGraficando otros gastos del Fondo de RT.' )
unidad<-1e6
aux <- as.data.table( gast_adm )
aux <- aux[ , gast_adm := `Gastos Administrativos`/unidad]
x_lim <- c( 2013, 2021 )
x_brk <- 2013:2021
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 1500000)
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_otros_gatos_fondo <- ggplot( data = aux ) + 
  geom_line( aes( x = AÑO, 
                  y =`Gastos Administrativos`, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'USD') +
  scale_color_manual( values =  c( parametros$iess_green, 
                                   parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 0, hjust = 0.5 ) )

ggsave( plot = iess_otros_gatos_fondo, 
        filename = paste0( parametros$resultado_graficos, 'iess_otros_gatos_fondo_rtr', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Limpiando Ram--------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()