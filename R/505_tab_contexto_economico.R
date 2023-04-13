message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tLectura del contexto económico' )

# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData, 'IESS_contexto_economico.RData' ) )

# Cargar función tildes a latex---------------------------------------------------------------------
source( 'R/500_tildes_a_latex.R', encoding = 'UTF-8', echo = FALSE )

#Tabla del contexto económico-----------------------------------------------------------------------
message( '\tTablas del contexto económico' )

#Tabla de la inflación------------------------------------------------------------------------------

aux <- inflacion %>%
  filter( mes == '12',
          anio <= '2021') %>%
  dplyr::select( periodo,
                 ipc,
                 inflacion_mensual,
                 inflacion_acumulada,
                 inflacion_promedio_acumulada ) %>%
  mutate( periodo = as.character( periodo ) )


aux_xtab <- xtable( aux, digits = c(0, 0, 2, 2, 2 ,2 ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_inflacion', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )


#Tabla de desempleo---------------------------------------------------------------------------------

aux <- desempleo %>%
  filter( mes == '12' ) %>%
  dplyr::select( periodo,
                 desempleo_hombre,
                 desempleo_mujer,
                 desempleo_nacional,
                 empleo_adecuado_pleno_n,
                 subempleo_nacional ) %>%
  mutate( periodo = as.character( periodo ) )

aux_xtab <- xtable( aux, digits = c(0, 0, 2, 2, 2 ,2, 2 ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_desempleo', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )


#Tabla de PIB---------------------------------------------------------------------------------------

aux <- pib_real %>%
  filter( anio >= '2000',
          anio <= '2021' ) %>%
  mutate( anio = as.character( anio ) )

aux_xtab <- xtable( aux, digits = c(0, 0, 0, 0 , 2 ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_pib_real', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )


#Tabla de SBU---------------------------------------------------------------------------------------

aux <- sbu %>%
  filter( anio <= '2021') %>%
  mutate( anio = as.character( anio ) )

aux_xtab <- xtable( aux, digits = c(0, 0, 2, 2 , 2 ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_sbu', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )


#Tabla de salarios promedio-------------------------------------------------------------------------

aux <- salarios %>%
  mutate( anio = as.character( anio ) ) %>%
  filter( mes == '12' ) %>%
  arrange( periodo ) %>%
  mutate( incremento = sal_prom - lag( sal_prom ) ) %>%
  mutate( tasa = 100 * (sal_prom - lag( sal_prom )) / lag( sal_prom ) ) %>%
  na.omit( . ) %>%
  dplyr::select( periodo,
                 sal_prom,
                 incremento,
                 tasa ) %>%
  mutate( periodo = as.character( periodo ))
  

aux_xtab <- xtable( aux, digits = c(0, 0, 2, 2 , 2 ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_salarios', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )


#Tabla de aumento de pensiones----------------------------------------------------------------------

aux <- incre_pensiones %>%
  dplyr::select( -x2022 )

aux_xtab <- xtable( aux, digits = c(0, 0, rep( 2, 7 ) ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_incre_pensiones', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )


#Tabla de pensiones mínimas-------------------------------------------------------------------------

aux <- pension_min %>%
  filter( anio <= '2021' ) %>%
  mutate( anio = as.character( anio ) )

aux_xtab <- xtable( aux, digits = c(0, 0, 0, rep( 2, 6 ) ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_pension_min', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )


#Tabla de pensiones máximas-------------------------------------------------------------------------

aux <- pension_max %>%
  filter( anio <= '2021' ) %>%
  mutate( anio = as.character( anio ) )

aux_xtab <- xtable( aux, digits = c(0, 0, 0, rep( 2, 7 ) ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_pension_max', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )


#Tabla de tasas de interés---------------------------------------------------------------------------

aux <- tasas_interes %>%
  filter( anio <= '2021', mes == '12' ) %>%
  mutate( periodo = as.character( periodo ) ) %>%
    dplyr::select( periodo,
                   tasa_activa,
                   tasa_pasiva,
                   spread)


aux_xtab <- xtable( aux, digits = c(0, 0, rep( 2, 3 ) ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_tasas_interes', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

#Tabla de ROA---------------------------------------------------------------------------------------

aux <- roa %>%
  filter( anio <= '2021' ) %>%
  mutate( anio = as.character( anio ) ) %>%
  mutate( across( where(is.numeric), ~ .x * 100 ) )

aux_xtab <- xtable( aux, digits = c(0, 0, rep( 2, 5 ) ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_roa', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

#Tabla de ROE---------------------------------------------------------------------------------------

aux <- roe %>%
  filter( anio <= '2021' ) %>%
  mutate( anio = as.character( anio ) ) %>%
  mutate( across( where(is.numeric), ~ .x * 100 ) )

aux_xtab <- xtable( aux, digits = c(0, 0, rep( 2, 5 ) ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_roe', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )


#Tabla rendimientos del BIESS-----------------------------------------------------------------------

aux <- rendimiento_biess %>%
  filter( fecha <= as.Date("31/12/2021", "%d/%m/%Y" ),
          mes == '12' ) %>%
  mutate( fecha = as.character( fecha ) ) %>%
  dplyr::select( -mes ) %>%
  mutate( rendimiento = 100 * rendimiento ) %>%
  na.omit( )

aux_xtab <- xtable( aux, digits = c(0, 0, rep( 2, 7 ) ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_rendimiento_biess', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity )

#Borrando data frames-------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls( )[ !( ls( ) %in% c( 'parametros' ) ) ] )
gc( )
