message( paste( rep('-', 100 ), collapse = '' ) )
<<<<<<< HEAD
=======
col_nom <- c( 'provincia', 'ruc', 'sucursal', 'razon_social','tipo_de_empresa','resolucion','fecha_acuerdo',
              'cod-estado_acuerdo','estado_acuerdo','ruc_disp','valrecafi','valpagafi',
              'valor_pendiente','concepto','cedula','nombre','fecha_siniestro','glosa',
              'fecha_emis','estado_glosa','valor_glosa','fecha_cancel ','numero_exp','funcionario_emite',
              'fecha_anul_acu','func_anul_acu','motiv_anul_acu')
col_tip <- c( 'character', 'character', 'numeric', 'character','character','numeric','character','character','character','numeric ',
              'numeric','numeric','numeric','character','character','character','character','numeric',
              'character','character','numeric','character','numeric','character','character','character','character')
message( '\tReporte Responsabilidades Patronales' )


file <- paste0( parametros$Data_seg, 'SENTENCIA CORTE CONST 1024/IESS-DSGRT-2022-0409-M/Reporte Responsabilidades Patronales Riesgos Trabajo_18-03-2022f.xlsx' )

reporte_resp_patronal <- read_xlsx( path = file, skip = 0, col_names = T) %>% clean_names()  #para nombres de columnas
reporte_resp_patronal <- reporte_resp_patronal %>% 
                          mutate(cedula=as.character(cedula),
                                 ruc=as.character(ruc)) %>% 
                          mutate(cedula= if_else(nchar(cedula)==9, paste0("0",cedula), cedula)) %>% 
                          mutate(fecha_acuerdo=as.Date(fecha_acuerdo,"%Y-%m-%d")) %>% 
                          mutate(fecha_siniestro=as.Date(fecha_siniestro,"%Y-%m-%d" )) %>% 
                          mutate(fecha_cancelacion=as.Date(fecha_cancelacion, "%Y-%m-%d")) %>% 
                          mutate(fecha_emision=as.Date(fecha_emision,"%Y-%m-%d")) %>% 
                          mutate(fecha_anulacion_acuerdo=as.Date(fecha_anulacion_acuerdo,"%Y-%m-%d")) %>% 
                          mutate(tipo_de_empresa=as.factor(tipo_de_empresa)) %>% 
                          mutate(provincia=as.factor(provincia))


file <- paste0( parametros$Data, 'CES/IESS_registro_civil.RData' )

load(file)
rc <- rc %>% select(cedula,sexo,fecha_nacimiento)
reporte_resp_patronal <-  left_join(reporte_resp_patronal,rc,by="cedula") %>%
                          filter(cod_estado_acuerdo!="ANU") %>%  # se quita los de codigo anulado
                          mutate(edad=as.integer(round(((fecha_siniestro-fecha_nacimiento)/365),0))) # se calcula la edad a la
                                                                                                     #fecha de siniestro

       
 save( reporte_resp_patronal,
       file = paste0( parametros$RData_seg, 'IESS_RTR_responsabilidad_patronal.RData' ) )
# #-----------------------------------------------------------------------------------------------
  message( paste( rep('-', 100 ), collapse = '' ) )
  rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
  gc()
>>>>>>> 8824a1abc505d081fd82eb1b018fc98a3efa3cd5

message( '\tLectura de la mora patronal' )

#Cargando información financiera--------------------------------------------------------------------
file_1<-paste0(parametros$Data_seg, 'SENTENCIA CORTE CONST 1024/IESS-DNRGC-2022-0585-M/','base datos mora 05 marzo 2022-1.xlsx' )

options(java.parameters = "-Xmx1000m")
data <- read.xlsx(file_1)
#Caraga de recursos administrados por el BIESS------------------------------------------------------
parte_1 <- read_excel(file_1,
                               sheet = 1,
                               col_names = TRUE,
                               col_types = NULL,
                               n_max = 1000,
                               na = "",
                               skip = 0) %>% clean_names()

inver_corte <- read_excel(file,
                          sheet = 2,
                          col_names = TRUE,
                          col_types = NULL,
                          na = "",
                          skip = 0) %>% clean_names()

rendimientos_netos <- read_excel(file,
                                 sheet = 3,
                                 col_names = TRUE,
                                 col_types = NULL,
                                 na = "",
                                 skip = 0) %>% clean_names() %>%
  mutate(corte_a=as.Date(corte_a,"%d/%m/%Y"))

ingresos <- read_excel(file,
                       sheet = 4,
                       col_names = TRUE,
                       col_types = NULL,
                       na = "",
                       skip = 0) %>% clean_names()

gastos_opera <- read_excel(file,
                           sheet = 5,
                           col_names = TRUE,
                           col_types = NULL,
                           na = "",
                           skip = 0) %>% clean_names()

inv_instrumento <- read_excel(file,
                              sheet = 6,
                              col_names = TRUE,
                              col_types = NULL,
                              na = "",
                              skip = 0) %>% clean_names()

detalle_bonos <- read_excel(file,
                            sheet = 7,
                            col_names = TRUE,
                            col_types = NULL,
                            na = "",
                            skip = 0) %>% clean_names()

detalle_bonos_40 <- read_excel(file,
                               sheet = 8,
                               col_names = TRUE,
                               col_types = NULL,
                               na = "",
                               skip = 0) %>% clean_names() %>%
  mutate(fecha_colocacion=as.Date(fecha_colocacion,"%d/%m/%Y"),
         vencimiento=as.Date(vencimiento,"%d/%m/%Y"),
         pago_del_periodo=as.Date(pago_del_periodo,"%d/%m/%Y"))

detalle_obligaciones <- read_excel(file,
                                   sheet = 9,
                                   col_names = TRUE,
                                   col_types = NULL,
                                   na = "",
                                   skip = 0) %>% clean_names()

detalle_titularizaciones <- read_excel(file,
                                       sheet = 10,
                                       col_names = TRUE,
                                       col_types = NULL,
                                       na = "",
                                       skip = 0) %>% clean_names()

#Guardando en un Rdata------------------------------------------------------------------------------
message( '\tGuardando inversiones en un solo data.frame' )

save( recurs_adm_biess,
      inver_corte,
      rendimientos_netos,
      ingresos,
      gastos_opera,
      inv_instrumento,
      detalle_bonos,
      detalle_bonos_40,
      detalle_obligaciones,
      detalle_titularizaciones,
      file = paste0( parametros$RData_seg, 'IESS_RTR_inversiones.RData' ) )

#Borrando data.frames-------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ]  )
gc()