message( paste( rep('-', 100 ), collapse = '' ) )
col_nom <- c( 'provincia', 'ruc', 'sucursal', 'razon_social','tipo_de_empresa','resolucion','fecha_acuerdo',
              'cod-estado_acuerdo','estado_acuerdo','ruc_disp','valrecafi','valpagafi',
              'valor_pendiente','concepto','cedula','nombre','fecha_siniestro','glosa',
              'fecha_emis','estado_glosa','valor_glosa','fecha_cancel ','numero_exp','funcionario_emite',
              'fecha_anul_acu','func_anul_acu','motiv_anul_acu')
col_tip <- c( 'character', 'character', 'numeric', 'character','character','numeric','character','character','character','numeric ',
              'numeric','numeric','numeric','character','character','character','character','numeric',
              'character','character','numeric','character','numeric','character','character','character','character')
message( '\tReporte Responsabilidades Patronales' )
parametros$Dat <- "D:\\Pasantes\\Darlyn\\IESS_sentecia_1024_19_sgrt\\Data\\SENTENCIA CORTE CONST 1024\\IESS-DSGRT-2022-0409-M\\"

file <- paste0( parametros$Dat, 'Reporte Responsabilidades Patronales Riesgos Trabajo_18-03-2022f.xlsx' )

reporte_resp_patronal <- read_xlsx( path = file, skip = 0, col_names = T) %>% clean_names()  #para nombres de columnas
reporte_resp_patronal <- reporte_resp_patronal %>% 
                          mutate(cedula=as.character(cedula),
                                 ruc=as.character(ruc)) %>% 
                          mutate(cedula= if_else(nchar(cedula)==9, paste0("0",cedula), cedula)) %>% 
                          mutate(fecha_acuerdo=as.Date(fecha_acuerdo,"%Y-%m-%d")) %>% 
                          mutate(fecha_siniestro=as.Date(fecha_siniestro,"%Y-%m-%d" )) %>% 
                          mutate(fecha_cancelacion=as.Date(fecha_cancelacion, "%Y-%m-%d")) %>% 
                          mutate(fecha_emision=as.Date(fecha_emision,"%Y-%m-%d")) %>% 
                          mutate(fecha_anulacion_acuerdo=as.Date(fecha_anulacion_acuerdo,"%Y-%m-%d"))




# for 
# 
# 
# reporte_resp_patronal$provincia=as.factor(reporte_resp_patronal$provincia)
# #reporte_resp_patronal$ruc %>%  filter(reporte_resp_patronal$provincia==AZUAY)
# #reporte_resp_patronal$ruc %>% paste0("0", reporte_resp_patronal$ruc, sep = " ")
# azuya <- as.data.frame(filter(reporte_resp_patronal, provincia=="AZUAY" |  provincia=="BOLIVAR" | provincia=="CHIMBORAZO" 
#                               | provincia =="CAÑAR" | provincia =="CARCHI" | provincia=="COTOPAXI"| provincia=="EL ORO"|
#                                 provincia=="ESMERALDAS"| provincia=="GUAYAS")) 

# #Lectura desglose de pensiones de RT
# message( '\tLeyendo ingresos vs gastos administrativos' )
# file <- paste0( parametros$Data_seg, 'IESS_RTR_informacion_financiera.xlsx' )
# gastos_pensiones_rt <- read_xlsx( path = file, sheet = 'gastos_pensiones_rt', skip = 0,
#                                   col_names = T)
# 
# 
# lista <- c('activo_del_fondo', 'analisis_componentes_activo', 'analisis_horizontal_activo'
#            , 'analisis_vertical_activo', 'cuentas_cobrar_fondo', 'pasivos_fondo'
#            ,'componentes_pasivos_fondo', 'analisis_horizontal_pasivos', 'analisis_vertical_pasivos'
#            , 'cuentas_pagar_fondo', 'patrimonio_fondo', 'componentes_patrimonio_fondo'
#            , 'analisis_horizontal_patrimonio', 'analisis_vertical_patrimonio', 'ingresos_fondo'
#            , 'componentes_ingresos', 'analisis_horizontal_ingresos', 'analisis_vertical_ingresos'
#            , 'ingresos_aportes', 'otros_ingresos', 'gastos', 'componentes_gastos'
#            , 'analisis_horizontal_gastos', 'analisis_vertical_gastos', 'gastos_prestacionales'
#            , 'otros_gastos_prestacionales', 'gast_adm', 'relacion_patrimonio_beneficio'
#            , 'ingresos_vs_gastos', 'ingresos_vs_gtos_adm')
# save( list=lista,
#       file = paste0( parametros$RData_seg, 'IESS_RTR_analisis_financiero.RData' ) )
# #-----------------------------------------------------------------------------------------------
 # message( paste( rep('-', 100 ), collapse = '' ) )
 # rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
 # gc()


# # Guardando mortalidad -----------------------------------------------------------------------------
 # message("\tGuardando tablas")
 # save(mortalidad_jubilados_edad_rtr,
 #      file = paste0(parametros$RData_seg, "IESS_RTR_mortalidad_pensionistas.RData")
 # )
 
# # Borrando Dataframes--------------------------------------------------------------------------------
 # message(paste(rep("-", 100), collapse = ""))
 # rm(list = ls()[!(ls() %in% c("parametros"))])
 # gc()
