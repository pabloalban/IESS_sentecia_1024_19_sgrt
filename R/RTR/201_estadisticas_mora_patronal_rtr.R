message(paste(rep("-", 100), collapse = ""))
#Cargando Rdatas------------------------------------------------------------------------------------
message("\tCargando mora patronal del SGRT")
load(paste0(parametros$RData_seg, "IESS_RTR_rentas_2018.RData"))




#Guardar en Rdatas----------------------------------------------------------------------------------
message("\tGuardando Rdatas")
save( tab_evo_ben_pt,

      file = paste0( parametros$RData_seg, 'IESS_RTR_tablas_demografia.RData' ) )
#Limpiar Ram----------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
