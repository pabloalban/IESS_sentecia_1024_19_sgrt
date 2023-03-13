message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tLeyendo Activo Fondo' )
file <- paste0( parametros$Data_seg, 'IESS_RTR_informacion_financiera.xlsx' )

activo_del_fondo <- read_xlsx( path = file, sheet = 'Tabla1', skip = 0, 
                               col_names = T)
#Análisis Componentes Activo------------------------------------------------------------------------
message( '\tLeyendo Análisis Componentes Activo' )
file <- paste0( parametros$Data_seg, 'IESS_RTR_informacion_financiera.xlsx' )
analisis_componentes_activo <- read_xlsx( path = file, sheet = 'Tabla2', skip = 0,
                                          col_names = T)
#Leyendo Análisis Horizontal del Activo-------------------------------------------------------------
message( '\tLeyendo Análisis Horizontal del Activo' )
analisis_horizontal_activo <- read_xlsx( path = file, sheet = 'Tabla3', skip = 0,
                                         col_names = T)

#Leyendo Análisis Vertical del Activo---------------------------------------------------------------
message( '\tLeyendo Análisis Vertical del Activo' )
analisis_vertical_activo <- read_xlsx( path = file, sheet = 'Tabla4', skip = 0,
                                       col_names = T)

#Leyendo Cuentas por Cobrar del Fondo RT------------------------------------------------------------
message( '\tLeyendo Cuentas por CObrar del FOndo RT' )
cuentas_cobrar_fondo <- read_xlsx( path = file, sheet = 'Tabla5', skip = 0,
                                   col_names = T)

#Leyendo Pasivos del Fondo--------------------------------------------------------------------------
message( '\tLeyendo Pasivos del Fondo' )
pasivos_fondo <- read_xlsx( path = file, sheet = 'Tabla6', skip = 0,
                            col_names = T)

# Leyendo Componentes del Pasivo del Fondo----------------------------------------------------------
message( '\tLeyendo Componentes del Pasivo del Fondo' )
componentes_pasivos_fondo <- read_xlsx( path = file, sheet = 'Tabla7', skip = 0,
                                        col_names = T)

#Leyendo Análisis Horizontal del Pasivo-------------------------------------------------------------
message( '\tLeyendo Análisis Horizontal del Pasivo' )
analisis_horizontal_pasivos <- read_xlsx( path = file, sheet = 'Tabla8', skip = 0,
                                          col_names = T)

#Leyendo Análisis Vertical del Pasivo---------------------------------------------------------------
message( '\tLeyendo Análisis Vertical del Pasivo' )
analisis_vertical_pasivos <- read_xlsx( path = file, sheet = 'Tabla9', skip = 0,
                                        col_names = T)

#Leyendo Cuentas por Pagar del Fondo----------------------------------------------------------------
message( '\tLeyendo Cuentas por Pagar del Fondo' )
cuentas_pagar_fondo <- read_xlsx( path = file, sheet = 'Tabla10', skip = 0,
                                  col_names = T)

#Leyendo Patrimonio del Fondo-----------------------------------------------------------------------
message( '\tLeyendo Patrimonio del Fondo' )
patrimonio_fondo <- read_xlsx( path = file, sheet = 'Tabla11', skip = 0,
                               col_names = T)

#Leyendo Componentes del Patrimonio-----------------------------------------------------------------
message( '\tLeyendo Componentes del Patrimonio' )
componentes_patrimonio_fondo <- read_xlsx( path = file, sheet = 'Tabla12', skip = 0,
                                           col_names = T)

#Leyendo Análisis Horizontal del Patrimonio---------------------------------------------------------
message( '\tLeyendo Análisis Horizontal del Patrimonio' )
analisis_horizontal_patrimonio <- read_xlsx( path = file, sheet = 'Tabla13', skip = 0,
                                             col_names = T)

#Leyendo Análisis Vertical del Patrimonio-----------------------------------------------------------
message( '\tLeyendo Análisis Vertical del Patrimonio' )
analisis_vertical_patrimonio <- read_xlsx( path = file, sheet = 'Tabla14', skip = 0,
                                           col_names = T)

#Leyendo Ingresos del Fondo-------------------------------------------------------------------------
message( '\tLeyendo Ingresos del Fondo' )
ingresos_fondo <- read_xlsx( path = file, sheet = 'Tabla15', skip = 0,
                             col_names = T)

#Leyendo Componentes de los Ingresos----------------------------------------------------------------
message( '\tLeyendo Componentes de los Ingresos' )
componentes_ingresos <- read_xlsx( path = file, sheet = 'comp_ingre', skip = 0,
                                   col_names = T)

#Leyendo Análisis Horizontal de los Ingresos--------------------------------------------------------
message( '\tLeyendo Análisis Horizontal de los Ingresos' )
analisis_horizontal_ingresos <- read_xlsx( path = file, sheet = 'ana_hor_ing', skip = 0,
                                           col_names = T)

#Leyendo Análisis Vertical de los Ingresos----------------------------------------------------------
message( '\tLeyendo Análisis Vertical de los Ingresos' )
analisis_vertical_ingresos <- read_xlsx( path = file, sheet = 'ana_ver_ing', skip = 0,
                                         col_names = T)

#Leyendo Ingresos por Aportes-----------------------------------------------------------------------
message( '\tLeyendo Ingresos por Aportes' )
ingresos_aportes <- read_xlsx( path = file, sheet = 'Tabla19', skip = 0,
                               col_names = T)

#Leyendo Otros Ingresos-----------------------------------------------------------------------------
message( '\tLeyendo Otros Ingresos' )
otros_ingresos <- read_xlsx( path = file, sheet = 'Tabla20', skip = 0,
                             col_names = T)

#Leyendo Gastos-------------------------------------------------------------------------------------
message( '\tLeyendo Gastos' )
gastos <- read_xlsx( path = file, sheet = 'Tabla21', skip = 0,
                     col_names = T)

#Leyendo Componentes del Gastos---------------------------------------------------------------------
message( '\tLeyendo Componentes del Gastos' )
componentes_gastos <- read_xlsx( path = file, sheet = 'compo_gasto', skip = 0,
                                 col_names = T)

#Leyendo Análisis Horizontal del Gastos-------------------------------------------------------------
message( '\tLeyendo Análisis Horizontal del Gastos' )
analisis_horizontal_gastos <- read_xlsx( path = file, sheet = 'ana_hor_gastos', skip = 0,
                                         col_names = T)

#Leyendo Análisis Vertical del Gastos---------------------------------------------------------------
message( '\tLeyendo Análisis Vertical del Gastos' )
analisis_vertical_gastos <- read_xlsx( path = file, sheet = 'ana_ver_gastos', skip = 0,
                                       col_names = T)

# Leyendo Gastos Prestacionales---------------------------------------------------------------------
message( '\tLeyendo Gastos Prestacionales' )
gastos_prestacionales <- read_xlsx( path = file, sheet = 'gastos_prestacionales', skip = 0,
                                    col_names = T)

#Leyendo Otros Gastos Prestacionales----------------------------------------------------------------
message( '\tLeyendo Otros Gastos Prestacionales' )
otros_gastos_prestacionales <- read_xlsx( path = file, sheet = 'Tabla26', skip = 0,
                                          col_names = T)

# Leyendo Otros Gastos------------------------------------------------------------------------------
message( '\tLeyendo Otros Gastos' )
gast_adm <- read_xlsx( path = file, sheet = 'gast_adm', skip = 0,
                       col_names = T)

#Lectura de ingresos vs gastos totales--------------------------------------------------------------
message( '\tLeyendo ingresos vs gastos totales' )
ingresos_vs_gastos <- read_xlsx( path = file, sheet = 'ingresos_vs_gastos', skip = 0,
                                 col_names = T)

#Lectura de ingresos vs gastos adminstrativos-------------------------------------------------------
message( '\tLeyendo ingresos vs gastos administrativos' )
ingresos_vs_gtos_adm <- read_xlsx( path = file, sheet = 'ingresos_vs_gtos_adm', skip = 0,
                                   col_names = T)

#Lectura de ingresos vs gastos adminstrativos-------------------------------------------------------
message( '\tLeyendo ingresos vs gastos administrativos' )
relacion_patrimonio_beneficio <- read_xlsx( path = file, sheet = 'relacion_patrimonio_beneficio', skip = 0,
                                            col_names = T)

#Lectura desglose de pensiones de RT----------------------------------------------------------------
message( '\tLeyendo ingresos vs gastos administrativos' )
gastos_pensiones_rt <- read_xlsx( path = file, sheet = 'gastos_pensiones_rt', skip = 0,
                                  col_names = T)


#Guardar RData--------------------------------------------------------------------------------------
lista <- c('activo_del_fondo', 'analisis_componentes_activo', 'analisis_horizontal_activo'
           , 'analisis_vertical_activo', 'cuentas_cobrar_fondo', 'pasivos_fondo'
           ,'componentes_pasivos_fondo', 'analisis_horizontal_pasivos', 'analisis_vertical_pasivos'
           , 'cuentas_pagar_fondo', 'patrimonio_fondo', 'componentes_patrimonio_fondo'
           , 'analisis_horizontal_patrimonio', 'analisis_vertical_patrimonio', 'ingresos_fondo'
           , 'componentes_ingresos', 'analisis_horizontal_ingresos', 'analisis_vertical_ingresos'
           , 'ingresos_aportes', 'otros_ingresos', 'gastos', 'componentes_gastos'
           , 'analisis_horizontal_gastos', 'analisis_vertical_gastos', 'gastos_prestacionales'
           , 'otros_gastos_prestacionales', 'gast_adm', 'relacion_patrimonio_beneficio'
           , 'ingresos_vs_gastos', 'ingresos_vs_gtos_adm')

save( list=lista,
      file = paste0( parametros$RData_seg, 'IESS_RTR_analisis_financiero.RData' ) )

#Limpiar RAM----------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
