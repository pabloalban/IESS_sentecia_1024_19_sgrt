# Preparación --------------------------------------------------------------------------------------
source( 'R/400_preparar_reporte.R', encoding = 'UTF-8', echo = FALSE )

# # Gráficos genéricos
source( 'R/402_graf_tasas_macro.R', encoding = 'UTF-8', echo = FALSE )
# source( 'R/410_graf_poblacion_piramide.R', encoding = 'UTF-8', echo = FALSE )

# Gráficos específicas de RTR ----------------------------------------------------------------------
source( 'R/rtr/400_graf_analisis_demografico_rtr.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/rtr/402_graf_analisis_financiero_rtr.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/rtr/404_graf_inversiones_rtr.R', encoding = 'UTF-8', echo = FALSE )

# Tablas genéricas ---------------------------------------------------------------------------------
# source( 'R/200_hipotesis_macro.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/501_tab_tasas_macro.R', encoding = 'UTF-8', echo = FALSE )


# Tablas específicas de RTR ------------------------------------------------------------------------
source( 'R/rtr/500_tab_analisis_financiero_rtr.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/rtr/501_tab_analisis_demografico_rtr.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/rtr/503_tab_causas_desfinanciamientos_rtr.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/rtr/504_tab_inversiones_rtr.R', encoding = 'UTF-8', echo = FALSE )

# Reporte LaTeX ------------------------------------------------------------------------------------
source( parametros$reporte_script, encoding = 'UTF-8', echo = FALSE )

# Reportes excel -----------------------------------------------------------------------------------