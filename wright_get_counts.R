# =============================================================================
# Integración en el qmd: extracción de frecuencias no ponderadas
# y armado del vector `counts` que consume draw_wright()
# =============================================================================

# get_counts(): devuelve un named integer vector con el n NO PONDERADO de cada
# nivel del factor wrightclass_vN. Los nombres coinciden con las etiquetas del
# factor (idénticas a las usadas en los layouts).
#
# - No usa el diseño survey: el conteo muestral es independiente de los pesos.
# - Registra en un atributo cuántos casos quedaron sin clasificar (NA),
#   como control de consistencia de los case_when (que no llevan .default).
#
# Uso:
#   counts_v9 <- get_counts(data, wrightclass_v9_cap_reduced)
#   draw_wright(layout_v9, counts_v9, "v9", LL_50)

get_counts <- function(data, var) {
  var <- rlang::ensym(var)
  tab <- data %>%
    dplyr::mutate(.lab = sjlabelled::as_label({{ var }})) %>%   # usa etiquetas del factor
    dplyr::count(.lab)

  # separar NA (casos sin clasificar)
  n_na <- tab %>% dplyr::filter(is.na(.lab)) %>% dplyr::pull(n)
  n_na <- if (length(n_na) == 0) 0L else n_na

  tab <- tab %>% dplyr::filter(!is.na(.lab))
  out <- stats::setNames(tab$n, as.character(tab$.lab))

  attr(out, "n_unclassified") <- n_na
  attr(out, "n_total")        <- sum(tab$n) + n_na
  out
}

# NOTA sobre los nombres: draw_wright() busca cada n por el nombre exacto del
# nivel (p.ej. "6. Skilled & Nonskilled Supervisors"). Como tus factores ya
# llevan esas etiquetas con el prefijo numérico, y los layouts usan esos mismos
# strings en n_name, el emparejamiento es directo. Si una etiqueta del factor
# no coincide con la del layout, draw_wright() dibuja "?" en esa celda: es una
# señal visual de desajuste, no un error silencioso.

# -----------------------------------------------------------------------------
# Ejemplo de chunk para el qmd (uno por versión):
# -----------------------------------------------------------------------------
# ```{r}
# #| label: fig-wright-v9
# #| fig-width: 10
# #| fig-height: 7
# #| echo: false
#
# counts_v9 <- get_counts(data, wrightclass_v9_cap_reduced)
#
# # control de consistencia: cuántos casos no fueron clasificados
# if (attr(counts_v9, "n_unclassified") > 0)
#   message("v9: ", attr(counts_v9, "n_unclassified"), " casos sin clasificar (NA)")
#
# draw_wright(layout_v9, counts_v9,
#             title = "v9 · Capitalists reduced",
#             row_labels_left = LL_50)
# ```
