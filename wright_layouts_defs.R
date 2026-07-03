# =============================================================================
# wright_layouts_defs.R
# Definiciones de layout para los 19 esquemas de clase de Wright (Chile CONCLAT)
# -----------------------------------------------------------------------------
# Este archivo contiene SOLO definiciones (generadores de layout, etiquetas de
# fila, y un despachador wright_layout()/wright_rowlabels()). No calcula
# frecuencias ni genera archivos. Está pensado para source() desde el qmd,
# junto con wright_system.R (motor) y wright_get_counts.R (extracción de n).
#
# Uso típico en el qmd:
#   source("wright_system.R")
#   source("wright_layouts_defs.R")
#   source("wright_get_counts.R")
#   ...
#   counts <- get_counts(data, wrightclass_v9_cap_reduced)
#   draw_wright(wright_layout("v9"), counts,
#               title = "v9 · Capitalists reduced",
#               row_labels_left = wright_rowlabels("v9"))
#
# IMPORTANTE: las etiquetas de nivel usadas aquí (p. ej. "6. Skilled &
# Nonskilled Supervisors") deben coincidir EXACTAMENTE, prefijo numérico
# incluido, con los labels del factor wrightclass_vN en el qmd. Si no
# coinciden, draw_wright() dibuja "?" en la celda afectada. Estas definiciones
# se verificaron contra 02-esquemas-clases-cl.qmd; si cambian los labels en el
# qmd, hay que actualizarlas aquí.
# =============================================================================

suppressMessages(library(dplyr))

# -----------------------------------------------------------------------------
# Etiquetas de la columna izquierda (tamaño de empresa) según versión
# -----------------------------------------------------------------------------
LL_10 <- c("10+", "2-9",  "0-1")   # umbral empleadores >= 10
LL_50 <- c("50+", "2-49", "0-1")   # umbral capitalistas >= 50
LL_2  <- c("2+", "0-1")   # owners fusionado >= 2 (v7, v10, v19): 2 bloques
                          # (celda unida de empleadores + petty bourgeoisie)

# -----------------------------------------------------------------------------
# Generadores de layout por patrón estructural
# (requieren blk(), definido en wright_system.R)
# -----------------------------------------------------------------------------

# Patrón COMPLETO (12 categorías): v1, v2
layout_full <- function(top_owner_label, top_owner_name) {
  bind_rows(
    blk(top_owner_label,        top_owner_name,               row=3, col=0),
    blk("Small\nEmployers",     "2. Small employers",         row=2, col=0),
    blk("Petty\nBourgeoisie",   "3. Petty bourgeoisie",       row=1, col=0),
    blk("Expert\nManagers",     "4. Expert managers",         row=3, col=1),
    blk("Skilled\nManagers",    "7. Skilled managers",        row=3, col=2),
    blk("Nonskilled\nManagers", "10. Nonskilled managers",    row=3, col=3),
    blk("Expert\nSupervisors",  "5. Expert supervisors",      row=2, col=1),
    blk("Skilled\nSupervisors", "8. Skilled supervisors",     row=2, col=2),
    blk("Nonskilled\nSuperv.",  "11. Nonskilled supervisors", row=2, col=3),
    blk("Experts",              "6. Experts",                 row=1, col=1),
    blk("Skilled\nWorkers",     "9. Skilled workers",         row=1, col=2),
    blk("Nonskilled\nWorkers",  "12. Nonskilled workers",     row=1, col=3)
  )
}

# Patrón INFORMALIDAD (13 categorías): v3, v4
layout_informal <- function(top_owner_label, top_owner_name) {
  bind_rows(
    blk(top_owner_label,        top_owner_name,               row=3, col=0),
    blk("Small\nEmployers",     "2. Small employers",         row=2, col=0),
    blk("Petty\nbourg.",        "3. Petty bourgeoisie",       row=1, col=0,
        split="diag", n_name2="13. Informal self-employed", level2="Inf.\nself-emp."),
    blk("Expert\nManagers",     "4. Expert managers",         row=3, col=1),
    blk("Skilled\nManagers",    "7. Skilled managers",        row=3, col=2),
    blk("Nonskilled\nManagers", "9. Nonskilled managers",     row=3, col=3),
    blk("Expert\nSupervisors",  "5. Expert supervisors",      row=2, col=1),
    blk("Skilled\nSupervisors", "8. Skilled supervisors",     row=2, col=2),
    blk("Nonskilled\nSuperv.",  "10. Nonskilled supervisors", row=2, col=3),
    blk("Experts",              "6. Experts",                 row=1, col=1),
    blk("Formal\nworkers",      "11. Formal workers",         row=1, col=2, col_max=3,
        split="diag", n_name2="12. Informal workers", level2="Informal\nworkers")
  )
}

# Patrón MANAGERS+SUPERVISORES POR FILA, owner separado: v5, v6, v17, v18
layout_rowmerge <- function(top_owner_label, top_owner_name,
                            mgr_name="4. Managers", sup_name="5. Supervisors",
                            exp_name="6. Experts",
                            fw_name="7. Formal workers", iw_name="8. Informal workers",
                            pb_name="3. Petty bourgeoisie",
                            ise_name="9. Informal self-employed") {
  bind_rows(
    blk(top_owner_label,   top_owner_name,       row=3, col=0),
    blk("Small\nEmployers","2. Small employers", row=2, col=0),
    blk("Petty\nbourg.",   pb_name,              row=1, col=0,
        split="diag", n_name2=ise_name, level2="Inf.\nself-emp."),
    blk("Managers",        mgr_name,             row=3, col=1, col_max=3),
    blk("Supervisors",     sup_name,             row=2, col=1, col_max=3),
    blk("Experts",         exp_name,             row=1, col=1),
    blk("Formal\nworkers", fw_name,              row=1, col=2, col_max=3,
        split="diag", n_name2=iw_name, level2="Informal\nworkers")
  )
}

# Patrón MANAGERS+SUPERVISORES POR FILA, owner FUSIONADO vertical: v7, v19
layout_rowmerge_ownermerge <- function(owner_name="1. Employers",
                            mgr_name, sup_name, exp_name, fw_name, iw_name,
                            pb_name, ise_name) {
  bind_rows(
    blk("Employers",       owner_name,  row=2, row_max=3, col=0),
    blk("Petty\nbourg.",   pb_name,     row=1, col=0,
        split="diag", n_name2=ise_name, level2="Inf.\nself-emp."),
    blk("Managers",        mgr_name,    row=3, col=1, col_max=3),
    blk("Supervisors",     sup_name,    row=2, col=1, col_max=3),
    blk("Experts",         exp_name,    row=1, col=1),
    blk("Formal\nworkers", fw_name,     row=1, col=2, col_max=3,
        split="diag", n_name2=iw_name, level2="Informal\nworkers")
  )
}

# Patrón SUPERVISORES EN BLOQUE 2x2, owner separado:
# v8, v9, v11, v12, v13, v14, v15, v16
layout_supblock <- function(top_owner_label, top_owner_name,
                            sup_name="6. Skilled & Nonskilled Supervisors",
                            em_name="4. Expert managers", exp_name="5. Experts",
                            fw_name="7. Formal workers", iw_name="8. Informal workers",
                            pb_name="3. Petty bourgeoisie",
                            ise_name="9. Informal self-employed") {
  bind_rows(
    blk(top_owner_label,    top_owner_name,       row=3, col=0),
    blk("Small\nEmployers", "2. Small employers", row=2, col=0),
    blk("Petty\nbourg.",    pb_name,              row=1, col=0,
        split="diag", n_name2=ise_name, level2="Inf.\nself-emp."),
    blk("Expert\nManagers", em_name,              row=3, col=1),
    blk("Skilled & Nonskilled Managers\n+ All Supervisors", sup_name,
        row=2, row_max=3, col=1, col_max=3,
        split="lshape", notch_row=3, notch_col=1),
    blk("Experts",          exp_name,             row=1, col=1),
    blk("Formal\nworkers",  fw_name,              row=1, col=2, col_max=3,
        split="diag", n_name2=iw_name, level2="Informal\nworkers")
  )
}

# Patrón SUPERVISORES EN BLOQUE 2x2, owner FUSIONADO vertical: v10
layout_supblock_ownermerge <- function(owner_name="1. Employers",
                            sup_name, em_name, exp_name, fw_name, iw_name,
                            pb_name, ise_name) {
  bind_rows(
    blk("Employers",     owner_name,  row=2, row_max=3, col=0),
    blk("Petty\nbourg.", pb_name,     row=1, col=0,
        split="diag", n_name2=ise_name, level2="Inf.\nself-emp."),
    blk("Expert\nManagers", em_name,  row=3, col=1),
    blk("Skilled & Nonskilled Managers\n+ All Supervisors", sup_name,
        row=2, row_max=3, col=1, col_max=3,
        split="lshape", notch_row=3, notch_col=1),
    blk("Experts",       exp_name,    row=1, col=1),
    blk("Formal\nworkers", fw_name,   row=1, col=2, col_max=3,
        split="diag", n_name2=iw_name, level2="Informal\nworkers")
  )
}

# -----------------------------------------------------------------------------
# Despachador: devuelve el layout de una versión por su id ("v1".."v19")
# Encapsula la asignación versión -> patrón y los prefijos numéricos correctos
# de cada factor (verificados contra el qmp). Así el qmd solo llama
# wright_layout("v9") sin recordar el patrón.
# -----------------------------------------------------------------------------
wright_layout <- function(version) {
  switch(version,
    # --- completos ---
    "v1" = layout_full("Employers",   "1. Employers"),
    "v2" = layout_full("Capitalists", "1. Capitalists"),
    # --- informalidad ---
    "v3" = layout_informal("Employers",   "1. Employers"),
    "v4" = layout_informal("Capitalists", "1. Capitalists"),
    # --- rowmerge, owner separado ---
    "v5" = layout_rowmerge("Employers",   "1. Employers"),
    "v6" = layout_rowmerge("Capitalists", "1. Capitalists"),
    "v17" = layout_rowmerge("Employers",   "1. Employers"),
    "v18" = layout_rowmerge("Capitalists", "1. Capitalists"),
    # --- rowmerge, owner fusionado (prefijos corridos: managers=3, etc.) ---
    "v7" = layout_rowmerge_ownermerge(
             owner_name="1. Employers",
             mgr_name="3. Managers", sup_name="4. Supervisors",
             exp_name="5. Experts", fw_name="6. Formal workers",
             iw_name="7. Informal workers",
             pb_name="2. Petty bourgeoisie",
             ise_name="8. Informal self-employed"),
    "v19" = layout_rowmerge_ownermerge(
             owner_name="1. Employers",
             mgr_name="3. Managers", sup_name="4. Supervisors",
             exp_name="5. Experts", fw_name="6. Formal workers",
             iw_name="7. Informal workers",
             pb_name="2. Petty bourgeoisie",
             ise_name="8. Informal self-employed"),
    # --- supblock, owner separado ---
    "v8"  = layout_supblock("Employers",   "1. Employers"),
    "v9"  = layout_supblock("Capitalists", "1. Capitalists"),
    "v11" = layout_supblock("Employers",   "1. Employers"),
    "v12" = layout_supblock("Capitalists", "1. Capitalists"),
    "v13" = layout_supblock("Employers",   "1. Employers"),
    "v14" = layout_supblock("Capitalists", "1. Capitalists"),
    "v15" = layout_supblock("Employers",   "1. Employers"),
    "v16" = layout_supblock("Capitalists", "1. Capitalists"),
    # --- supblock, owner fusionado (prefijos corridos: expert mgr=3, etc.) ---
    "v10" = layout_supblock_ownermerge(
             owner_name="1. Employers",
             sup_name="5. Skilled & Nonskilled Supervisors",
             em_name="3. Expert managers", exp_name="4. Experts",
             fw_name="6. Formal workers", iw_name="7. Informal workers",
             pb_name="2. Petty bourgeoisie",
             ise_name="8. Informal self-employed"),
    stop("Versión no reconocida: ", version)
  )
}

# -----------------------------------------------------------------------------
# Despachador de etiquetas de fila izquierda por versión
# -----------------------------------------------------------------------------
# Nota: el número de etiquetas debe igualar el número de BLOQUES de owners del
# layout. En los patrones estándar hay 3 bloques de owners (3 etiquetas). En
# owner fusionado (v7, v10, v19) hay 2 bloques —la celda unida de empleadores
# (>=2) y petty bourgeoisie (0-1)— por lo que se devuelven 2 etiquetas: "2+"
# centrada en la celda unida, "0-1" en petty bourgeoisie.
wright_rowlabels <- function(version) {
  if (version %in% c("v7","v10","v19")) return(c("2+", "0-1"))
  if (version %in% c("v2","v4","v6","v9","v12","v14","v16","v18")) return(LL_50)
  LL_10  # v1,v3,v5,v8,v11,v13,v15,v17
}

# -----------------------------------------------------------------------------
# Mapa versión -> nombre de variable en el data.frame (para get_counts).
# Útil si se quiere iterar programáticamente sobre las 19.
# -----------------------------------------------------------------------------
wright_varname <- c(
  v1="wrightclass_v1_original",     v2="wrightclass_v2_capitalists",
  v3="wrightclass_v3_emp_informal", v4="wrightclass_v4_cap_informal",
  v5="wrightclass_v5_emp_reduced",  v6="wrightclass_v6_cap_reduced",
  v7="wrightclass_v7_reduced",      v8="wrightclass_v8_emp_reduced",
  v9="wrightclass_v9_cap_reduced",  v10="wrightclass_v10_reduced",
  v11="wrightclass_v11_emp_reduced",v12="wrightclass_v12_cap_reduced",
  v13="wrightclass_v13_emp_reduced",v14="wrightclass_v14_cap_reduced",
  v15="wrightclass_v15_emp_reduced",v16="wrightclass_v16_cap_reduced",
  v17="wrightclass_v17_emp_reduced",v18="wrightclass_v18_cap_reduced",
  v19="wrightclass_v19_reduced"
)
