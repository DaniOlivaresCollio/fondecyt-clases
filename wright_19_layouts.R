# =============================================================================
# 19 esquemas de clase de Wright — Chile CONCLAT
# Layouts + frecuencias de validación (tomadas de los dibujos a mano)
# =============================================================================
# source() relativo: asume que este archivo se ejecuta desde la raíz del
# proyecto (donde está fondecyt-clases.Rproj / _quarto.yml).
source("wright_system.R")
suppressMessages({library(dplyr); library(gridExtra)})

# Generadores de layout y etiquetas LL_*: se toman del archivo canónico de
# definiciones para evitar divergencia. Este script solo añade las frecuencias
# de validación y la exportación del PDF.
source("wright_layouts_defs.R")

# -----------------------------------------------------------------------------
# ESPECIFICACIÓN DE LAS 19 VERSIONES
# Cada entrada: layout, etiquetas de fila izq, título, y frecuencias de validación
# (n tomados de los dibujos a mano; en producción vienen del pipeline)
# -----------------------------------------------------------------------------

# Frecuencias base compartidas de skills/authority (v1-tipo)
n_full_emp <- c("1. Employers"=107,"2. Small employers"=276,"3. Petty bourgeoisie"=551,
  "4. Expert managers"=82,"7. Skilled managers"=107,"10. Nonskilled managers"=25,
  "5. Expert supervisors"=34,"8. Skilled supervisors"=83,"11. Nonskilled supervisors"=39,
  "6. Experts"=222,"9. Skilled workers"=688,"12. Nonskilled workers"=825)
n_full_cap <- n_full_emp; n_full_cap["1. Employers"]<-NA
n_full_cap <- c("1. Capitalists"=36,"2. Small employers"=347,"3. Petty bourgeoisie"=551,
  "4. Expert managers"=82,"7. Skilled managers"=107,"10. Nonskilled managers"=25,
  "5. Expert supervisors"=34,"8. Skilled supervisors"=83,"11. Nonskilled supervisors"=39,
  "6. Experts"=222,"9. Skilled workers"=688,"12. Nonskilled workers"=825)

# informalidad
n_inf_emp <- c("1. Employers"=107,"2. Small employers"=276,
  "3. Petty bourgeoisie"=196,"13. Informal self-employed"=355,
  "4. Expert managers"=82,"7. Skilled managers"=107,"9. Nonskilled managers"=25,
  "5. Expert supervisors"=34,"8. Skilled supervisors"=83,"10. Nonskilled supervisors"=39,
  "6. Experts"=222,"11. Formal workers"=1172,"12. Informal workers"=341)
n_inf_cap <- n_inf_emp
n_inf_cap["1. Employers"] <- NA
names(n_inf_cap)[1] <- "1. Capitalists"; n_inf_cap["1. Capitalists"] <- 36
n_inf_cap["2. Small employers"] <- 347

# rowmerge (managers/supervisores por fila)
n_row_emp <- c("1. Employers"=107,"2. Small employers"=276,
  "3. Petty bourgeoisie"=196,"9. Informal self-employed"=355,
  "4. Managers"=214,"5. Supervisors"=156,
  "6. Experts"=222,"7. Formal workers"=1172,"8. Informal workers"=341)
n_row_cap <- n_row_emp
names(n_row_cap)[1]<-"1. Capitalists"; n_row_cap["1. Capitalists"]<-36
n_row_cap["2. Small employers"]<-347

# supblock (288)
n_sup_emp <- c("1. Employers"=107,"2. Small employers"=276,
  "3. Petty bourgeoisie"=196,"9. Informal self-employed"=355,
  "4. Expert managers"=82,"6. Skilled & Nonskilled Supervisors"=288,
  "5. Experts"=222,"7. Formal workers"=1172,"8. Informal workers"=341)
n_sup_cap <- n_sup_emp
names(n_sup_cap)[1]<-"1. Capitalists"; n_sup_cap["1. Capitalists"]<-36
n_sup_cap["2. Small employers"]<-347

# --- construir lista de 19 ---
specs <- list()
add <- function(id, plot) specs[[id]] <<- plot

# v1, v2
add("v1", draw_wright(layout_full("Employers","1. Employers"), n_full_emp,
        "v1 · Original Emp", LL_10))
add("v2", draw_wright(layout_full("Capitalists","1. Capitalists"), n_full_cap,
        "v2 · Original Cap", LL_50))
# v3, v4
add("v3", draw_wright(layout_informal("Employers","1. Employers"), n_inf_emp,
        "v3 · Employers + informales", LL_10))
add("v4", draw_wright(layout_informal("Capitalists","1. Capitalists"), n_inf_cap,
        "v4 · Capitalists + informales", LL_50))
# v5, v6 (rowmerge)
add("v5", draw_wright(layout_rowmerge("Employers","1. Employers"), n_row_emp,
        "v5 · Reducido Emp Argentina", LL_10))
add("v6", draw_wright(layout_rowmerge("Capitalists","1. Capitalists"), n_row_cap,
        "v6 · Reducido Cap Argentina", LL_50))
# v7 (rowmerge + owner fusionado 383)
n_v7 <- c("1. Employers"=383,"2. Petty bourgeoisie"=196,"8. Informal self-employed"=355,
  "3. Managers"=214,"4. Supervisors"=156,"5. Experts"=222,
  "6. Formal workers"=1172,"7. Informal workers"=341)
add("v7", draw_wright(
  layout_rowmerge_ownermerge("1. Employers","3. Managers","4. Supervisors",
    "5. Experts","6. Formal workers","7. Informal workers",
    "2. Petty bourgeoisie","8. Informal self-employed"),
  n_v7, "v7 · Reducido Argentina", LL_2))
# v8, v9 (supblock)
add("v8", draw_wright(layout_supblock("Employers","1. Employers"), n_sup_emp,
        "v8 · Employers reduced", LL_10))
add("v9", draw_wright(layout_supblock("Capitalists","1. Capitalists"), n_sup_cap,
        "v9 · Capitalists reduced", LL_50))
# v10 (supblock + owner fusionado 383)
n_v10 <- c("1. Employers"=383,"2. Petty bourgeoisie"=196,"8. Informal self-employed"=355,
  "3. Expert managers"=82,"5. Skilled & Nonskilled Supervisors"=288,
  "4. Experts"=222,"6. Formal workers"=1172,"7. Informal workers"=341)
add("v10", draw_wright(
  layout_supblock_ownermerge("1. Employers","5. Skilled & Nonskilled Supervisors",
    "3. Expert managers","4. Experts","6. Formal workers","7. Informal workers",
    "2. Petty bourgeoisie","8. Informal self-employed"),
  n_v10, "v10 · Reduced", LL_2))
# v11..v16 (supblock, owner separado, distintos n)
mk_sup <- function(owner_lab, owner_name, o1,sm,pb,ise, title, LL) {
  n <- n_sup_emp
  if (grepl("Capital", owner_name)) { names(n)[1]<-"1. Capitalists" }
  n[1]<-o1; n[2]<-sm; n[3]<-pb; n[4]<-ise
  draw_wright(layout_supblock(owner_lab, names(n)[1]), n, title, LL)
}
add("v11", mk_sup("Employers","1. Employers",107,276,187,364,"v11 · Employers reduced",LL_10))
add("v12", mk_sup("Capitalists","1. Capitalists",36,347,187,364,"v12 · Capitalists reduced",LL_50))
add("v13", (function(){n<-n_sup_emp;n[1]<-18;n[2]<-365;n[3]<-187;n[4]<-364;n["7. Formal workers"]<-1171;
  draw_wright(layout_supblock("Employers","1. Employers"),n,"v13 · Employers reduced",LL_10)})())
add("v14", (function(){n<-n_sup_cap;n[1]<-7;n[2]<-376;n[3]<-187;n[4]<-364;
  draw_wright(layout_supblock("Capitalists","1. Capitalists"),n,"v14 · Capitalists reduced",LL_50)})())
add("v15", (function(){n<-n_sup_emp;n[1]<-40;n[2]<-343;n[3]<-187;n[4]<-364;
  draw_wright(layout_supblock("Employers","1. Employers"),n,"v15 · Employers reduced",LL_10)})())
add("v16", (function(){n<-n_sup_cap;n[1]<-15;n[2]<-368;n[3]<-187;n[4]<-364;
  draw_wright(layout_supblock("Capitalists","1. Capitalists"),n,"v16 · Capitalists reduced",LL_50)})())
# v17, v18 (rowmerge, distintos n)
add("v17", (function(){n<-n_row_emp;n[1]<-40;n[2]<-343;n[3]<-187;n[4]<-364;
  draw_wright(layout_rowmerge("Employers","1. Employers"),n,"v17 · Reducido Emp Argentina",LL_10)})())
add("v18", (function(){n<-n_row_cap;n[1]<-15;n[2]<-368;n[3]<-187;n[4]<-364;
  draw_wright(layout_rowmerge("Capitalists","1. Capitalists"),n,"v18 · Reducido Cap Argentina",LL_50)})())
# v19 (rowmerge + owner fusionado 383)
n_v19 <- c("1. Employers"=383,"2. Petty bourgeoisie"=187,"8. Informal self-employed"=364,
  "3. Managers"=214,"4. Supervisors"=156,"5. Experts"=222,
  "6. Formal workers"=1172,"7. Informal workers"=341)
add("v19", draw_wright(
  layout_rowmerge_ownermerge("1. Employers","3. Managers","4. Supervisors",
    "5. Experts","6. Formal workers","7. Informal workers",
    "2. Petty bourgeoisie","8. Informal self-employed"),
  n_v19, "v19 · Reducido Argentina", LL_2))

cat("Total esquemas:", length(specs), "\n")

# --- exportar PDF de validación (una lámina por página) ---
# Se guarda en output/ (carpeta que ya existe en tu proyecto).
dir.create("output", showWarnings = FALSE)
pdf("output/esquemas_chile_validacion.pdf", width=10, height=7)
for (id in names(specs)) print(specs[[id]])
invisible(dev.off())
cat("PDF generado en output/esquemas_chile_validacion.pdf\n")
