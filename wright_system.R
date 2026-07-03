# =============================================================================
# Esquemas de clase de Wright — sistema de graficado para CONCLAT
# =============================================================================
# Arquitectura en dos capas:
#
#  (1) LAYOUT por versión: una tabla que dice, para cada nivel del factor
#      wrightclass_vN, qué región de la matriz ocupa. La región se expresa
#      en la grilla canónica de Wright:
#         - filas (autoridad):  row = 3 (managers/10+/50+), 2 (supervisors),
#                               1 (workers/0-1)
#         - cols (skills):      col = 1 (experts), 2 (skilled), 3 (nonskilled)
#         - columna owners:     col = 0  (una sola columna a la izquierda)
#      Un bloque puede abarcar varias filas/cols (fusión) indicando
#      row_min/row_max/col_min/col_max. Una celda dividida formal/informal
#      se marca con split = "diag" y trae dos etiquetas + dos frecuencias.
#
#  (2) GRAFICADOR: toma el layout + un named vector de frecuencias n
#      (nombres = niveles del factor) y dibuja el esquema.
#
# Las frecuencias son CONTEO NO PONDERADO (n muestral), como se definió.
# El layout NO se deriva 100% automáticamente: las fusiones y diagonales
# son decisiones de representación (ver nota metodológica al usuario).
# =============================================================================

suppressMessages(library(ggplot2))

# ---- Paleta CONCLAT ----
col_axis   <- "#0f6481"
col_margin <- "#d02e38"
col_line   <- "#1a1a1a"
col_text   <- "#1a1a1a"

# Geometría canónica de la grilla ------------------------------------------
# Columna owners ocupa x [0,1]. Separador [1, 1.4]. Empleados: 3 columnas de
# ancho 1 en x [1.4, 4.4]. Filas: y [0,1], [1,2], [2,3].
.col_x <- function(col) {
  # col 0 = owners; col 1..3 = experts/skilled/nonskilled
  if (col == 0) return(c(0, 1))
  x0 <- 1.4 + (col - 1)
  c(x0, x0 + 1)
}
.row_y <- function(row) c(row - 1, row)  # row 1..3 -> y

# Construye el rectángulo (xmin,xmax,ymin,ymax) de un bloque que puede
# abarcar un rango de filas/columnas.
.block_rect <- function(row_min, row_max, col_min, col_max) {
  xs <- range(c(.col_x(col_min), .col_x(col_max)))
  ys <- range(c(.row_y(row_min), .row_y(row_max)))
  tibble::tibble(xmin = xs[1], xmax = xs[2], ymin = ys[1], ymax = ys[2])
}

# -----------------------------------------------------------------------------
# GRAFICADOR
# -----------------------------------------------------------------------------
# layout: data.frame con una fila por BLOQUE visual. Columnas:
#   level      : etiqueta de clase a mostrar (texto libre, con \n si se quiere)
#   n_name     : nombre del nivel del factor cuyo conteo va en el bloque
#                (debe existir en el vector `counts`)
#   row_min,row_max,col_min,col_max : región en la grilla
#   split      : NA (celda normal) o "diag" (dividida formal/informal)
#   n_name2, level2 : solo si split=="diag" (triángulo inferior)
# counts: named integer vector; nombres = niveles del factor, valores = n
# row_labels_left: etiquetas de la columna izquierda (según versión: 10+/50+, etc.)

draw_wright <- function(layout, counts,
                        title = NULL,
                        row_labels_left = c("10+", "2-9", "0-1"),
                        freq_size = 8, class_size = 3) {

  getn <- function(nm) {
    if (is.na(nm)) return("")
    if (!nm %in% names(counts)) return("?")
    as.character(counts[[nm]])
  }

  cells <- layout[is.na(layout$split), , drop = FALSE]
  diags <- layout[!is.na(layout$split) & layout$split == "diag", , drop = FALSE]

  # Rectángulos (bounding box de cada bloque)
  rects <- do.call(rbind, lapply(seq_len(nrow(layout)), function(i) {
    r <- layout[i, ]
    .block_rect(r$row_min, r$row_max, r$col_min, r$col_max)
  }))
  layout2 <- cbind(layout, rects)

  is_diag  <- !is.na(layout2$split) & layout2$split == "diag"
  is_lshape<- !is.na(layout2$split) & layout2$split == "lshape"
  cells2 <- layout2[!is_diag & !is_lshape, , drop = FALSE]
  diags2 <- layout2[is_diag, , drop = FALSE]
  lshapes<- layout2[is_lshape, , drop = FALSE]

  p <- ggplot()

  # --- celdas normales ---
  if (nrow(cells2) > 0) {
    p <- p +
      geom_rect(data = cells2,
                aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                fill = NA, color = col_line, linewidth = 0.6)
    for (i in seq_len(nrow(cells2))) {
      r <- cells2[i, ]
      xc <- (r$xmin + r$xmax) / 2
      yc <- (r$ymin + r$ymax) / 2
      p <- p +
        annotate("text", x = xc, y = yc + 0.12,
                 label = getn(r$n_name), hjust = 0.5, vjust = 0.5,
                 size = freq_size, fontface = "bold", color = col_text) +
        annotate("text", x = xc, y = yc - 0.22,
                 label = r$level, hjust = 0.5, vjust = 0.5,
                 size = class_size, color = col_text)
    }
  }

  # --- celdas diagonales ---
  if (nrow(diags2) > 0) {
    p <- p +
      geom_rect(data = diags2,
                aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                fill = NA, color = col_line, linewidth = 0.6) +
      geom_segment(data = diags2,
                   aes(x = xmin, y = ymin, xend = xmax, yend = ymax),
                   color = col_line, linewidth = 0.6)
    for (i in seq_len(nrow(diags2))) {
      r <- diags2[i, ]
      # Frecuencia superior-izquierda; su etiqueta justo debajo.
      # Frecuencia inferior-derecha; su etiqueta justo debajo.
      p <- p +
        annotate("text", x = r$xmin + 0.08, y = r$ymax - 0.18,
                 label = getn(r$n_name), hjust = 0, vjust = 1,
                 size = freq_size, fontface = "bold", color = col_text) +
        annotate("text", x = r$xmin + 0.08, y = r$ymax - 0.52,
                 label = r$level, hjust = 0, vjust = 1,
                 size = class_size, color = col_text) +
        annotate("text", x = r$xmax - 0.08, y = r$ymin + 0.34,
                 label = getn(r$n_name2), hjust = 1, vjust = 0,
                 size = freq_size, fontface = "bold", color = col_text) +
        annotate("text", x = r$xmax - 0.08, y = r$ymin + 0.06,
                 label = r$level2, hjust = 1, vjust = 0,
                 size = class_size, color = col_text)
    }
  }

  # --- celdas en forma de L (bloque envolvente menos una esquina) ---
  # La muesca (celda recortada) se indica con notch_row/notch_col en el layout.
  # Contorno trazado como polígono; texto centrado horizontalmente y ubicado
  # en la base de la L (donde está la mayor parte del área), no sobre la muesca.
  if (nrow(lshapes) > 0) {
    for (i in seq_len(nrow(lshapes))) {
      r <- lshapes[i, ]
      bx <- c(r$xmin, r$xmax); by <- c(r$ymin, r$ymax)   # envolvente
      nx <- .col_x(r$notch_col); ny <- .row_y(r$notch_row) # muesca
      # Polígono de 6 vértices para muesca en esquina superior-izquierda
      # (expert managers: nx1==xmin, ny2==ymax).
      poly <- data.frame(
        x = c(nx[2], bx[2], bx[2], bx[1], bx[1], nx[2]),
        y = c(by[2], by[2], by[1], by[1], ny[1], ny[1])
      )
      p <- p +
        geom_polygon(data = poly, aes(x = x, y = y),
                     fill = NA, color = col_line, linewidth = 0.6)
      xc <- (bx[1] + bx[2]) / 2
      yc <- by[1] + (ny[1] - by[1]) / 2 + 0.15  # centro de la base horizontal
      p <- p +
        annotate("text", x = xc, y = yc + 0.15,
                 label = getn(r$n_name), hjust = 0.5, vjust = 0.5,
                 size = freq_size, fontface = "bold", color = col_text) +
        annotate("text", x = xc, y = yc - 0.20,
                 label = r$level, hjust = 0.5, vjust = 0.5,
                 size = class_size, color = col_text)
    }
  }

  # --- límites del lienzo ---
  x_hi <- 4.4; x_lo <- 0; y_hi <- 3; y_lo <- 0

  # --- posiciones de las etiquetas de fila izquierda (tamaño de empresa) ---
  # Se derivan de los bloques REALES de la columna de owners (col 0), no de una
  # grilla fija de tres filas. Cuando owners está fusionado verticalmente
  # (v7, v10, v19), su etiqueta se centra en la celda unida y aparece una sola
  # vez. row_labels_left debe traer una etiqueta por bloque de owners, ordenadas
  # de arriba (mayor tamaño) hacia abajo.
  own <- layout2[layout2$xmin >= -1e-3 & layout2$xmax <= 1 + 1e-3, , drop = FALSE]
  own <- own[order(-((own$ymin + own$ymax) / 2)), , drop = FALSE]  # top-down
  own_y <- (own$ymin + own$ymax) / 2
  if (length(row_labels_left) != nrow(own)) {
    warning("row_labels_left tiene ", length(row_labels_left),
            " etiquetas pero hay ", nrow(own), " bloques de owners.")
  }
  lab_left <- rep(row_labels_left, length.out = nrow(own))

  p +
    # títulos de ejes
    annotate("text", x = (x_lo+x_hi)/2, y = y_hi+0.55,
             label = "RELATION TO THE MEANS OF PRODUCTION",
             color = col_axis, fontface = "bold", size = 4) +
    annotate("text", x = (x_lo+x_hi)/2, y = y_lo-0.55,
             label = "RELATION TO SCARCE SKILLS",
             color = col_axis, fontface = "bold", size = 4) +
    annotate("text", x = x_hi+0.85, y = (y_lo+y_hi)/2,
             label = "RELATION TO AUTHORITY",
             color = col_axis, fontface = "bold", size = 4, angle = 270) +
    annotate("text", x = x_lo-0.85, y = (y_lo+y_hi)/2,
             label = "NUMBER OF EMPLOYEES",
             color = col_axis, fontface = "bold", size = 4, angle = 90) +
    # márgenes
    annotate("text", x = 0.5, y = y_hi+0.22, label = "Owners",
             color = col_margin, size = 3.5) +
    annotate("text", x = 2.9, y = y_hi+0.22, label = "Employees",
             color = col_margin, size = 3.5) +
    annotate("text", x = c(1.9, 2.9, 3.9), y = y_lo-0.22,
             label = c("Experts","Skilled","Nonskilled"),
             color = col_margin, size = 3.2) +
    annotate("text", x = x_hi+0.30, y = c(2.5,1.5,0.5),
             label = c("Managers","Supervisors","Workers"),
             color = col_margin, size = 3.2, hjust = 0) +
    annotate("text", x = x_lo-0.30, y = own_y,
             label = lab_left, color = col_margin, size = 3.2, hjust = 1) +
    coord_equal(clip = "off") +
    labs(title = title) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 13),
          plot.margin = margin(35, 75, 35, 75))
}

# -----------------------------------------------------------------------------
# Helper para construir filas de layout de forma legible
# -----------------------------------------------------------------------------
blk <- function(level, n_name, row, col,
                row_max = row, col_max = col,
                split = NA, n_name2 = NA, level2 = NA,
                notch_row = NA, notch_col = NA) {
  tibble::tibble(
    level = level, n_name = n_name,
    row_min = row, row_max = row_max,
    col_min = col, col_max = col_max,
    split = split, n_name2 = n_name2, level2 = level2,
    notch_row = notch_row, notch_col = notch_col
  )
}

# =============================================================================
# Fin del motor. Los layouts de las 19 versiones y las frecuencias están en
# wright_19_layouts.R, que hace source() de este archivo.
# =============================================================================
