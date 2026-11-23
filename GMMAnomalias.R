# =====================================================================
# DETECCIÓN DE ANOMALÍAS CON GMM Y LOF
# Validación en fraude con tarjetas y aplicación al sistema financiero
# boliviano (caso Banco Fassil)
# Aduviri Monasterios Brandon Ulises · Carrera de Informática · UMSA
# ---------------------------------------------------------------------
# Cómo usarlo: correr cada BLOQUE por separado, en orden, seleccionándolo
# y presionando Ctrl + Enter. Datos en la carpeta data/:
#   creditcard.csv y dataBaseByEntidad.xlsx
# =====================================================================


# =====================================================
# BLOQUE 0 · CONFIGURACIÓN DEL PROYECTO
# =====================================================

# 1. Paquetes
paquetes <- c("data.table", "dplyr", "tidyr", "readxl", "lubridate", "janitor",
              "ggplot2", "plotly", "DT", "crosstalk", "htmlwidgets", "scales",
              "mclust", "dbscan", "isotree", "pROC", "PRROC",
              "flexdashboard", "rmarkdown")
faltan <- paquetes[!paquetes %in% installed.packages()[, "Package"]]
if (length(faltan) > 0) install.packages(faltan)
invisible(lapply(paquetes, library, character.only = TRUE))

# 2. Reproducibilidad
set.seed(2026)
options(scipen = 999)

# 3. Carpetas del proyecto
carpetas <- c("data", "output/graficos", "output/tablas", "bitacora", "dashboard")
for (carp in carpetas) dir.create(carp, recursive = TRUE, showWarnings = FALSE)

# 4. Paleta base de los gráficos (el dashboard la convierte a negro/rojo)
aguayo <- c(magenta  = "#C2185B", rojo     = "#D32F2F", naranja  = "#F57C00",
            amarillo = "#FBC02D", verde    = "#2E7D32", turquesa = "#00838F",
            azul     = "#283593", morado   = "#6A1B9A",
            crema    = "#FBF6EC", tinta    = "#1E1B18")

# 5. Bitácora (se conserva entre sesiones y no duplica registros)
if (file.exists("bitacora/bitacora.csv")) {
  bitacora <- read.csv("bitacora/bitacora.csv", stringsAsFactors = FALSE,
                       fileEncoding = "UTF-8")
} else {
  bitacora <- data.frame(n = integer(), fecha_hora = character(), bloque = character(),
                         accion = character(), decision = character(),
                         justificacion = character(), filas_antes = numeric(),
                         filas_despues = numeric(), stringsAsFactors = FALSE)
}

registrar <- function(bloque, accion, decision, justificacion,
                      filas_antes = NA, filas_despues = NA) {
  nuevo <- data.frame(
    n = NA, fecha_hora = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    bloque = bloque, accion = accion, decision = decision,
    justificacion = justificacion,
    filas_antes = filas_antes, filas_despues = filas_despues)
  existe <- which(bitacora$bloque == bloque & bitacora$accion == accion)
  if (length(existe) > 0) {
    nuevo$n <- bitacora$n[existe[1]]
    bitacora[existe[1], ] <<- nuevo          # si ya existía, se actualiza
  } else {
    nuevo$n <- nrow(bitacora) + 1
    bitacora <<- rbind(bitacora, nuevo)      # si es nuevo, se agrega
  }
  write.csv(bitacora, "bitacora/bitacora.csv", row.names = FALSE, fileEncoding = "UTF-8")
  message("✔ Bitácora #", nuevo$n, ": ", accion)
}

registrar("B0", "Configuración del proyecto",
          "Paquetes, semilla 2026, carpetas y paleta de colores",
          "Garantizar la reproducibilidad del análisis")

# 6. Verificar datos (deben salir dos TRUE)
file.exists(c("data/creditcard.csv", "data/dataBaseByEntidad.xlsx"))


# =====================================================
# BLOQUE 1 · CARGA Y PERFIL DE LOS DATOS
# =====================================================

# ---------- 1.1 Dataset de validación: creditcard ----------
cc <- data.table::fread("data/creditcard.csv")
cc <- as.data.frame(cc)

cat("\n=== CREDITCARD ===\n")
cat("Filas:", nrow(cc), "| Columnas:", ncol(cc), "\n")
str(cc[, c("Time", "V1", "V2", "Amount", "Class")])
summary(cc[, c("Time", "Amount", "Class")])

registrar("B1", "Carga de creditcard.csv",
          paste("Se cargaron", nrow(cc), "filas y", ncol(cc), "columnas"),
          "Dataset de validación con etiqueta Class (0 = normal, 1 = fraude)",
          filas_antes = nrow(cc), filas_despues = nrow(cc))

# ---------- 1.2 Dataset boliviano: ASFI ----------
asfi <- readxl::read_excel("data/dataBaseByEntidad.xlsx")
asfi <- as.data.frame(asfi)
asfi$FECHA <- as.Date(asfi$FECHA)

cat("\n=== ASFI BOLIVIA ===\n")
cat("Filas:", nrow(asfi), "| Columnas:", ncol(asfi), "\n")
cat("Entidades:", length(unique(asfi$TIPO_DE_ENTIDAD)), "\n")
cat("Periodo:", format(min(asfi$FECHA)), "a", format(max(asfi$FECHA)), "\n")
cat("Meses:", length(unique(asfi$FECHA)), "\n")

registrar("B1", "Carga de dataBaseByEntidad.xlsx (ASFI)",
          paste("Se cargaron", nrow(asfi), "filas de",
                length(unique(asfi$TIPO_DE_ENTIDAD)), "entidades (2020-2023)"),
          "Datos reales del sistema financiero boliviano para el caso Fassil",
          filas_antes = nrow(asfi), filas_despues = nrow(asfi))

# ---------- 1.3 Diccionario de cuentas ASFI ----------
diccionario <- data.frame(
  codigo = c("CD_210_00","CD_220_00","CD_230_00","CD_240_00","CD_260_00",
             "CD_270_00","CD_280_00","CD_411_00","CD_412_00","CD_413_00",
             "CD_414_00","CD_415_00","CD_416_00","CD_417_00"),
  descripcion = c("Obligaciones con el público (depósitos)",
                  "Obligaciones con instituciones fiscales",
                  "Obligaciones con bancos y entidades de financiamiento",
                  "Otras cuentas por pagar",
                  "Valores en circulación",
                  "Obligaciones subordinadas",
                  "Obligaciones con empresas públicas",
                  "Cargos por obligaciones con el público",
                  "Cargos por obligaciones con instituciones fiscales",
                  "Cargos por obligaciones con bancos y entidades de financ.",
                  "Cargos por otras cuentas por pagar",
                  "Cargos por valores en circulación",
                  "Cargos por obligaciones subordinadas",
                  "Cargos por obligaciones con empresas públicas"),
  tipo = c(rep("Pasivo (saldo, Bs)", 7), rep("Gasto financiero (acumulado anual, Bs)", 7))
)
print(diccionario)

# ---------- 1.4 Tabla de perfil de ambos datasets ----------
perfil <- function(df, nombre) {
  num <- df[sapply(df, is.numeric)]
  data.frame(
    dataset   = nombre,
    variable  = names(num),
    faltantes = sapply(num, function(x) sum(is.na(x))),
    ceros_pct = round(sapply(num, function(x) mean(x == 0, na.rm = TRUE)) * 100, 1),
    minimo    = sapply(num, min, na.rm = TRUE),
    mediana   = sapply(num, median, na.rm = TRUE),
    maximo    = sapply(num, max, na.rm = TRUE),
    row.names = NULL
  )
}

perfil_total <- rbind(perfil(cc, "creditcard"), perfil(asfi, "ASFI Bolivia"))
write.csv(perfil_total, "output/tablas/perfil_datasets.csv", row.names = FALSE)

DT::datatable(perfil_total, rownames = FALSE, filter = "top",
              caption = "Perfil de variables de ambos datasets",
              options = list(pageLength = 15, scrollX = TRUE)) |>
  DT::formatRound(c("minimo", "mediana", "maximo"), 2)

registrar("B1", "Perfil de variables",
          "Tabla con faltantes, % de ceros, mínimo, mediana y máximo",
          "Primer diagnóstico de calidad antes de limpiar")


# =====================================================
# BLOQUE 2 · CALIDAD DE DATOS Y LIMPIEZA INICIAL
# =====================================================

# ---------- 2.0 Funciones de estilo para gráficos ----------
col <- function(x) unname(aguayo[x])

tema_aguayo <- function(p, titulo, subtitulo = NULL) {
  txt <- paste0("<b>", titulo, "</b>",
                if (!is.null(subtitulo)) paste0("<br><sup>", subtitulo, "</sup>") else "")
  p |>
    layout(title = list(text = txt, x = 0.02,
                        font = list(family = "Georgia", size = 18, color = col("tinta"))),
           paper_bgcolor = col("crema"), plot_bgcolor = col("crema"),
           font = list(family = "Courier New", color = col("tinta")),
           margin = list(t = 90)) |>
    config(displaylogo = FALSE, locale = "es")
}

guardar <- function(p, nombre) {
  htmlwidgets::saveWidget(p, file.path(getwd(), "output/graficos", paste0(nombre, ".html")),
                          selfcontained = TRUE)
}

# ---------------- PARTE A · CREDITCARD ----------------

# ---------- 2.1 Duplicados ----------
n_antes <- nrow(cc)
n_dup   <- sum(duplicated(cc))
cc      <- cc[!duplicated(cc), ]
cat("Duplicados eliminados:", n_dup, "| Filas finales:", nrow(cc), "\n")

registrar("B2", "Eliminación de duplicados (creditcard)",
          paste("Se eliminaron", n_dup, "filas idénticas"),
          "Registros repetidos inflan la densidad y sesgan GMM y LOF",
          filas_antes = n_antes, filas_despues = nrow(cc))

# ---------- 2.2 Montos en cero ----------
ceros_amt  <- sum(cc$Amount == 0)
fraude_cero <- sum(cc$Amount == 0 & cc$Class == 1)
cat("Transacciones con Amount = 0:", ceros_amt, "| de ellas fraude:", fraude_cero, "\n")

registrar("B2", "Revisión de montos en cero (creditcard)",
          paste("Se conservan", ceros_amt, "transacciones con monto 0 (", fraude_cero, "son fraude)"),
          "Pueden ser verificaciones de tarjeta; contienen fraudes, por eso no se eliminan")

# ---------- 2.3 Desbalance de clases ----------
tabla_clase <- as.data.frame(table(Class = cc$Class))
tabla_clase$etiqueta <- ifelse(tabla_clase$Class == 1, "Fraude", "Normal")
tabla_clase$pct <- round(tabla_clase$Freq / sum(tabla_clase$Freq) * 100, 3)
print(tabla_clase)

g_desbalance <- plot_ly(tabla_clase, labels = ~etiqueta, values = ~Freq, type = "pie",
                        hole = 0.62, sort = FALSE,
                        marker = list(colors = c(col("turquesa"), col("magenta")),
                                      line = list(color = col("crema"), width = 4)),
                        textinfo = "label+percent",
                        hovertemplate = "<b>%{label}</b><br>%{value:,} transacciones<br>%{percent}<extra></extra>") |>
  layout(showlegend = FALSE,
         annotations = list(text = paste0("<b>", tabla_clase$Freq[2], "</b><br>fraudes"),
                            showarrow = FALSE, font = list(size = 22, color = col("magenta")))) |>
  tema_aguayo("Desbalance de clases · creditcard",
              "El fraude es la anomalía: menos del 0,2 % de las transacciones")
print(g_desbalance)
guardar(g_desbalance, "B2_desbalance_creditcard")

registrar("B2", "Análisis de desbalance (creditcard)",
          paste0("Fraudes: ", tabla_clase$Freq[2], " (", tabla_clase$pct[2], " %)"),
          "Justifica el enfoque no supervisado: el fraude es raro, igual que una anomalía")

# ---------------- PARTE B · ASFI BOLIVIA ----------------

# ---------- 2.4 Tipo de entidad según prefijo del código ----------
asfi$entidad <- asfi$TIPO_DE_ENTIDAD
asfi$tipo_entidad <- dplyr::recode(substr(asfi$entidad, 1, 1),
                                   B = "Banco", C = "Cooperativa", I = "IFD",
                                   P = "Banco PyME", V = "Entidad de Vivienda")

tabla_tipo <- asfi |>
  dplyr::distinct(entidad, tipo_entidad) |>
  dplyr::count(tipo_entidad, name = "entidades") |>
  dplyr::arrange(entidades)
print(tabla_tipo)

registrar("B2", "Clasificación por tipo de entidad (ASFI)",
          "Se crea tipo_entidad con el prefijo: B, C, I, P, V",
          "Permite comparar anomalías entre bancos, cooperativas, IFD, etc.")

# ---------- 2.5 Duplicados entidad-mes ----------
dup_asfi <- sum(duplicated(asfi[, c("entidad", "FECHA")]))
cat("Duplicados entidad-mes:", dup_asfi, "\n")
registrar("B2", "Revisión de duplicados (ASFI)",
          paste("Duplicados entidad-mes:", dup_asfi),
          "Cada entidad debe tener un solo reporte por mes")

# ---------- 2.6 Meses inactivos (todas las cuentas en cero) ----------
cols_num <- names(asfi)[sapply(asfi, is.numeric)]
asfi$activo <- rowSums(asfi[, cols_num] != 0) > 0

inactivos <- asfi |>
  dplyr::filter(!activo) |>
  dplyr::count(entidad, tipo_entidad, name = "meses_en_cero") |>
  dplyr::arrange(dplyr::desc(meses_en_cero))
print(inactivos)

orden <- asfi |> dplyr::distinct(entidad, tipo_entidad) |>
  dplyr::arrange(tipo_entidad, entidad)
asfi_mapa <- asfi |>
  dplyr::mutate(entidad = factor(entidad, levels = rev(orden$entidad)),
                estado = ifelse(activo, "Activa", "Sin datos / cerrada"))

g_actividad <- plot_ly(asfi_mapa, x = ~FECHA, y = ~entidad, z = ~as.numeric(activo),
                       type = "heatmap", xgap = 1, ygap = 1, showscale = FALSE,
                       colorscale = list(c(0, col("magenta")), c(1, col("verde"))),
                       text = ~paste0("<b>", entidad, "</b> · ", tipo_entidad,
                                      "<br>", format(FECHA, "%b %Y"), "<br>", estado),
                       hoverinfo = "text", height = 1100) |>
  layout(xaxis = list(title = ""), yaxis = list(title = "", tickfont = list(size = 9)),
         shapes = list(list(type = "line", x0 = as.Date("2023-04-26"), x1 = as.Date("2023-04-26"),
                            y0 = 0, y1 = 1, yref = "paper",
                            line = list(color = col("amarillo"), width = 3, dash = "dot"))),
         annotations = list(list(x = as.Date("2023-04-26"), y = 1.01, yref = "paper",
                                 text = "<b>Intervención Fassil</b>", showarrow = FALSE,
                                 font = list(color = col("rojo"))))) |>
  tema_aguayo("Actividad de las 69 entidades financieras · 2020-2023",
              "Verde = reporta datos · Rojo = todas las cuentas en cero (cerrada o sin reporte)")
print(g_actividad)
guardar(g_actividad, "B2_actividad_entidades_asfi")

n_antes <- nrow(asfi)
asfi_act <- asfi[asfi$activo, ]
cat("Filas ASFI activas:", nrow(asfi_act), "de", n_antes, "\n")

registrar("B2", "Eliminación de meses inactivos (ASFI)",
          paste("Se quitaron", n_antes - nrow(asfi_act),
                "registros con todas las cuentas en cero (ej. Fassil desde abr-2023)"),
          "Un mes sin operaciones no es una anomalía de comportamiento, es ausencia de datos",
          filas_antes = n_antes, filas_despues = nrow(asfi_act))

# ---------- 2.7 % de ceros por variable (en meses activos) ----------
ceros_var <- data.frame(
  variable = cols_num,
  pct_ceros = round(colMeans(asfi_act[, cols_num] == 0) * 100, 1)
) |> dplyr::left_join(diccionario, by = c("variable" = "codigo")) |>
  dplyr::arrange(pct_ceros)
ceros_var$utilidad <- ifelse(ceros_var$pct_ceros <= 40, "Útil (≤ 40 % ceros)", "Descartar (> 40 % ceros)")
print(ceros_var[, c("variable", "pct_ceros", "utilidad")])

g_ceros <- plot_ly(ceros_var, x = ~pct_ceros, y = ~reorder(variable, -pct_ceros),
                   type = "bar", orientation = "h", color = ~utilidad,
                   colors = c(col("magenta"), col("verde")),
                   text = ~paste0("<b>", variable, "</b><br>",
                                  ifelse(is.na(descripcion), "Subcuenta de 414", descripcion),
                                  "<br>Ceros: ", pct_ceros, " %"),
                   hoverinfo = "text") |>
  layout(xaxis = list(title = "% de meses en cero", range = c(0, 105)),
         yaxis = list(title = ""), legend = list(orientation = "h", y = -0.12),
         shapes = list(list(type = "line", x0 = 40, x1 = 40, y0 = 0, y1 = 1, yref = "paper",
                            line = list(color = col("tinta"), dash = "dash")))) |>
  tema_aguayo("¿Qué cuentas ASFI sirven para detectar anomalías?",
              "Variables con demasiados ceros no aportan información a GMM ni LOF")
print(g_ceros)
guardar(g_ceros, "B2_ceros_variables_asfi")

vars_utiles <- ceros_var$variable[ceros_var$pct_ceros <= 40]
cat("Variables útiles:", paste(vars_utiles, collapse = ", "), "\n")

registrar("B2", "Selección de cuentas ASFI por % de ceros",
          paste("Se conservan:", paste(vars_utiles, collapse = ", ")),
          "Cuentas con más de 40 % de ceros casi no varían y distorsionan la densidad")


# =====================================================
# BLOQUE 3 · TRANSFORMACIONES Y VARIABLES NUEVAS
# =====================================================

# ---------------- PARTE A · CREDITCARD ----------------

# ---------- 3.1 Variables derivadas ----------
TC_EUR_BS <- 9.25   # referencial 2013: 1 EUR ≈ 1,33 USD × 6,96 Bs

cc <- cc |>
  dplyr::mutate(
    hora       = floor(Time / 3600) %% 24,          # hora relativa al inicio del registro
    dia        = ifelse(Time < 86400, "Día 1", "Día 2"),
    log_amount = log1p(Amount),
    amount_bs  = round(Amount * TC_EUR_BS, 2)
  )

registrar("B3", "Variables derivadas (creditcard)",
          "Se crean hora, dia, log_amount = log(1+Amount) y amount_bs (× 9,25)",
          "Amount es muy asimétrico; el log lo acerca a una forma gaussiana, útil para GMM")

# ---------- 3.2 Gráfico: monto original vs log ----------
binear <- function(x, n) {
  h <- hist(x, breaks = n, plot = FALSE)
  data.frame(centro = h$mids, frecuencia = h$counts)
}
b1 <- binear(cc$Amount[cc$Amount <= 1000], 80)
b2 <- binear(cc$log_amount, 60)

h1 <- plot_ly(b1, x = ~centro, y = ~frecuencia, type = "bar", name = "Monto original (≤ 1000 €)",
              marker = list(color = col("naranja")),
              hovertemplate = "Monto ≈ %{x:.0f} €<br>%{y:,} transacciones<extra></extra>")
h2 <- plot_ly(b2, x = ~centro, y = ~frecuencia, type = "bar", name = "log(1 + monto)",
              marker = list(color = col("turquesa")),
              hovertemplate = "log ≈ %{x:.2f}<br>%{y:,} transacciones<extra></extra>")

g_amount <- subplot(h1, h2, nrows = 1, titleX = TRUE, margin = 0.06) |>
  layout(bargap = 0.05, legend = list(orientation = "h", y = -0.2),
         xaxis = list(title = "Monto (€)"), xaxis2 = list(title = "log(1 + monto)")) |>
  tema_aguayo("Transformación logarítmica del monto",
              "Izquierda: muy sesgado a la derecha · Derecha: forma más simétrica")
print(g_amount)
guardar(g_amount, "B3_transformacion_monto")

# ---------- 3.3 Gráfico: fraude por hora ----------
por_hora <- cc |>
  dplyr::group_by(hora) |>
  dplyr::summarise(transacciones = dplyr::n(), fraudes = sum(Class),
                   tasa = round(fraudes / transacciones * 1000, 2), .groups = "drop")

g_hora <- plot_ly(por_hora) |>
  add_bars(x = ~hora, y = ~transacciones, name = "Transacciones",
           marker = list(color = col("turquesa")),
           hovertemplate = "Hora %{x}<br>%{y:,} transacciones<extra></extra>") |>
  add_trace(x = ~hora, y = ~tasa, name = "Fraudes por cada 1000", yaxis = "y2",
            type = "scatter", mode = "lines+markers",
            line = list(color = col("magenta"), width = 3),
            marker = list(color = col("amarillo"), size = 9,
                          line = list(color = col("magenta"), width = 2)),
            hovertemplate = "Hora %{x}<br>%{y} fraudes por cada 1000<extra></extra>") |>
  layout(xaxis = list(title = "Hora relativa del día", dtick = 2),
         yaxis = list(title = "Transacciones"),
         yaxis2 = list(title = "Fraudes por cada 1000", overlaying = "y", side = "right"),
         legend = list(orientation = "h", y = -0.2)) |>
  tema_aguayo("¿A qué hora ocurre el fraude?",
              "En las horas de poca actividad la proporción de fraude se dispara")
print(g_hora)
guardar(g_hora, "B3_fraude_por_hora")

# ---------------- PARTE B · ASFI BOLIVIA ----------------

# ---------- 3.4 Solo entidades que captan depósitos ----------
n_antes <- nrow(asfi_act)
asfi_dep <- asfi_act |> dplyr::filter(CD_210_00 > 0)
registrar("B3", "Filtro: meses con depósitos del público (ASFI)",
          paste("Se quitaron", n_antes - nrow(asfi_dep), "registros sin depósitos (cuenta 210 = 0)"),
          "El caso Fassil es una corrida de depósitos; se analizan entidades que captan ahorro",
          filas_antes = n_antes, filas_despues = nrow(asfi_dep))

# ---------- 3.5 Desacumular gastos y crear indicadores ----------
asfi_tr <- asfi_dep |>
  dplyr::arrange(entidad, FECHA) |>
  dplyr::mutate(anio = lubridate::year(FECHA), mes = lubridate::month(FECHA)) |>
  # a) gastos 4xx vienen acumulados en el año -> gasto del mes
  dplyr::group_by(entidad, anio) |>
  dplyr::mutate(
    consecutivo       = dplyr::lag(mes) == mes - 1,
    gasto_publico_mes = dplyr::case_when(mes == 1    ~ CD_411_00,
                                         consecutivo ~ CD_411_00 - dplyr::lag(CD_411_00),
                                         TRUE        ~ NA_real_),
    gasto_bancos_mes  = dplyr::case_when(mes == 1    ~ CD_413_00,
                                         consecutivo ~ CD_413_00 - dplyr::lag(CD_413_00),
                                         TRUE        ~ NA_real_)
  ) |>
  # b) variaciones mensuales
  dplyr::group_by(entidad) |>
  dplyr::mutate(
    mes_ant_ok       = as.numeric(FECHA - dplyr::lag(FECHA)) <= 31,
    var_depositos    = ifelse(mes_ant_ok, (CD_210_00 / dplyr::lag(CD_210_00) - 1) * 100, NA),
    var_oblig_bancos = ifelse(mes_ant_ok & dplyr::lag(CD_230_00) > 0,
                              (CD_230_00 / dplyr::lag(CD_230_00) - 1) * 100, 0)
  ) |>
  dplyr::ungroup() |>
  # c) indicadores de costo y estructura
  dplyr::mutate(
    pasivo_total       = CD_210_00 + CD_220_00 + CD_230_00 + CD_240_00 + CD_280_00,
    costo_publico      = gasto_publico_mes / CD_210_00 * 12 * 100,      # % anual
    costo_bancos       = ifelse(CD_230_00 > 0, gasto_bancos_mes / CD_230_00 * 12 * 100, 0),
    log_depositos      = log1p(CD_210_00),
    log_oblig_bancos   = log1p(CD_230_00),
    ratio_cxp          = CD_240_00 / pasivo_total * 100,
    ratio_emp_publicas = CD_280_00 / pasivo_total * 100,
    depositos_mill_bs  = CD_210_00 / 1e6
  )

cat("Gastos mensuales negativos (ajustes contables):",
    sum(asfi_tr$gasto_publico_mes < 0, na.rm = TRUE) + sum(asfi_tr$gasto_bancos_mes < 0, na.rm = TRUE), "\n")

registrar("B3", "Desacumulación de gastos financieros (ASFI)",
          "gasto_mes = acumulado del mes - acumulado del mes anterior (enero queda igual)",
          "Las cuentas 4xx se reportan acumuladas en el año; sin esto el gasto crece artificialmente")

registrar("B3", "Indicadores financieros (ASFI)",
          paste("var_depositos, var_oblig_bancos, costo_publico, costo_bancos (% anual),",
                "log_depositos, log_oblig_bancos, ratio_cxp, ratio_emp_publicas"),
          "Una corrida de depósitos se manifiesta como caída brusca de depósitos y cambio en costos")

# ---------- 3.6 Dataset final para modelar ----------
vars_asfi <- c("log_depositos", "var_depositos", "costo_publico", "log_oblig_bancos",
               "var_oblig_bancos", "costo_bancos", "ratio_cxp", "ratio_emp_publicas")

n_antes <- nrow(asfi_tr)
asfi_modelo <- asfi_tr[complete.cases(asfi_tr[, vars_asfi]), ]
cat("Registros ASFI listos para modelar:", nrow(asfi_modelo),
    "| Entidades:", length(unique(asfi_modelo$entidad)), "\n")

registrar("B3", "Dataset final ASFI",
          paste("Se quitan", n_antes - nrow(asfi_modelo),
                "registros sin mes anterior (primer mes de cada entidad o saltos)"),
          "Las variaciones necesitan el mes previo; sin él no se pueden calcular",
          filas_antes = n_antes, filas_despues = nrow(asfi_modelo))

summary(asfi_modelo[, vars_asfi])

# ---------- 3.7 Gráfico: la caída del Banco Fassil ----------
fassil <- asfi |>
  dplyr::filter(entidad == "BFS") |>
  dplyr::arrange(FECHA) |>
  dplyr::mutate(dep = CD_210_00 / 1e6,
                var = (CD_210_00 / dplyr::lag(CD_210_00) - 1) * 100,
                var = ifelse(is.finite(var), var, NA))

g_fassil <- plot_ly(fassil) |>
  add_bars(x = ~FECHA, y = ~var, name = "Variación mensual (%)", yaxis = "y2",
           marker = list(color = ~ifelse(var < 0, col("magenta"), col("verde")), opacity = 0.55),
           hovertemplate = "%{x|%b %Y}<br>Variación: %{y:.2f} %<extra></extra>") |>
  add_trace(x = ~FECHA, y = ~dep, name = "Depósitos (millones Bs)",
            type = "scatter", mode = "lines+markers",
            line = list(color = col("azul"), width = 3, shape = "spline"),
            marker = list(color = col("amarillo"), size = 7, line = list(color = col("azul"), width = 1.5)),
            hovertemplate = "%{x|%b %Y}<br>Bs %{y:,.0f} millones<extra></extra>") |>
  layout(xaxis = list(title = ""),
         yaxis = list(title = "Depósitos del público (millones Bs)"),
         yaxis2 = list(title = "Variación mensual (%)", overlaying = "y", side = "right",
                       zeroline = TRUE, range = c(-15, 15)),
         legend = list(orientation = "h", y = -0.15),
         shapes = list(
           list(type = "rect", x0 = as.Date("2023-03-13"), x1 = as.Date("2023-04-26"),
                y0 = 0, y1 = 1, yref = "paper", fillcolor = col("rojo"), opacity = 0.12, line = list(width = 0)),
           list(type = "line", x0 = as.Date("2023-04-26"), x1 = as.Date("2023-04-26"),
                y0 = 0, y1 = 1, yref = "paper", line = list(color = col("rojo"), width = 2, dash = "dot"))),
         annotations = list(
           list(x = as.Date("2023-03-13"), y = 0.95, yref = "paper", xanchor = "right",
                text = "13 mar 2023<br><b>Corrida de depósitos</b>", showarrow = TRUE, ax = -60, ay = 0,
                font = list(color = col("rojo"))),
           list(x = as.Date("2023-04-26"), y = 0.6, yref = "paper", xanchor = "left",
                text = "26 abr 2023<br><b>Intervención ASFI</b>", showarrow = TRUE, ax = 50, ay = 0,
                font = list(color = col("rojo"))))) |>
  tema_aguayo("Banco Fassil: depósitos del público 2020-2023",
              "La anomalía que buscaremos con GMM y LOF")
print(g_fassil)
guardar(g_fassil, "B3_caso_fassil")

# ---------- 3.8 Gráfico: variación de depósitos por tipo de entidad ----------
fassil_pts <- asfi_modelo |> dplyr::filter(entidad == "BFS", FECHA >= as.Date("2022-10-01"))

g_var <- plot_ly() |>
  add_boxplot(data = asfi_modelo, x = ~tipo_entidad, y = ~var_depositos, color = ~tipo_entidad,
              colors = c(col("azul"), col("naranja"), col("verde"), col("morado")),
              boxpoints = "outliers", name = ~tipo_entidad,
              text = ~paste0(entidad, " · ", format(FECHA, "%b %Y")), hoverinfo = "text+y") |>
  add_markers(data = fassil_pts, x = ~tipo_entidad, y = ~var_depositos, name = "Fassil (oct-22 a mar-23)",
              marker = list(symbol = "star", size = 16, color = col("magenta"),
                            line = list(color = col("tinta"), width = 1)),
              text = ~paste0("<b>FASSIL</b> · ", format(FECHA, "%b %Y"),
                             "<br>", round(var_depositos, 2), " %"), hoverinfo = "text") |>
  layout(yaxis = list(title = "Variación mensual de depósitos (%)", range = c(-15, 15)),
         xaxis = list(title = ""), legend = list(orientation = "h", y = -0.15)) |>
  tema_aguayo("Variación mensual de depósitos por tipo de entidad",
              "Estrellas: Banco Fassil en los meses previos a su intervención (zoom ±15 %)")
print(g_var)
guardar(g_var, "B3_variacion_depositos_tipo")

# ---------- 3.9 Guardar datos procesados ----------
saveRDS(cc, "data/cc_procesado.rds")
saveRDS(asfi_modelo, "data/asfi_modelo.rds")
saveRDS(asfi, "data/asfi_completo.rds")
registrar("B3", "Guardado de datos procesados",
          "cc_procesado.rds, asfi_modelo.rds y asfi_completo.rds en data/",
          "Reanudar el trabajo sin repetir la carga y la limpieza")



# =====================================================
# BLOQUE 4 · MEZCLAS GAUSSIANAS (GMM)
# =====================================================

# ---------- 4.0 Funciones de apoyo ----------
metricas <- function(pred, real) {
  TP <- sum(pred == 1 & real == 1); FP <- sum(pred == 1 & real == 0)
  FN <- sum(pred == 0 & real == 1); TN <- sum(pred == 0 & real == 0)
  precision <- TP / (TP + FP); recall <- TP / (TP + FN)
  data.frame(TP, FP, FN, TN,
             precision = round(precision, 4), recall = round(recall, 4),
             F1 = round(2 * precision * recall / (precision + recall), 4))
}

auc_pr <- function(score, real) {                      # área bajo la curva precisión-recall
  o <- order(score, decreasing = TRUE); y <- real[o]
  tp <- cumsum(y); prec <- tp / seq_along(y); rec <- tp / sum(y)
  sum(diff(c(0, rec)) * prec)
}

slog <- function(x) sign(x) * log1p(abs(x))            # log con signo (para variaciones %)

grafico_bic <- function(modelo, titulo) {
  b <- as.data.frame(unclass(modelo$BIC)[, , drop = FALSE])
  b$G <- as.numeric(rownames(b))
  b <- tidyr::pivot_longer(b, -G, names_to = "modelo", values_to = "BIC")
  b <- b[!is.na(b$BIC), ]
  plot_ly(b, x = ~G, y = ~BIC, color = ~modelo, type = "scatter", mode = "lines+markers",
          colors = unname(aguayo[1:8]),
          hovertemplate = "%{fullData.name} · G = %{x}<br>BIC = %{y:,.0f}<extra></extra>") |>
    layout(xaxis = list(title = "Número de componentes (G)", dtick = 1),
           yaxis = list(title = "BIC (mayor es mejor)"),
           annotations = list(list(x = modelo$G, y = max(b$BIC), showarrow = TRUE,
                                   text = paste0("<b>Elegido: ", modelo$modelName, ", G = ", modelo$G, "</b>"),
                                   font = list(color = col("magenta"))))) |>
    tema_aguayo(titulo, "Mclust elige el modelo y el número de componentes con mayor BIC")
}

# =====================================================
# PARTE A · CREDITCARD (validación)
# =====================================================

# ---------- 4.1 Matriz de entrada (sin usar Class) ----------
vars_cc <- c(paste0("V", 1:28), "log_amount")
X_cc <- scale(cc[, vars_cc])

set.seed(2026)
idx_ajuste <- sample(nrow(X_cc), 20000)      # muestra aleatoria para ajustar (no ve la etiqueta)

registrar("B4", "Preparación de entrada GMM (creditcard)",
          "V1-V28 + log_amount estandarizadas; ajuste con muestra aleatoria de 20 000 filas",
          "Mclust con 283 mil filas es muy costoso; la muestra conserva la estructura de los datos")

# ---------- 4.2 Ajuste del GMM ----------
t0 <- Sys.time()
gmm_cc <- Mclust(X_cc[idx_ajuste, ], G = 1:9, modelNames = c("VVI", "EEE", "VVV"),
                 initialization = list(subset = sample(20000, 2000)), verbose = FALSE)
cat("Tiempo de ajuste:", round(difftime(Sys.time(), t0, units = "secs"), 1), "seg\n")
summary(gmm_cc)

registrar("B4", "Ajuste GMM (creditcard)",
          paste0("Modelo elegido por BIC: ", gmm_cc$modelName, " con G = ", gmm_cc$G, " componentes"),
          "El BIC equilibra ajuste y complejidad del modelo")

g_bic_cc <- grafico_bic(gmm_cc, "Selección del GMM · creditcard")
print(g_bic_cc); guardar(g_bic_cc, "B4_bic_creditcard")

# ---------- 4.3 Puntaje de anomalía = baja densidad ----------
cc$logdens_gmm <- dens(X_cc, modelName = gmm_cc$modelName,
                       parameters = gmm_cc$parameters, logarithm = TRUE)
cc$score_gmm <- -cc$logdens_gmm                       # más alto = más anómalo

umbral_pct <- 0.01                                     # 1 % de menor densidad
umbral_gmm <- quantile(cc$logdens_gmm, umbral_pct)
cc$anom_gmm <- as.integer(cc$logdens_gmm < umbral_gmm)

m_gmm_cc <- metricas(cc$anom_gmm, cc$Class)
auc_roc_gmm <- as.numeric(pROC::auc(cc$Class, cc$score_gmm, quiet = TRUE))
auc_pr_gmm  <- auc_pr(cc$score_gmm, cc$Class)
m_gmm_cc$AUC_ROC <- round(auc_roc_gmm, 4); m_gmm_cc$AUC_PR <- round(auc_pr_gmm, 4)
print(m_gmm_cc)

# Sensibilidad al umbral (como en el material: percentil 5 %)
sens_gmm <- do.call(rbind, lapply(c(0.005, 0.01, 0.02, 0.05), function(p) {
  pr <- as.integer(cc$logdens_gmm < quantile(cc$logdens_gmm, p))
  cbind(percentil = paste0(p * 100, " %"), metricas(pr, cc$Class))
}))
print(sens_gmm)
write.csv(sens_gmm, "output/tablas/B4_gmm_creditcard_umbrales.csv", row.names = FALSE)

registrar("B4", "Detección de anomalías GMM (creditcard)",
          paste0("Umbral 1 %: detecta ", m_gmm_cc$TP, " de ", sum(cc$Class), " fraudes (recall ",
                 round(m_gmm_cc$recall * 100, 1), " %) · AUC-ROC ", round(auc_roc_gmm, 3)),
          "Sin ver la etiqueta, el GMM ubica la mayoría de fraudes en zonas de baja densidad")

# ---------- 4.4 Gráfico: densidad de normales vs fraudes ----------
rango <- range(cc$logdens_gmm[cc$logdens_gmm > quantile(cc$logdens_gmm, 0.0005)])
cortes <- seq(rango[1], rango[2], length.out = 70)
dist_dens <- rbind(
  data.frame(clase = "Normal", binear(pmax(cc$logdens_gmm[cc$Class == 0], rango[1]), cortes)),
  data.frame(clase = "Fraude", binear(pmax(cc$logdens_gmm[cc$Class == 1], rango[1]), cortes)))
dist_dens <- dist_dens |> dplyr::group_by(clase) |>
  dplyr::mutate(proporcion = frecuencia / sum(frecuencia) * 100) |> dplyr::ungroup()

g_dens_cc <- plot_ly(dist_dens, x = ~centro, y = ~proporcion, color = ~clase, type = "scatter",
                     mode = "lines", fill = "tozeroy", line = list(shape = "spline", width = 2),
                     colors = c(col("magenta"), col("turquesa")),
                     hovertemplate = "%{fullData.name}<br>log-densidad ≈ %{x:.1f}<br>%{y:.2f} %<extra></extra>") |>
  layout(xaxis = list(title = "log-densidad según el GMM (izquierda = más raro)"),
         yaxis = list(title = "% de transacciones de cada clase"),
         legend = list(orientation = "h", y = -0.2),
         shapes = list(list(type = "line", x0 = umbral_gmm, x1 = umbral_gmm, y0 = 0, y1 = 1,
                            yref = "paper", line = list(color = col("tinta"), dash = "dash", width = 2))),
         annotations = list(list(x = umbral_gmm, y = 0.95, yref = "paper", xanchor = "right",
                                 text = "<b>Umbral 1 %</b> ← anomalías", showarrow = FALSE))) |>
  tema_aguayo("GMM: los fraudes viven en zonas de baja densidad",
              "Distribución de la log-densidad para transacciones normales y fraudulentas")
print(g_dens_cc); guardar(g_dens_cc, "B4_densidad_gmm_creditcard")

# ---------- 4.5 Gráfico: matriz de confusión ----------
grafico_confusion <- function(m, titulo) {
  mc <- data.frame(Real = c("Normal", "Normal", "Fraude", "Fraude"),
                   Predicho = c("Normal", "Anomalía", "Normal", "Anomalía"),
                   n = c(m$TN, m$FP, m$FN, m$TP),
                   tipo = c("Verdadero negativo", "Falso positivo", "Falso negativo", "Verdadero positivo"))
  plot_ly(mc, x = ~Predicho, y = ~Real, z = ~log10(n + 1), type = "heatmap", showscale = FALSE,
          colorscale = list(c(0, col("crema")), c(0.5, col("amarillo")), c(1, col("naranja"))),
          text = ~paste0("<b>", tipo, "</b><br>", format(n, big.mark = " ")), hoverinfo = "text") |>
    add_annotations(x = mc$Predicho, y = mc$Real, showarrow = FALSE,
                    text = paste0("<b>", format(mc$n, big.mark = " "), "</b><br>", mc$tipo),
                    font = list(size = 15, color = col("tinta"))) |>
    layout(xaxis = list(title = "Predicho por el modelo"), yaxis = list(title = "Real (Class)")) |>
    tema_aguayo(titulo, "Class solo se usa aquí, para evaluar")
}
g_conf_gmm <- grafico_confusion(m_gmm_cc, "Matriz de confusión · GMM (umbral 1 %)")
print(g_conf_gmm); guardar(g_conf_gmm, "B4_confusion_gmm_creditcard")

# ---------- 4.6 Gráfico: espacio V14 vs V17 ----------
set.seed(2026)
muestra_plot <- c(sample(which(cc$Class == 0), 8000), which(cc$Class == 1))
pl <- cc[muestra_plot, ]
pl$resultado <- dplyr::case_when(pl$Class == 1 & pl$anom_gmm == 1 ~ "Fraude detectado",
                                 pl$Class == 1 & pl$anom_gmm == 0 ~ "Fraude no detectado",
                                 pl$Class == 0 & pl$anom_gmm == 1 ~ "Falsa alarma",
                                 TRUE ~ "Normal")
pl$resultado <- factor(pl$resultado, levels = c("Normal", "Falsa alarma", "Fraude no detectado", "Fraude detectado"))

g_esp_gmm <- plot_ly(pl, x = ~V14, y = ~V17, color = ~resultado, type = "scattergl", mode = "markers",
                     colors = c("#BFB8AD", col("amarillo"), col("azul"), col("magenta")),
                     marker = list(size = 6, opacity = 0.75),
                     text = ~paste0(resultado, "<br>Monto: ", Amount, " € (Bs ", amount_bs, ")"),
                     hoverinfo = "text") |>
  layout(legend = list(orientation = "h", y = -0.15)) |>
  tema_aguayo("Resultado del GMM en el plano V14 – V17",
              "8 000 normales al azar + todos los fraudes")
print(g_esp_gmm); guardar(g_esp_gmm, "B4_espacio_gmm_creditcard")

# =====================================================
# PARTE B · ASFI BOLIVIA
# =====================================================

# ---------- 4.7 Preparación: log con signo + estandarización ----------
asfi_modelo <- as.data.frame(asfi_modelo)
T_asfi <- asfi_modelo[, vars_asfi]
for (v in c("var_depositos", "var_oblig_bancos", "costo_publico", "costo_bancos",
            "ratio_cxp", "ratio_emp_publicas")) T_asfi[[v]] <- slog(T_asfi[[v]])

registrar("B4", "Transformación log con signo (ASFI)",
          "Se aplica sign(x)·log(1+|x|) a variaciones, costos y ratios",
          "Hay variaciones extremas (hasta +4245 %) que dominarían el escalado; el log las comprime sin perder el signo")

# ---------- 4.8 NIVEL 1: todo el sistema financiero ----------
X_sis <- scale(T_asfi)
gmm_sis <- Mclust(X_sis, G = 1:9, verbose = FALSE)
summary(gmm_sis)

asfi_modelo$logdens_gmm_sis <- dens(X_sis, gmm_sis$modelName, gmm_sis$parameters, logarithm = TRUE)
asfi_modelo$pct_gmm_sis <- round(rank(asfi_modelo$logdens_gmm_sis) / nrow(asfi_modelo) * 100, 1)
asfi_modelo$anom_gmm_sis <- as.integer(asfi_modelo$pct_gmm_sis <= 5)   # 5 % menos denso
cat("Anomalías GMM (sistema):", sum(asfi_modelo$anom_gmm_sis), "\n")

registrar("B4", "GMM nivel sistema (ASFI)",
          paste0(gmm_sis$modelName, ", G = ", gmm_sis$G, " · ", sum(asfi_modelo$anom_gmm_sis),
                 " entidad-mes anómalos (5 % de menor densidad)"),
          "Umbral del 5 % como en el ejemplo del material de clase")

g_bic_sis <- grafico_bic(gmm_sis, "Selección del GMM · sistema financiero boliviano")
print(g_bic_sis); guardar(g_bic_sis, "B4_bic_asfi_sistema")

top_sis <- asfi_modelo |>
  dplyr::arrange(logdens_gmm_sis) |>
  dplyr::select(entidad, tipo_entidad, FECHA, var_depositos, costo_publico,
                var_oblig_bancos, pct_gmm_sis) |>
  head(20)
print(top_sis)
write.csv(top_sis, "output/tablas/B4_top20_gmm_sistema.csv", row.names = FALSE)

cat("\nFassil en el nivel sistema (percentil de densidad, menor = más raro):\n")
print(asfi_modelo |> dplyr::filter(entidad == "BFS", FECHA >= as.Date("2022-10-01")) |>
        dplyr::select(FECHA, var_depositos, pct_gmm_sis, anom_gmm_sis))

# Gráfico: mapa del sistema (variación de depósitos vs costo), color = rareza GMM
asfi_modelo$rareza_sis <- 100 - asfi_modelo$pct_gmm_sis
g_sis <- plot_ly(asfi_modelo, x = ~slog(var_depositos), y = ~slog(costo_publico),
                 type = "scatter", mode = "markers",
                 marker = list(size = ~ifelse(anom_gmm_sis == 1, 11, 6),
                               color = ~rareza_sis, showscale = TRUE,
                               colorscale = list(c(0, col("turquesa")), c(0.6, col("amarillo")),
                                                 c(0.9, col("naranja")), c(1, col("magenta"))),
                               colorbar = list(title = "Rareza<br>GMM"),
                               line = list(color = ~ifelse(entidad == "BFS", col("tinta"), "rgba(0,0,0,0)"),
                                           width = 2), opacity = 0.85),
                 text = ~paste0("<b>", entidad, "</b> · ", tipo_entidad, "<br>", format(FECHA, "%b %Y"),
                                "<br>Var. depósitos: ", round(var_depositos, 2), " %",
                                "<br>Costo público: ", round(costo_publico, 2), " %",
                                "<br>Percentil densidad: ", pct_gmm_sis), hoverinfo = "text") |>
  layout(xaxis = list(title = "Variación de depósitos (log con signo)"),
         yaxis = list(title = "Costo de obligaciones con el público (log con signo)")) |>
  tema_aguayo("GMM sobre 67 entidades financieras de Bolivia",
              "Cada punto es una entidad en un mes · borde negro = Banco Fassil")
print(g_sis); guardar(g_sis, "B4_gmm_asfi_sistema")

# ---------- 4.9 NIVEL 2: solo bancos (grupo de pares de Fassil) ----------
vars_bancos <- c("log_depositos", "var_depositos", "costo_publico", "var_oblig_bancos")
bancos <- asfi_modelo[asfi_modelo$tipo_entidad == "Banco", ]
X_ban <- scale(T_asfi[asfi_modelo$tipo_entidad == "Banco", vars_bancos])

gmm_ban <- Mclust(X_ban, G = 1:6, verbose = FALSE)
summary(gmm_ban)

bancos$logdens_gmm <- dens(X_ban, gmm_ban$modelName, gmm_ban$parameters, logarithm = TRUE)
bancos$pct_gmm <- round(rank(bancos$logdens_gmm) / nrow(bancos) * 100, 1)
bancos$anom_gmm <- as.integer(bancos$pct_gmm <= 5)

cat("\nFassil frente a los demás bancos:\n")
print(bancos |> dplyr::filter(entidad == "BFS", FECHA >= as.Date("2022-10-01")) |>
        dplyr::select(FECHA, var_depositos, costo_publico, pct_gmm, anom_gmm))

registrar("B4", "GMM nivel bancos (ASFI)",
          paste0(gmm_ban$modelName, ", G = ", gmm_ban$G, " sobre ", nrow(bancos),
                 " banco-mes con 4 variables de liquidez y costo"),
          "Fassil es un banco grande: compararlo con sus pares evita que lo oculte la volatilidad de las cooperativas")

# Gráfico: calendario de rareza banco × mes
bancos$rareza <- 100 - bancos$pct_gmm
g_cal_gmm <- plot_ly(bancos, x = ~FECHA, y = ~entidad, z = ~rareza, type = "heatmap",
                     xgap = 2, ygap = 2,
                     colorscale = list(c(0, col("crema")), c(0.7, col("amarillo")),
                                       c(0.9, col("naranja")), c(1, col("magenta"))),
                     colorbar = list(title = "Rareza<br>GMM"),
                     text = ~paste0("<b>", entidad, "</b> · ", format(FECHA, "%b %Y"),
                                    "<br>Var. depósitos: ", round(var_depositos, 2), " %",
                                    "<br>Percentil densidad: ", pct_gmm,
                                    ifelse(anom_gmm == 1, "<br><b>⚠ ANOMALÍA</b>", "")),
                     hoverinfo = "text", height = 550) |>
  layout(xaxis = list(title = ""), yaxis = list(title = ""),
         shapes = list(list(type = "line", x0 = as.Date("2023-04-26"), x1 = as.Date("2023-04-26"),
                            y0 = 0, y1 = 1, yref = "paper",
                            line = list(color = col("rojo"), width = 2, dash = "dot")))) |>
  tema_aguayo("Calendario de rareza GMM · 14 bancos de Bolivia",
              "Más magenta = mes más atípico para ese banco · línea roja = intervención Fassil")
print(g_cal_gmm); guardar(g_cal_gmm, "B4_calendario_gmm_bancos")

# ---------- 4.10 Guardar resultados ----------
saveRDS(cc, "data/cc_procesado.rds")
saveRDS(asfi_modelo, "data/asfi_modelo.rds")
saveRDS(bancos, "data/bancos_modelo.rds")
saveRDS(list(gmm_cc = gmm_cc, gmm_sis = gmm_sis, gmm_ban = gmm_ban), "output/modelos_gmm.rds")
registrar("B4", "Guardado de resultados GMM",
          "Puntajes y modelos guardados en data/ y output/",
          "Se usarán en la comparación y en el dashboard")


# =====================================================
# BLOQUE 5 · LOCAL OUTLIER FACTOR (LOF)
# =====================================================

# ---------- 5.0 LOF a partir de los k vecinos más cercanos ----------
# Se calculan los vecinos UNA vez y luego el LOF para varios k (mucho más rápido).
# Es la misma fórmula de dbscan::lof (se verifica abajo):
#   reach-dist(p,o) = max(k-dist(o), d(p,o))
#   lrd(p)          = 1 / promedio(reach-dist(p, vecinos))
#   LOF(p)          = promedio(lrd(vecinos)) / lrd(p)
lof_knn <- function(kn, k) {
  id <- kn$id[, 1:k, drop = FALSE]; d <- kn$dist[, 1:k, drop = FALSE]
  kdist <- d[, k]
  reach <- pmax(matrix(kdist[id], nrow(id)), d)
  lrd <- 1 / (rowMeans(reach) + 1e-12)
  rowMeans(matrix(lrd[id], nrow(id))) / lrd
}

# Verificación con dbscan::lof (deben coincidir: diferencia ≈ 0)
set.seed(1); Xp <- matrix(rnorm(3000), ncol = 3)
cat("Diferencia máxima con dbscan::lof:",
    max(abs(dbscan::lof(Xp, minPts = 11) - lof_knn(kNN(Xp, k = 10), 10))), "\n")

registrar("B5", "Implementación de LOF desde kNN",
          "lof_knn() calcula el LOF para varios k con una sola búsqueda de vecinos; verificada contra dbscan::lof",
          "Probar varios k sin repetir la búsqueda de vecinos, que es la parte más costosa")

# =====================================================
# PARTE A · CREDITCARD (validación)
# =====================================================

# ---------- 5.1 Muestra aleatoria (proporción natural de fraude) ----------
set.seed(2026)
idx_lof <- sample(nrow(cc), 40000)
cc_lof <- cc[idx_lof, ]
X_lof  <- X_cc[idx_lof, ]
cat("Muestra LOF:", nrow(cc_lof), "transacciones |", sum(cc_lof$Class), "fraudes\n")

registrar("B5", "Muestra para LOF (creditcard)",
          paste0("40 000 transacciones al azar (", sum(cc_lof$Class), " fraudes, proporción natural)"),
          "LOF compara cada punto con sus vecinos: con 283 mil filas en 29 dimensiones es demasiado costoso",
          filas_antes = nrow(cc), filas_despues = nrow(cc_lof))

# ---------- 5.2 Vecinos más cercanos (la parte lenta: 2 a 5 minutos) ----------
t0 <- Sys.time()
knn_cc <- kNN(X_lof, k = 200)
cat("Tiempo kNN:", round(difftime(Sys.time(), t0, units = "mins"), 1), "min\n")

# ---------- 5.3 Efecto de k (el material advierte que LOF es sensible a k) ----------
ks <- c(5, 10, 20, 50, 100, 200)
efecto_k <- do.call(rbind, lapply(ks, function(k) {
  s <- lof_knn(knn_cc, k)
  pr <- as.integer(s > quantile(s, 0.99))
  data.frame(k = k, AUC_ROC = round(as.numeric(pROC::auc(cc_lof$Class, s, quiet = TRUE)), 4),
             metricas(pr, cc_lof$Class))
}))
print(efecto_k)
write.csv(efecto_k, "output/tablas/B5_lof_efecto_k_creditcard.csv", row.names = FALSE)

auc_gmm_muestra <- as.numeric(pROC::auc(cc_lof$Class, cc_lof$score_gmm, quiet = TRUE))

g_k <- plot_ly(efecto_k, x = ~k, y = ~AUC_ROC, type = "scatter", mode = "lines+markers+text",
               name = "LOF", text = ~round(AUC_ROC, 3), textposition = "top center",
               line = list(color = col("naranja"), width = 4),
               marker = list(size = 12, color = col("amarillo"), line = list(color = col("naranja"), width = 2)),
               hovertemplate = "k = %{x}<br>AUC-ROC = %{y:.3f}<extra></extra>") |>
  add_lines(x = range(ks), y = rep(auc_gmm_muestra, 2), name = "GMM (referencia)",
            line = list(color = col("turquesa"), dash = "dash", width = 2), inherit = FALSE) |>
  layout(xaxis = list(title = "Número de vecinos k", type = "log", tickvals = ks),
         yaxis = list(title = "AUC-ROC", range = c(0.4, 1)),
         shapes = list(list(type = "line", x0 = 5, x1 = 200, y0 = 0.5, y1 = 0.5,
                            line = list(color = col("rojo"), dash = "dot"))),
         annotations = list(list(x = log10(7), y = 0.53, text = "azar (0,5)", showarrow = FALSE,
                                 font = list(color = col("rojo")))),
         legend = list(orientation = "h", y = -0.2)) |>
  tema_aguayo("LOF depende mucho de k",
              "Con k pequeño los fraudes, que forman grupitos, se ven 'normales' entre ellos")
print(g_k); guardar(g_k, "B5_efecto_k_lof_creditcard")

# ---------- 5.4 LOF final (k = 100) ----------
k_cc <- 100
cc_lof$lof <- lof_knn(knn_cc, k_cc)
cc_lof$anom_lof <- as.integer(cc_lof$lof > quantile(cc_lof$lof, 0.99))

m_lof_cc <- metricas(cc_lof$anom_lof, cc_lof$Class)
m_lof_cc$AUC_ROC <- round(as.numeric(pROC::auc(cc_lof$Class, cc_lof$lof, quiet = TRUE)), 4)
m_lof_cc$AUC_PR  <- round(auc_pr(cc_lof$lof, cc_lof$Class), 4)
print(m_lof_cc)
cat("Puntos con LOF > 1.5 (regla del material):", sum(cc_lof$lof > 1.5), "\n")

registrar("B5", "Detección de anomalías LOF (creditcard)",
          paste0("k = ", k_cc, ", umbral 1 %: detecta ", m_lof_cc$TP, " de ", sum(cc_lof$Class),
                 " fraudes · AUC-ROC ", m_lof_cc$AUC_ROC),
          "k = 100 es estable según la curva de efecto de k")

g_conf_lof <- grafico_confusion(m_lof_cc, paste0("Matriz de confusión · LOF (k = ", k_cc, ", umbral 1 %)"))
print(g_conf_lof); guardar(g_conf_lof, "B5_confusion_lof_creditcard")

# ---------- 5.5 Gráfico: puntaje LOF de normales vs fraudes ----------
g_lof_dist <- plot_ly(cc_lof, x = ~ifelse(Class == 1, "Fraude", "Normal"), y = ~lof,
                      color = ~ifelse(Class == 1, "Fraude", "Normal"),
                      colors = c(col("magenta"), col("turquesa")),
                      type = "violin", box = list(visible = TRUE), meanline = list(visible = TRUE),
                      points = FALSE, spanmode = "hard") |>
  layout(yaxis = list(title = "Puntaje LOF (escala log)", type = "log"), xaxis = list(title = ""),
         showlegend = FALSE,
         shapes = list(list(type = "line", x0 = -0.5, x1 = 1.5, y0 = quantile(cc_lof$lof, 0.99),
                            y1 = quantile(cc_lof$lof, 0.99),
                            line = list(color = col("tinta"), dash = "dash")))) |>
  tema_aguayo(paste0("Puntaje LOF por clase (k = ", k_cc, ")"),
              "LOF ≈ 1: densidad similar a sus vecinos · LOF > 1: más aislado que sus vecinos")
print(g_lof_dist); guardar(g_lof_dist, "B5_violin_lof_creditcard")

# ---------- 5.6 Gráfico: espacio V14 – V17 con tamaño según LOF ----------
pl_lof <- cc_lof[cc_lof$Class == 1 | seq_len(nrow(cc_lof)) %in% sample(nrow(cc_lof), 8000), ]
g_esp_lof <- plot_ly(pl_lof, x = ~V14, y = ~V17, type = "scattergl", mode = "markers",
                     marker = list(size = ~pmin(4 + 4 * log(lof), 22),
                                   color = ~log(lof), showscale = TRUE,
                                   colorscale = list(c(0, col("turquesa")), c(0.5, col("amarillo")),
                                                     c(1, col("magenta"))),
                                   colorbar = list(title = "log(LOF)"),
                                   symbol = ~ifelse(Class == 1, "x", "circle"), opacity = 0.8),
                     text = ~paste0(ifelse(Class == 1, "<b>FRAUDE</b>", "Normal"),
                                    "<br>LOF = ", round(lof, 2), "<br>Monto: Bs ", amount_bs),
                     hoverinfo = "text") |>
  tema_aguayo("LOF en el plano V14 – V17",
              "Tamaño y color = puntaje LOF · ✕ = fraude real")
print(g_esp_lof); guardar(g_esp_lof, "B5_espacio_lof_creditcard")

# =====================================================
# PARTE B · ASFI BOLIVIA
# =====================================================

# ---------- 5.7 NIVEL 1: sistema financiero ----------
knn_sis <- kNN(X_sis, k = 50)
k_sis <- 20
asfi_modelo$lof_sis <- lof_knn(knn_sis, k_sis)
asfi_modelo$pct_lof_sis <- round(rank(-asfi_modelo$lof_sis) / nrow(asfi_modelo) * 100, 1)
asfi_modelo$anom_lof_sis <- as.integer(asfi_modelo$pct_lof_sis <= 5)
cat("Anomalías LOF (sistema):", sum(asfi_modelo$anom_lof_sis),
    "| con LOF > 1.5:", sum(asfi_modelo$lof_sis > 1.5), "\n")

registrar("B5", "LOF nivel sistema (ASFI)",
          paste0("k = ", k_sis, " · ", sum(asfi_modelo$anom_lof_sis), " entidad-mes anómalos (5 % con mayor LOF)"),
          "Mismas 8 variables y escalado que el GMM para comparar en igualdad de condiciones")

top_lof_sis <- asfi_modelo |> dplyr::arrange(dplyr::desc(lof_sis)) |>
  dplyr::select(entidad, tipo_entidad, FECHA, var_depositos, costo_publico, lof_sis, pct_lof_sis) |>
  head(20)
print(top_lof_sis)
write.csv(top_lof_sis, "output/tablas/B5_top20_lof_sistema.csv", row.names = FALSE)

# ---------- 5.8 NIVEL 2: bancos ----------
knn_ban <- kNN(X_ban, k = 50)
k_ban <- 10
bancos$lof <- lof_knn(knn_ban, k_ban)
bancos$pct_lof <- round(rank(-bancos$lof) / nrow(bancos) * 100, 1)
bancos$anom_lof <- as.integer(bancos$pct_lof <= 5)

cat("\nFassil frente a los demás bancos (LOF):\n")
print(bancos |> dplyr::filter(entidad == "BFS", FECHA >= as.Date("2022-10-01")) |>
        dplyr::select(FECHA, var_depositos, lof, pct_lof, anom_lof))

registrar("B5", "LOF nivel bancos (ASFI)",
          paste0("k = ", k_ban, " sobre ", nrow(bancos), " banco-mes"),
          "Entre sus pares, el comportamiento de Fassil se compara con bancos de tamaño similar")

# ---------- 5.9 Sensibilidad de Fassil a k (sistema y bancos) ----------
ks_asfi <- c(5, 10, 15, 20, 30, 50)
f_sis <- which(asfi_modelo$entidad == "BFS" & asfi_modelo$FECHA >= as.Date("2022-10-01"))
f_ban <- which(bancos$entidad == "BFS" & bancos$FECHA >= as.Date("2022-10-01"))
sens_fassil <- do.call(rbind, lapply(ks_asfi, function(k) {
  ps <- rank(-lof_knn(knn_sis, k)) / nrow(asfi_modelo) * 100
  pb <- rank(-lof_knn(knn_ban, k)) / nrow(bancos) * 100
  rbind(data.frame(nivel = "Sistema", k = k, FECHA = asfi_modelo$FECHA[f_sis], percentil = round(ps[f_sis], 1)),
        data.frame(nivel = "Bancos",  k = k, FECHA = bancos$FECHA[f_ban],     percentil = round(pb[f_ban], 1)))
}))
write.csv(sens_fassil, "output/tablas/B5_sensibilidad_fassil_k.csv", row.names = FALSE)

g_sens <- subplot(lapply(c("Sistema", "Bancos"), function(nv) {
  d <- sens_fassil[sens_fassil$nivel == nv, ]
  plot_ly(d, x = ~format(FECHA, "%b %y"), y = ~paste("k =", k), z = ~percentil, type = "heatmap",
          zmin = 0, zmax = 40, showscale = (nv == "Bancos"),
          colorscale = list(c(0, col("magenta")), c(0.125, col("naranja")),
                            c(0.3, col("amarillo")), c(1, col("crema"))),
          colorbar = list(title = "Percentil<br>(menor =<br>más raro)"),
          text = ~paste0(nv, " · ", format(FECHA, "%b %Y"), "<br>k = ", k,
                         "<br>Percentil: ", percentil, ifelse(percentil <= 5, "<br><b>⚠ ANOMALÍA</b>", "")),
          hoverinfo = "text") |>
    add_annotations(x = format(d$FECHA, "%b %y"), y = paste("k =", d$k), showarrow = FALSE,
                    text = ifelse(d$percentil <= 5, paste0("<b>", d$percentil, "</b>"), d$percentil),
                    font = list(size = 11, color = col("tinta"))) |>
    layout(xaxis = list(title = nv, categoryorder = "array",
                        categoryarray = format(sort(unique(d$FECHA)), "%b %y")))
}), nrows = 1, titleX = TRUE, shareY = TRUE, margin = 0.04) |>
  tema_aguayo("¿Con qué k detecta LOF al Banco Fassil?",
              "Percentil del puntaje LOF de Fassil (oct-2022 a mar-2023) · ≤ 5 = anomalía")
print(g_sens); guardar(g_sens, "B5_sensibilidad_fassil_k")

# ---------- 5.10 Gráfico: calendario LOF de los bancos ----------
bancos$rareza_lof <- 100 - bancos$pct_lof
g_cal_lof <- plot_ly(bancos, x = ~FECHA, y = ~entidad, z = ~rareza_lof, type = "heatmap",
                     xgap = 2, ygap = 2,
                     colorscale = list(c(0, col("crema")), c(0.7, "#B2DFDB"),
                                       c(0.9, col("turquesa")), c(1, col("azul"))),
                     colorbar = list(title = "Rareza<br>LOF"),
                     text = ~paste0("<b>", entidad, "</b> · ", format(FECHA, "%b %Y"),
                                    "<br>LOF = ", round(lof, 2), "<br>Percentil: ", pct_lof,
                                    ifelse(anom_lof == 1, "<br><b>⚠ ANOMALÍA</b>", "")),
                     hoverinfo = "text", height = 550) |>
  layout(xaxis = list(title = ""), yaxis = list(title = ""),
         shapes = list(list(type = "line", x0 = as.Date("2023-04-26"), x1 = as.Date("2023-04-26"),
                            y0 = 0, y1 = 1, yref = "paper",
                            line = list(color = col("rojo"), width = 2, dash = "dot")))) |>
  tema_aguayo(paste0("Calendario de rareza LOF · 14 bancos (k = ", k_ban, ")"),
              "Más azul = mes más aislado respecto a sus vecinos")
print(g_cal_lof); guardar(g_cal_lof, "B5_calendario_lof_bancos")

# ---------- 5.11 Guardar resultados ----------
saveRDS(cc_lof, "data/cc_lof.rds")
saveRDS(asfi_modelo, "data/asfi_modelo.rds")
saveRDS(bancos, "data/bancos_modelo.rds")
registrar("B5", "Guardado de resultados LOF",
          "cc_lof.rds, asfi_modelo.rds y bancos_modelo.rds actualizados",
          "Se usarán en la comparación de modelos y en el dashboard")


# =====================================================
# BLOQUE 6 · ISOLATION FOREST Y COMPARACIÓN DE MODELOS
# =====================================================

# ---------- 6.0 Isolation Forest (método de referencia) ----------
# Idea: una anomalía se aísla con pocos cortes aleatorios. Puntaje cercano a 1 = anómalo.
if_puntaje <- function(X, seed = 2026) {
  modelo <- isotree::isolation.forest(X, ntrees = 200, sample_size = 256, ndim = 1,
                                      seed = seed, nthreads = 2)
  predict(modelo, X, type = "score")
}

# =====================================================
# PARTE A · CREDITCARD
# =====================================================

# ---------- 6.1 Isolation Forest sobre las 283 726 transacciones ----------
t0 <- Sys.time()
cc$score_if <- if_puntaje(X_cc)
cat("Tiempo Isolation Forest:", round(difftime(Sys.time(), t0, units = "secs"), 1), "seg\n")
cc$anom_if <- as.integer(cc$score_if > quantile(cc$score_if, 0.99))

m_if_cc <- metricas(cc$anom_if, cc$Class)
m_if_cc$AUC_ROC <- round(as.numeric(pROC::auc(cc$Class, cc$score_if, quiet = TRUE)), 4)
m_if_cc$AUC_PR  <- round(auc_pr(cc$score_if, cc$Class), 4)
print(m_if_cc)

registrar("B6", "Isolation Forest (creditcard)",
          paste0("200 árboles, muestra 256 · umbral 1 %: ", m_if_cc$TP, " de ", sum(cc$Class),
                 " fraudes · AUC-ROC ", m_if_cc$AUC_ROC),
          "Método de referencia basado en aislamiento (no en densidad) para comparar con GMM y LOF")

# ---------- 6.2 Comparación justa en la MISMA muestra de 40 000 ----------
cc_lof$score_if <- cc$score_if[idx_lof]
puntajes <- list(GMM = cc_lof$score_gmm, LOF = cc_lof$lof, `Isolation Forest` = cc_lof$score_if)

comparacion_cc <- do.call(rbind, lapply(names(puntajes), function(nm) {
  s <- puntajes[[nm]]
  pr <- as.integer(s > quantile(s, 0.99))
  data.frame(modelo = nm,
             AUC_ROC = round(as.numeric(pROC::auc(cc_lof$Class, s, quiet = TRUE)), 4),
             AUC_PR  = round(auc_pr(s, cc_lof$Class), 4),
             metricas(pr, cc_lof$Class))
}))
comparacion_cc <- comparacion_cc[order(-comparacion_cc$AUC_PR), ]
print(comparacion_cc)
write.csv(comparacion_cc, "output/tablas/B6_comparacion_modelos_creditcard.csv", row.names = FALSE)

# Comparación en los datos completos (GMM e IF; LOF solo existe en la muestra)
comparacion_total <- rbind(cbind(modelo = "GMM", m_gmm_cc), cbind(modelo = "Isolation Forest", m_if_cc))
print(comparacion_total)
write.csv(comparacion_total, "output/tablas/B6_comparacion_datos_completos.csv", row.names = FALSE)

mejor <- comparacion_cc$modelo[1]
registrar("B6", "Comparación de modelos (creditcard)",
          paste0("Misma muestra de 40 000 · mejor AUC-PR: ", mejor, " (",
                 comparacion_cc$AUC_PR[1], ")"),
          "AUC-PR es la métrica más honesta con clases tan desbalanceadas (0,17 % de fraude)")

colores_mod <- c(GMM = col("magenta"), LOF = col("naranja"), `Isolation Forest` = col("azul"))

# ---------- 6.3 Gráfico: curvas ROC ----------
curvas <- do.call(rbind, lapply(names(puntajes), function(nm) {
  r <- pROC::roc(cc_lof$Class, puntajes[[nm]], quiet = TRUE)
  d <- data.frame(modelo = nm, fpr = 1 - r$specificities, tpr = r$sensitivities)
  d <- d[order(d$fpr, d$tpr), ]
  d[unique(round(seq(1, nrow(d), length.out = 400))), ]
}))
g_roc <- plot_ly(curvas, x = ~fpr, y = ~tpr, color = ~modelo, colors = colores_mod,
                 type = "scatter", mode = "lines", line = list(width = 3),
                 hovertemplate = "%{fullData.name}<br>Falsas alarmas: %{x:.1%}<br>Fraudes detectados: %{y:.1%}<extra></extra>") |>
  add_lines(x = c(0, 1), y = c(0, 1), name = "Azar", inherit = FALSE,
            line = list(color = "#BFB8AD", dash = "dot")) |>
  layout(xaxis = list(title = "Tasa de falsas alarmas (FPR)", tickformat = ".0%"),
         yaxis = list(title = "Tasa de fraudes detectados (recall)", tickformat = ".0%"),
         legend = list(orientation = "h", y = -0.2),
         annotations = list(list(x = 0.65, y = 0.25, showarrow = FALSE, align = "left",
                                 text = paste0("<b>AUC-ROC</b><br>",
                                               paste0(comparacion_cc$modelo, ": ", comparacion_cc$AUC_ROC,
                                                      collapse = "<br>"))))) |>
  tema_aguayo("Curvas ROC · GMM vs LOF vs Isolation Forest",
              "Misma muestra de 40 000 transacciones (proporción natural de fraude)")
print(g_roc); guardar(g_roc, "B6_curvas_roc")

# ---------- 6.4 Gráfico: curva de captura (¿cuánto fraude atrapo revisando el x %?) ----------
captura <- do.call(rbind, lapply(names(puntajes), function(nm) {
  o <- order(puntajes[[nm]], decreasing = TRUE)
  cap <- cumsum(cc_lof$Class[o]) / sum(cc_lof$Class)
  rev_pct <- seq_along(o) / length(o)
  sel <- unique(c(which(rev_pct <= 0.10)[seq(1, sum(rev_pct <= 0.10), length.out = 300)]))
  data.frame(modelo = nm, revisado = rev_pct[sel], capturado = cap[sel])
}))
g_captura <- plot_ly(captura, x = ~revisado, y = ~capturado, color = ~modelo, colors = colores_mod,
                     type = "scatter", mode = "lines", line = list(width = 3, shape = "spline"),
                     hovertemplate = "%{fullData.name}<br>Revisando el %{x:.1%} más raro<br>se atrapa el %{y:.0%} del fraude<extra></extra>") |>
  layout(xaxis = list(title = "% de transacciones revisadas (las más anómalas primero)", tickformat = ".0%"),
         yaxis = list(title = "% del fraude total atrapado", tickformat = ".0%", range = c(0, 1.02)),
         legend = list(orientation = "h", y = -0.2),
         shapes = list(list(type = "line", x0 = 0.01, x1 = 0.01, y0 = 0, y1 = 1,
                            line = list(color = col("tinta"), dash = "dash"))),
         annotations = list(list(x = 0.01, y = 1.02, text = "<b>umbral 1 %</b>", showarrow = FALSE,
                                 xanchor = "left"))) |>
  tema_aguayo("Curva de captura: el valor práctico para un banco",
              "Si el equipo antifraude solo puede revisar unas pocas alertas, ¿qué modelo rinde más?")
print(g_captura); guardar(g_captura, "B6_curva_captura")

# ---------- 6.5 Gráfico: ¿qué fraudes detecta cada modelo? ----------
fr <- cc_lof[cc_lof$Class == 1, ]
q_gmm <- quantile(cc_lof$score_gmm, 0.99); q_lof <- quantile(cc_lof$lof, 0.99)
q_if  <- quantile(cc_lof$score_if, 0.99)
combo <- paste0(ifelse(fr$score_gmm > q_gmm, "GMM ", ""), ifelse(fr$lof > q_lof, "LOF ", ""),
                ifelse(fr$score_if > q_if, "IF", ""))
combo <- trimws(gsub(" ", " + ", trimws(combo)))
combo[combo == ""] <- "Ninguno"
tab_combo <- as.data.frame(table(combinacion = combo)) |> dplyr::arrange(Freq)
print(tab_combo)

g_combo <- plot_ly(tab_combo, x = ~Freq, y = ~reorder(combinacion, Freq), type = "bar", orientation = "h",
                   marker = list(color = ~ifelse(combinacion == "Ninguno", "#BFB8AD",
                                                 ifelse(grepl("\\+.*\\+", combinacion), col("magenta"), col("naranja"))),
                                 line = list(color = col("tinta"), width = 1)),
                   text = ~Freq, textposition = "outside",
                   hovertemplate = "%{y}<br>%{x} fraudes<extra></extra>") |>
  layout(xaxis = list(title = "Número de fraudes reales"), yaxis = list(title = "")) |>
  tema_aguayo("¿Qué modelos detectan cada fraude? (umbral 1 %)",
              paste0("Los ", nrow(fr), " fraudes de la muestra · magenta = detectado por los tres"))
print(g_combo); guardar(g_combo, "B6_coincidencia_fraudes")

# =====================================================
# PARTE B · ASFI BOLIVIA
# =====================================================

# ---------- 6.6 Isolation Forest en sistema y bancos ----------
asfi_modelo$score_if_sis <- if_puntaje(X_sis)
asfi_modelo$pct_if_sis   <- round(rank(-asfi_modelo$score_if_sis) / nrow(asfi_modelo) * 100, 1)
asfi_modelo$anom_if_sis  <- as.integer(asfi_modelo$pct_if_sis <= 5)

bancos$score_if <- if_puntaje(X_ban)
bancos$pct_if   <- round(rank(-bancos$score_if) / nrow(bancos) * 100, 1)
bancos$anom_if  <- as.integer(bancos$pct_if <= 5)

registrar("B6", "Isolation Forest (ASFI)",
          "Aplicado a nivel sistema (8 variables) y bancos (4 variables), umbral 5 %",
          "Mismo umbral y variables que GMM y LOF")

# ---------- 6.7 Consenso de modelos (sistema) ----------
asfi_modelo$votos <- asfi_modelo$anom_gmm_sis + asfi_modelo$anom_lof_sis + asfi_modelo$anom_if_sis
cat("Entidad-mes marcados por los 3 modelos:", sum(asfi_modelo$votos == 3),
    "| por al menos 2:", sum(asfi_modelo$votos >= 2), "\n")

consenso <- asfi_modelo |>
  dplyr::filter(votos >= 2) |>
  dplyr::arrange(dplyr::desc(votos), pct_gmm_sis) |>
  dplyr::select(entidad, tipo_entidad, FECHA, votos, var_depositos, costo_publico,
                pct_gmm_sis, pct_lof_sis, pct_if_sis)
print(head(consenso, 25))
write.csv(consenso, "output/tablas/B6_consenso_anomalias_asfi.csv", row.names = FALSE)

registrar("B6", "Consenso de modelos (ASFI)",
          paste0(sum(asfi_modelo$votos == 3), " entidad-mes con 3 votos y ",
                 sum(asfi_modelo$votos >= 2), " con al menos 2"),
          "Una anomalía confirmada por métodos distintos es más confiable")

# ---------- 6.8 Gráfico: concordancia GMM vs LOF (sistema) ----------
asfi_modelo$votos_txt <- factor(paste(asfi_modelo$votos, "voto(s)"),
                                levels = paste(0:3, "voto(s)"))
g_concord <- plot_ly(asfi_modelo, x = ~100 - pct_gmm_sis, y = ~100 - pct_lof_sis, color = ~votos_txt,
                     colors = c("#D7D0C4", col("amarillo"), col("naranja"), col("magenta")),
                     type = "scatter", mode = "markers",
                     marker = list(size = ~4 + 3 * votos, opacity = 0.8,
                                   line = list(color = ~ifelse(entidad == "BFS", col("tinta"), "rgba(0,0,0,0)"),
                                               width = 2)),
                     text = ~paste0("<b>", entidad, "</b> · ", tipo_entidad, " · ", format(FECHA, "%b %Y"),
                                    "<br>Rareza GMM: ", 100 - pct_gmm_sis,
                                    "<br>Rareza LOF: ", 100 - pct_lof_sis,
                                    "<br>Rareza IF: ", 100 - pct_if_sis,
                                    "<br>Votos: ", votos), hoverinfo = "text") |>
  layout(xaxis = list(title = "Rareza según GMM (percentil)"),
         yaxis = list(title = "Rareza según LOF (percentil)"),
         legend = list(orientation = "h", y = -0.15)) |>
  tema_aguayo("¿Coinciden los modelos? · sistema financiero boliviano",
              "Esquina superior derecha = raro para ambos · color = votos de GMM, LOF e IF · borde negro = Fassil")
print(g_concord); guardar(g_concord, "B6_concordancia_asfi")

# ---------- 6.9 Tabla y gráfico: ¿quién detectó a Fassil? ----------
fassil_sis <- asfi_modelo |> dplyr::filter(entidad == "BFS", FECHA >= as.Date("2022-10-01")) |>
  dplyr::select(FECHA, GMM = pct_gmm_sis, LOF = pct_lof_sis, `Isolation Forest` = pct_if_sis) |>
  dplyr::mutate(nivel = "Sistema (67 entidades)")
fassil_ban <- bancos |> dplyr::filter(entidad == "BFS", FECHA >= as.Date("2022-10-01")) |>
  dplyr::select(FECHA, GMM = pct_gmm, LOF = pct_lof, `Isolation Forest` = pct_if) |>
  dplyr::mutate(nivel = "Bancos (14 pares)")
fassil_res <- tidyr::pivot_longer(rbind(fassil_sis, fassil_ban), c(GMM, LOF, `Isolation Forest`),
                                  names_to = "modelo", values_to = "percentil")
print(tidyr::pivot_wider(fassil_res, names_from = modelo, values_from = percentil))
write.csv(fassil_res, "output/tablas/B6_fassil_por_modelo.csv", row.names = FALSE)

g_fassil_mod <- subplot(lapply(unique(fassil_res$nivel), function(nv) {
  d <- fassil_res[fassil_res$nivel == nv, ]
  plot_ly(d, x = ~FECHA, y = ~percentil, color = ~modelo, colors = colores_mod,
          type = "scatter", mode = "lines+markers", legendgroup = ~modelo,
          showlegend = (nv == unique(fassil_res$nivel)[1]),
          line = list(width = 3), marker = list(size = 10),
          hovertemplate = paste0(nv, "<br>%{fullData.name} · %{x|%b %Y}<br>Percentil: %{y}<extra></extra>")) |>
    layout(yaxis = list(autorange = "reversed", title = "Percentil (más abajo = más raro)", range = c(45, 0)),
           xaxis = list(title = nv),
           shapes = list(list(type = "rect", x0 = min(d$FECHA) - 10, x1 = max(d$FECHA) + 10, y0 = 0, y1 = 5,
                              fillcolor = col("rojo"), opacity = 0.12, line = list(width = 0))))
}), nrows = 1, shareY = TRUE, titleX = TRUE, margin = 0.04) |>
  layout(legend = list(orientation = "h", y = -0.2),
         annotations = list(list(x = 0.5, y = 0.98, xref = "paper", yref = "paper", showarrow = FALSE,
                                 text = "<b>Zona de anomalía (≤ 5 %)</b>", font = list(color = col("rojo"))))) |>
  tema_aguayo("¿Qué modelo detectó al Banco Fassil antes de su intervención?",
              "Evolución del percentil de rareza de Fassil, oct-2022 a mar-2023")
print(g_fassil_mod); guardar(g_fassil_mod, "B6_fassil_por_modelo")

# ---------- 6.10 Guardar todo ----------
saveRDS(cc, "data/cc_procesado.rds")
saveRDS(cc_lof, "data/cc_lof.rds")
saveRDS(asfi_modelo, "data/asfi_modelo.rds")
saveRDS(bancos, "data/bancos_modelo.rds")
saveRDS(list(comparacion_cc = comparacion_cc, comparacion_total = comparacion_total,
             tab_combo = tab_combo, consenso = consenso, fassil_res = fassil_res,
             efecto_k = efecto_k, sens_gmm = sens_gmm, sens_fassil = sens_fassil,
             m_gmm_cc = m_gmm_cc, m_lof_cc = m_lof_cc, m_if_cc = m_if_cc),
        "output/resultados.rds")
registrar("B6", "Guardado de resultados finales",
          "resultados.rds con tablas de comparación, consenso y caso Fassil",
          "Insumo para el dashboard y el informe PDF")


# =====================================================
# BLOQUE 7A · GRÁFICOS DEL DASHBOARD (tema oscuro + caso Fassil)
# =====================================================
library(htmltools)

# ---------- 7.1 Reunir los gráficos de los bloques 2 a 6 ----------
nombres_g <- c("g_desbalance", "g_actividad", "g_ceros", "g_amount", "g_hora", "g_fassil", "g_var",
               "g_bic_cc", "g_dens_cc", "g_conf_gmm", "g_esp_gmm", "g_bic_sis", "g_sis", "g_cal_gmm",
               "g_k", "g_conf_lof", "g_lof_dist", "g_esp_lof", "g_sens", "g_cal_lof",
               "g_roc", "g_captura", "g_combo", "g_concord", "g_fassil_mod")
faltan_g <- nombres_g[!sapply(nombres_g, exists)]
if (length(faltan_g) > 0) {
  stop("Faltan gráficos: ", paste(faltan_g, collapse = ", "),
       "\nCorre otra vez los bloques 0 a 6 en esta sesión.")
}
graficos <- mget(nombres_g)

# Gráfico de Fassil por modelo: cada punto al inicio de su mes
fr_mes <- fassil_res
fr_mes$mes <- lubridate::floor_date(fr_mes$FECHA, "month")
graficos$g_fassil_mod <- subplot(lapply(unique(fr_mes$nivel), function(nv) {
  d <- fr_mes[fr_mes$nivel == nv, ]
  plot_ly(d, x = ~mes, y = ~percentil, color = ~modelo, colors = colores_mod,
          type = "scatter", mode = "lines+markers", legendgroup = ~modelo,
          showlegend = (nv == unique(fr_mes$nivel)[1]),
          line = list(width = 3), marker = list(size = 10),
          hovertemplate = paste0(nv, "<br>%{fullData.name} · %{x|%b %Y}<br>Percentil: %{y}<extra></extra>")) |>
    layout(yaxis = list(autorange = "reversed", title = "Percentil (más abajo = más raro)"),
           xaxis = list(title = nv, tickformat = "%b %Y", dtick = "M1"),
           shapes = list(list(type = "rect", x0 = min(d$mes) - 12, x1 = max(d$mes) + 12, y0 = 0, y1 = 5,
                              fillcolor = col("rojo"), opacity = 0.18, line = list(width = 0))))
}), nrows = 1, shareY = TRUE, titleX = TRUE, margin = 0.05) |>
  layout(legend = list(orientation = "h", y = -0.25))

# Mapa de sensibilidad de LOF ordenado por k
sf <- sens_fassil
sf$k_txt <- factor(paste("k =", sf$k), levels = paste("k =", sort(unique(sf$k))))
sf$mes <- factor(format(sf$FECHA, "%m/%Y"), levels = format(sort(unique(sf$FECHA)), "%m/%Y"))
graficos$g_sens <- subplot(lapply(c("Sistema", "Bancos"), function(nv) {
  d <- sf[sf$nivel == nv, ]
  plot_ly(d, x = ~mes, y = ~k_txt, z = ~percentil, type = "heatmap", zmin = 0, zmax = 40,
          showscale = (nv == "Bancos"),
          colorscale = list(c(0, col("magenta")), c(0.125, col("naranja")),
                            c(0.3, col("amarillo")), c(1, col("crema"))),
          colorbar = list(title = "Percentil"),
          text = ~paste0(nv, " · ", mes, "<br>", k_txt, "<br>Percentil: ", percentil,
                         ifelse(percentil <= 5, "<br><b>⚠ ANOMALÍA</b>", "")), hoverinfo = "text") |>
    add_annotations(x = d$mes, y = d$k_txt, showarrow = FALSE,
                    text = ifelse(d$percentil <= 5, paste0("<b>", d$percentil, "</b>"), d$percentil),
                    font = list(size = 11, color = "#FFFFFF")) |>
    layout(xaxis = list(title = nv), yaxis = list(title = ""))
}), nrows = 1, titleX = TRUE, shareY = TRUE, margin = 0.04)

# ---------- 7.2 Tema oscuro para todos los gráficos ----------
# Se cambian los colores "Aguayo" por la paleta negro/rojo y se oscurece el fondo
mapa_col <- c("#C2185B" = "#E50914", "#D32F2F" = "#FF3B3B", "#F57C00" = "#FF7A1A",
              "#FBC02D" = "#FFC23D", "#2E7D32" = "#2FD28B", "#00838F" = "#3FA7FF",
              "#283593" = "#2F6BFF", "#6A1B9A" = "#A274FF", "#FBF6EC" = "#151515",
              "#1E1B18" = "#EDEDED", "#BFB8AD" = "#4A4A4A", "#D7D0C4" = "#3A3A3A",
              "#B2DFDB" = "#123A5C")
hex_rgb <- function(h) paste(strtoi(substring(h, c(2, 4, 6), c(3, 5, 7)), 16L), collapse = ",")

cambiar_colores <- function(x) {
  if (is.list(x)) { x[] <- lapply(x, cambiar_colores); return(x) }
  if (is.character(x)) {
    for (h in names(mapa_col)) {
      x <- gsub(h, mapa_col[[h]], x, ignore.case = TRUE, fixed = FALSE)
      x <- gsub(paste0("(rgba?)\\(\\s*", gsub(",", "\\\\s*,\\\\s*", hex_rgb(h)), "\\s*([,)])"),
                paste0("\\1(", hex_rgb(mapa_col[[h]]), "\\2"), x)
    }
  }
  x
}

oscurecer <- function(p, alto = 430) {
  p <- plotly_build(p)
  p$x$data <- cambiar_colores(p$x$data)
  L <- cambiar_colores(p$x$layout)
  L$title <- NULL                                    # el título va en la tarjeta
  L$paper_bgcolor <- "rgba(0,0,0,0)"; L$plot_bgcolor <- "rgba(0,0,0,0)"
  L$font <- list(family = "Inter, 'Segoe UI', sans-serif", color = "#CFCFCF", size = 12)
  L$margin <- list(t = 30, r = 20, b = 50, l = 60)
  L$hoverlabel <- list(bgcolor = "#0B0B0B", bordercolor = "#E50914",
                       font = list(color = "#FFFFFF", family = "Inter, 'Segoe UI', sans-serif"))
  for (ax in grep("^[xy]axis", names(L), value = TRUE)) {
    L[[ax]]$gridcolor <- "#232323"; L[[ax]]$zerolinecolor <- "#3A3A3A"; L[[ax]]$linecolor <- "#3A3A3A"
  }
  if (!is.null(L$legend)) {
    L$legend$bgcolor <- "rgba(0,0,0,0)"
    if (identical(L$legend$orientation, "h")) L$legend$y <- min(c(L$legend$y, -0.18), na.rm = TRUE) - 0.07
  }
  if (!is.null(L$yaxis2) && identical(L$yaxis2$overlaying, "y")) L$margin$r <- 70   # eje derecho visible
  p$x$data <- lapply(p$x$data, function(tr) {                    # sin textos encima de las barras
    if (identical(tr$type, "bar") && !is.null(tr$text) && is.null(tr$hovertemplate)) tr$textposition <- "none"
    tr
  })
  p$x$layout <- L
  p$x$config$displaylogo <- FALSE
  p$width <- NULL
  if (is.null(p$height)) p$height <- alto
  p
}
graficos <- lapply(graficos, oscurecer)
graficos$g_actividad$height <- 1000
graficos$g_cal_gmm$height <- 520
graficos$g_cal_lof$height <- 520

# ---------- 7.2b Gráficos extra del caso Fassil (azul) ----------
f_mar <- fassil_res[fassil_res$FECHA == as.Date("2023-03-28"), ]
pct <- function(m, nv) f_mar$percentil[f_mar$modelo == m & grepl(nv, f_mar$nivel)]
AZ <- "#4C8DFF"; AZ2 <- "#1F5BFF"; GR <- "#5A5A5A"; RJ <- "#FF3B3B"
eventos <- list(
  list(type = "line", x0 = as.Date("2023-03-13"), x1 = as.Date("2023-03-13"), y0 = 0, y1 = 1, yref = "paper",
       line = list(color = RJ, dash = "dot", width = 1.5)),
  list(type = "line", x0 = as.Date("2023-04-26"), x1 = as.Date("2023-04-26"), y0 = 0, y1 = 1, yref = "paper",
       line = list(color = RJ, width = 2)))
notas_ev <- list(
  list(x = as.Date("2023-03-13"), y = 1.04, yref = "paper", text = "Corrida", showarrow = FALSE,
       font = list(color = RJ, size = 11), xanchor = "right"),
  list(x = as.Date("2023-04-26"), y = 1.04, yref = "paper", text = "Intervención", showarrow = FALSE,
       font = list(color = RJ, size = 11), xanchor = "left"))

bancos_tot <- asfi |>
  dplyr::filter(tipo_entidad == "Banco", CD_210_00 > 0) |>
  dplyr::select(entidad, FECHA, CD_210_00)

# (a) Depósitos indexados: ene-2022 = 100
base22 <- bancos_tot |> dplyr::filter(FECHA == as.Date("2022-01-28")) |>
  dplyr::select(entidad, base = CD_210_00)
idx <- bancos_tot |> dplyr::filter(FECHA >= as.Date("2022-01-01"), FECHA <= as.Date("2023-04-30")) |>
  dplyr::inner_join(base22, by = "entidad") |>
  dplyr::mutate(indice = CD_210_00 / base * 100)
med_idx <- idx |> dplyr::filter(entidad != "BFS") |> dplyr::group_by(FECHA) |>
  dplyr::summarise(indice = median(indice), .groups = "drop")
g_f_indice <- plot_ly()
for (e in setdiff(unique(idx$entidad), "BFS")) {
  d <- idx[idx$entidad == e, ]
  g_f_indice <- g_f_indice |> add_lines(data = d, x = ~FECHA, y = ~indice, name = e, showlegend = FALSE,
                                        line = list(color = GR, width = 1), opacity = 0.6,
                                        hovertemplate = paste0(e, " · %{x|%b %Y}<br>Índice: %{y:.1f}<extra></extra>"))
}
g_f_indice <- g_f_indice |>
  add_lines(data = med_idx, x = ~FECHA, y = ~indice, name = "Mediana bancos",
            line = list(color = "#FFFFFF", width = 2, dash = "dash"),
            hovertemplate = "Mediana · %{x|%b %Y}<br>Índice: %{y:.1f}<extra></extra>") |>
  add_lines(data = idx[idx$entidad == "BFS", ], x = ~FECHA, y = ~indice, name = "Fassil",
            line = list(color = AZ, width = 4),
            hovertemplate = "<b>Fassil</b> · %{x|%b %Y}<br>Índice: %{y:.1f}<extra></extra>") |>
  layout(xaxis = list(title = ""), yaxis = list(title = "Depósitos (ene-2022 = 100)"),
         shapes = eventos, annotations = notas_ev, legend = list(orientation = "h", y = -0.15))

# (b) Participación de Fassil en los depósitos de la banca
part <- bancos_tot |> dplyr::group_by(FECHA) |>
  dplyr::summarise(total = sum(CD_210_00), fassil = sum(CD_210_00[entidad == "BFS"]), .groups = "drop") |>
  dplyr::mutate(part = fassil / total * 100) |> dplyr::filter(part > 0)
g_f_part <- plot_ly(part, x = ~FECHA, y = ~part, type = "scatter", mode = "lines",
                    fill = "tozeroy", fillcolor = "rgba(76,141,255,0.25)", line = list(color = AZ, width = 3),
                    hovertemplate = "%{x|%b %Y}<br>Fassil: %{y:.2f} % de los depósitos de la banca<extra></extra>") |>
  layout(xaxis = list(title = ""), yaxis = list(title = "% de los depósitos bancarios", rangemode = "tozero"),
         shapes = eventos, annotations = notas_ev)

# (c) Variación mensual: Fassil vs mediana de bancos
var_cmp <- bancos |> dplyr::filter(FECHA >= as.Date("2022-07-01")) |>
  dplyr::group_by(FECHA) |>
  dplyr::summarise(Fassil = var_depositos[entidad == "BFS"][1],
                   `Mediana bancos` = median(var_depositos[entidad != "BFS"]), .groups = "drop") |>
  dplyr::filter(!is.na(Fassil)) |>
  dplyr::mutate(mes = factor(format(FECHA, "%m/%Y"), levels = format(sort(unique(FECHA)), "%m/%Y")))
g_f_var <- plot_ly(var_cmp, x = ~mes) |>
  add_bars(y = ~`Mediana bancos`, name = "Mediana bancos", marker = list(color = GR),
           hovertemplate = "%{x}<br>Mediana: %{y:.2f} %<extra></extra>") |>
  add_bars(y = ~Fassil, name = "Fassil", marker = list(color = ~ifelse(Fassil < -2, RJ, AZ)),
           hovertemplate = "%{x}<br>Fassil: %{y:.2f} %<extra></extra>") |>
  layout(barmode = "group", xaxis = list(title = ""), yaxis = list(title = "Variación mensual de depósitos (%)"),
         legend = list(orientation = "h", y = -0.15))

# (d) Costo de las obligaciones con el público: Fassil vs rango de los bancos
costo_b <- bancos |> dplyr::filter(entidad != "BFS", FECHA <= as.Date("2023-04-30")) |> dplyr::group_by(FECHA) |>
  dplyr::summarise(q1 = quantile(costo_publico, .25), med = median(costo_publico),
                   q3 = quantile(costo_publico, .75), .groups = "drop")
costo_f <- bancos |> dplyr::filter(entidad == "BFS")
g_f_costo <- plot_ly(costo_b, x = ~FECHA) |>
  add_ribbons(ymin = ~q1, ymax = ~q3, name = "50 % central de los bancos",
              fillcolor = "rgba(160,160,160,0.18)", line = list(width = 0), hoverinfo = "skip") |>
  add_lines(y = ~med, name = "Mediana bancos", line = list(color = "#FFFFFF", dash = "dash", width = 1.5),
            hovertemplate = "%{x|%b %Y}<br>Mediana: %{y:.2f} %<extra></extra>") |>
  add_lines(data = costo_f, x = ~FECHA, y = ~costo_publico, name = "Fassil", line = list(color = AZ, width = 3),
            hovertemplate = "<b>Fassil</b> · %{x|%b %Y}<br>Costo: %{y:.2f} % anual<extra></extra>") |>
  layout(xaxis = list(title = ""), yaxis = list(title = "Costo anual de obligaciones con el público (%)"),
         shapes = eventos, annotations = notas_ev, legend = list(orientation = "h", y = -0.15))

# (e) Radar: ¿en qué variables era raro Fassil? (percentil de cada variable entre todos los banco-mes)
vars_radar <- c(var_depositos = "Variación depósitos", costo_publico = "Costo con el público",
                var_oblig_bancos = "Variación oblig. con bancos", costo_bancos = "Costo con bancos",
                ratio_cxp = "Otras cuentas por pagar")
pct_var <- sapply(names(vars_radar), function(v) abs(rank(bancos[[v]]) / nrow(bancos) * 100 - 50) * 2)
meses_radar <- as.Date(c("2022-10-28", "2023-01-28", "2023-03-28"))
cols_radar <- c("#2A3F6E", "#2F6BFF", "#7FB2FF")
g_f_radar <- plot_ly(type = "scatterpolar", mode = "lines+markers")
for (j in seq_along(meses_radar)) {
  fila <- which(bancos$entidad == "BFS" & bancos$FECHA == meses_radar[j])
  r <- round(pct_var[fila, ], 1)
  g_f_radar <- g_f_radar |>
    add_trace(r = c(r, r[1]), theta = c(vars_radar, vars_radar[1]), name = format(meses_radar[j], "%m/%Y"),
              fill = "toself", fillcolor = paste0(cols_radar[j], "33"),
              line = list(color = cols_radar[j], width = 2), marker = list(color = cols_radar[j], size = 7),
              hovertemplate = "%{theta}<br>Rareza: %{r} / 100<extra>%{fullData.name}</extra>")
}
g_f_radar <- g_f_radar |>
  layout(polar = list(bgcolor = "rgba(0,0,0,0)",
                      radialaxis = list(range = c(0, 100), gridcolor = "#2A2A2A", tickfont = list(size = 9)),
                      angularaxis = list(gridcolor = "#2A2A2A")),
         legend = list(orientation = "h", y = -0.1))

# (f) Velocímetros: rareza de Fassil en marzo de 2023 entre los bancos (100 = lo más raro)
rz <- c(GMM = 100 - pct("GMM", "Bancos"), LOF = 100 - pct("LOF", "Bancos"),
        `Isolation Forest` = 100 - pct("Isolation Forest", "Bancos"))
g_f_gauge <- plot_ly()
for (j in seq_along(rz)) {
  g_f_gauge <- g_f_gauge |>
    add_trace(type = "indicator", mode = "gauge+number", value = unname(rz[j]),
              title = list(text = names(rz)[j], font = list(size = 15)),
              number = list(suffix = "", font = list(size = 30)),
              domain = list(x = c((j - 1) / 3 + 0.02, j / 3 - 0.02), y = c(0, 1)),
              gauge = list(axis = list(range = c(0, 100), tickcolor = "#666"),
                           bar = list(color = ifelse(rz[j] >= 95, RJ, AZ), thickness = 0.3),
                           bgcolor = "#0B1430", borderwidth = 0,
                           steps = list(list(range = c(0, 95), color = "#0E1A3A"),
                                        list(range = c(95, 100), color = "#4A0D12")),
                           threshold = list(line = list(color = RJ, width = 3), thickness = 0.85, value = 95)))
}

extra <- list(g_f_indice = g_f_indice, g_f_part = g_f_part, g_f_var = g_f_var,
              g_f_costo = g_f_costo, g_f_radar = g_f_radar, g_f_gauge = g_f_gauge)
graficos <- c(graficos, lapply(extra, oscurecer))
graficos$g_f_gauge$height <- 300
saveRDS(graficos, "output/graficos_dashboard.rds")
cat("Gráficos listos:", length(graficos), "\n")


# =====================================================
# BLOQUE 7B · DISEÑO, ANIMACIONES Y GUARDADO DEL DASHBOARD
# =====================================================

# ---------- 7.3 Cifras clave (calculadas) ----------
fmt <- function(x, d = 0) formatC(x, format = "f", digits = d, big.mark = " ", decimal.mark = ",")

# mini-curva (sparkline) de depósitos de Fassil para su tarjeta
fs <- fassil$dep; fs[is.na(fs)] <- 0
px <- round(seq(0, 200, length.out = length(fs)), 1)
py <- round(56 - fs / max(fs) * 50, 1)
spark <- paste0("M", paste(px, py, sep = ",", collapse = " L"))

# ---------- 7.4 Tarjetas animadas del inicio ----------
kpi <- function(clase, frente, reverso) {
  div(class = paste("kpi", clase), tabindex = "0",
      div(class = "kpi-in",
          div(class = "kpi-cara kpi-frente", frente),
          div(class = "kpi-cara kpi-reverso", reverso)))
}
contador <- function(valor, dec = 0, sufijo = "") {
  span(class = "contar", `data-fin` = valor, `data-dec` = dec, `data-suf` = sufijo, "0")
}

kpis <- div(class = "kpis",
  kpi("k-trans",
      tagList(div(class = "kpi-icono flujo", lapply(1:14, function(i) span(style = paste0("--i:", i)))),
              div(class = "kpi-num", contador(nrow(cc))), div(class = "kpi-lab", "transacciones analizadas"),
              div(class = "kpi-tip", "toca para ver por qué ↻")),
      tagList(h4("¿Por qué importa?"), p("Con tantas transacciones es imposible revisarlas a mano: ",
                                        "un modelo que señale solo el 1 % más raro ahorra casi todo el trabajo de revisión."))),
  kpi("k-fraude",
      tagList(div(class = "kpi-icono radar", span(), span(), span(class = "punto")),
              div(class = "kpi-num", contador(sum(cc$Class))), div(class = "kpi-lab", "fraudes reales"),
              div(class = "kpi-sub", paste0(fmt(mean(cc$Class) * 100, 3), " % del total"))),
      tagList(h4("¿Por qué importa?"), p("El fraude es rarísimo: un modelo que dijera siempre 'normal' acertaría el 99,8 % ",
                                        "y no serviría de nada. Por eso se busca como anomalía y no como una clase más."))),
  kpi("k-gmm",
      tagList(div(class = "anillo", `data-pct` = round(m_gmm_cc$recall * 100, 1),
                  HTML('<svg viewBox="0 0 120 120"><circle class="fondo" cx="60" cy="60" r="50"/>
                        <circle class="barra" cx="60" cy="60" r="50"/></svg>'),
                  div(class = "anillo-txt", contador(round(m_gmm_cc$recall * 100, 1), 1, " %"))),
              div(class = "kpi-lab", "fraudes que atrapa el GMM"),
              div(class = "kpi-sub", "revisando solo el 1 % más raro")),
      tagList(h4("¿Por qué importa?"), p("Demuestra que el GMM encuentra fraude sin etiquetas. Si funciona donde hay ",
                                        "respuesta conocida, se puede confiar en él donde no la hay, como en los datos de ASFI."))),
  kpi("k-ent",
      tagList(div(class = "circuito", {
                # 67 cuadraditos (uno por entidad) en una rejilla de 17 columnas;
                # el retardo depende del ángulo respecto al centro -> la luz gira como un radar
                n_ent <- length(unique(asfi_modelo$entidad))
                fil <- (seq_len(n_ent) - 1) %/% 17; colu <- (seq_len(n_ent) - 1) %% 17
                ang <- (atan2(fil - 1.5, (colu - 8) / 2.5) * 180 / pi + 360) %% 360
                lapply(seq_len(n_ent), function(i)
                  span(style = paste0("animation-delay:", round(ang[i] / 360 * 2.4 - 2.4, 3), "s")))
              }),
              div(class = "kpi-num", contador(length(unique(asfi_modelo$entidad)))),
              div(class = "kpi-lab", "entidades financieras de Bolivia"), div(class = "kpi-sub", "ASFI · 2020-2023")),
      tagList(h4("¿Por qué importa?"), p("ASFI debe vigilar a todas estas entidades cada mes. Un sistema automático de ",
                                        "alertas permite fijarse primero en las que se comportan raro."))),
  kpi("k-fassil",
      tagList(HTML(paste0('<svg class="spark" viewBox="0 0 200 60"><path d="', spark, '"/></svg>')),
              div(class = "kpi-num", contador(pct("GMM", "Bancos"), 1), span(class = "sep", " · "),
                  contador(pct("LOF", "Bancos"), 1)),
              div(class = "kpi-lab", "percentil de Fassil · mar-2023"), div(class = "kpi-sub", "GMM · LOF (≤ 5 = anomalía)")),
      tagList(h4("¿Por qué importa?"), p("La crisis ya se veía en los datos un mes antes de la intervención. ",
                                        "Una alerta temprana así podría ayudar a proteger a los ahorristas.")))
)

# ---------- 7.5 Componentes ----------
tarjeta <- function(widget, titulo, descripcion = NULL, ancho = "") {
  div(class = paste("card", ancho),
      div(class = "card-tit", span(class = "punto-rojo"), titulo),
      div(class = "card-cuerpo", widget),
      if (!is.null(descripcion)) div(class = "desc", HTML(descripcion)))
}
tabla_dt <- function(df, n = 10) {
  DT::datatable(df, rownames = FALSE, class = "compact",
                options = list(pageLength = n, scrollX = TRUE, dom = "ftip",
                               language = list(search = "Buscar:", info = "_START_ a _END_ de _TOTAL_",
                                               paginate = list(previous = "‹", `next` = "›"))))
}
seccion <- function(id, numero, titulo, intro, ..., clase = "") {
  div(class = paste("seccion", clase), id = id,
      div(class = "sec-cab", span(class = "sec-num", numero), h2(titulo)),
      p(class = "sec-intro", HTML(intro)),
      div(class = "grilla", ...))
}
mejor_pr <- comparacion_cc[1, ]
G <- graficos
bitacora <- bitacora[!grepl("^Dashboard interactivo", bitacora$accion), ]   # se registra al final

# ---------- 7.6 Secciones ----------
secciones <- list(
  seccion("inicio", "01", "Inicio",
          paste0("Detección de anomalías con <b>Mezclas Gaussianas (GMM)</b> y <b>Local Outlier Factor (LOF)</b>: ",
                 "primero se <b>validan</b> con fraude en tarjetas y luego se <b>aplican</b> al sistema financiero ",
                 "boliviano. Pasa el mouse sobre las tarjetas y tócalas para ver por qué importan."),
          kpis,
          tarjeta(G$g_fassil, "La anomalía real: Banco Fassil",
                  paste0("Depósitos del público de Fassil (millones de Bs) y su variación mensual. En marzo de 2023 cayeron ",
                         fmt(abs(fassil$var[fassil$FECHA == as.Date("2023-03-28")]), 1),
                         " %; el 26 de abril ASFI lo intervino y desde ahí sus cuentas quedan en cero."), "completo")),

  seccion("calidad", "02", "Calidad de datos",
          paste0("Duplicados eliminados: <b>", fmt(n_dup), "</b> · registros ASFI inactivos retirados: <b>",
                 sum(!asfi$activo), "</b> · cuentas con más de 40 % de ceros descartadas."),
          tarjeta(G$g_desbalance, "Desbalance de clases",
                  "Proporción de transacciones normales y fraudulentas. El fraude es menos del 0,2 %, por eso se trata como anomalía."),
          tarjeta(G$g_ceros, "Utilidad de las cuentas ASFI",
                  "Porcentaje de meses en cero por cuenta contable. Las verdes (≤ 40 %) se usan en los modelos."),
          tarjeta(G$g_actividad, "Actividad mensual de las 69 entidades",
                  "Cada celda es una entidad en un mes. En rojo: meses sin datos (entidad cerrada o fusionada, como Fassil desde abril de 2023).",
                  "completo")),

  seccion("exploracion", "03", "Exploración",
          "Transformaciones aplicadas antes de modelar y primeras señales de comportamiento raro.",
          tarjeta(G$g_amount, "Transformación logarítmica del monto",
                  "Izquierda: monto original, muy sesgado. Derecha: log(1 + monto), más simétrico y cercano a la forma gaussiana que supone el GMM."),
          tarjeta(G$g_hora, "Fraude por hora",
                  "Barras: transacciones por hora. Línea: fraudes por cada 1000. En las horas de poca actividad la proporción de fraude se dispara."),
          tarjeta(G$g_var, "Variación de depósitos por tipo de entidad",
                  "Cajas de la variación mensual de depósitos. Las estrellas son Fassil entre oct-2022 y mar-2023.",
                  "completo")),

  seccion("gmm", "04", "Mezclas Gaussianas (GMM)",
          paste0("El GMM describe los datos como una mezcla de distribuciones normales; una observación es anómala si cae en una ",
                 "zona de <b>baja densidad</b>. Modelo elegido: <b>", gmm_cc$modelName, " con ", gmm_cc$G, " componentes</b>."),
          tarjeta(G$g_bic_cc, "Selección del modelo por BIC",
                  "BIC de cada tipo de covarianza según el número de componentes. Se elige el más alto."),
          tarjeta(G$g_dens_cc, "Densidad de normales vs fraudes",
                  paste0("Distribución de la log-densidad. Los fraudes se concentran a la izquierda: con el umbral del 1 % se detectan ",
                         m_gmm_cc$TP, " de ", sum(cc$Class), ".")),
          tarjeta(G$g_conf_gmm, "Matriz de confusión · GMM",
                  paste0("Precisión ", fmt(m_gmm_cc$precision * 100, 1), " % · recall ", fmt(m_gmm_cc$recall * 100, 1),
                         " % · AUC-ROC ", fmt(m_gmm_cc$AUC_ROC, 3), ".")),
          tarjeta(G$g_esp_gmm, "Resultado en el plano V14 – V17",
                  "Fraudes detectados, no detectados y falsas alarmas sobre dos de las variables más informativas."),
          tarjeta(G$g_sis, "GMM en el sistema financiero boliviano",
                  "Cada punto es una entidad en un mes; color = rareza, puntos grandes = 5 % menos denso, borde = Fassil."),
          tarjeta(G$g_bic_sis, "BIC · sistema financiero",
                  paste0("Modelo elegido para ASFI: ", gmm_sis$modelName, " con ", gmm_sis$G, " componentes.")),
          tarjeta(G$g_cal_gmm, "Calendario de rareza GMM · 14 bancos",
                  paste0("Cada celda es un banco en un mes; más intenso = más raro. Fassil en marzo de 2023: percentil ",
                         fmt(pct("GMM", "Bancos"), 1), " (anomalía)."), "completo")),

  seccion("lof", "05", "Local Outlier Factor (LOF)",
          "LOF compara la densidad de cada punto con la de sus <b>k vecinos</b>: LOF ≈ 1 es normal, LOF > 1 es más aislado que su vecindario.",
          tarjeta(G$g_k, "Efecto del número de vecinos k",
                  paste0("AUC-ROC de LOF según k. Con k pequeño falla (los fraudes forman grupitos); con k = ", k_cc,
                         " llega a ", fmt(m_lof_cc$AUC_ROC, 3), ".")),
          tarjeta(G$g_lof_dist, "Puntaje LOF por clase",
                  "Distribución del puntaje LOF (escala log). La línea punteada es el umbral del 1 %."),
          tarjeta(G$g_conf_lof, "Matriz de confusión · LOF",
                  paste0("Muestra de 40 000: detecta ", m_lof_cc$TP, " de ", m_lof_cc$TP + m_lof_cc$FN, " fraudes.")),
          tarjeta(G$g_esp_lof, "LOF en el plano V14 – V17",
                  "Tamaño y color = puntaje LOF; las ✕ son fraudes reales."),
          tarjeta(G$g_sens, "¿Con qué k detecta LOF a Fassil?",
                  "Percentil de Fassil por mes y por k. En negrita, los casos marcados como anomalía (≤ 5).", "completo"),
          tarjeta(G$g_cal_lof, "Calendario de rareza LOF · 14 bancos",
                  paste0("Igual que el calendario GMM, pero con LOF (k = ", k_ban, "). Fassil en marzo de 2023: percentil ",
                         fmt(pct("LOF", "Bancos"), 1), "."), "completo")),

  seccion("comparacion", "06", "Comparación de modelos",
          paste0("Los tres métodos se evalúan en la <b>misma muestra</b> de 40 000 transacciones. Mejor AUC-PR: <b>",
                 mejor_pr$modelo, " (", fmt(mejor_pr$AUC_PR, 3), ")</b>."),
          tarjeta(G$g_roc, "Curvas ROC",
                  "Fraudes detectados vs falsas alarmas para cada modelo. Cuanto más arriba a la izquierda, mejor."),
          tarjeta(G$g_captura, "Curva de captura",
                  "Qué porcentaje del fraude se atrapa revisando solo el x % de transacciones más raras."),
          tarjeta(G$g_combo, "¿Qué modelo detecta cada fraude?",
                  "Cantidad de fraudes detectados por cada combinación de modelos (umbral 1 %)."),
          tarjeta(G$g_concord, "Concordancia GMM vs LOF · ASFI",
                  "Rareza GMM vs rareza LOF por entidad-mes; color = cuántos modelos lo marcan como anómalo."),
          tarjeta(tabla_dt(comparacion_cc), "Tabla de métricas",
                  "AUC-ROC, AUC-PR, precisión, recall y F1 de cada modelo.", "completo")),

  seccion("fassil", "07", "Caso Banco Fassil",
          paste0("Los métodos de <b>densidad</b> (GMM y LOF) marcan a Fassil como anómalo en marzo de 2023, antes de su ",
                 "intervención. Isolation Forest no (percentil ", fmt(pct("Isolation Forest", "Bancos"), 1),
                 "): Fassil no era un extremo global, sino raro frente a sus pares."),
          tarjeta(G$g_f_gauge, "Rareza de Fassil en marzo de 2023 (entre bancos)",
                  "Velocímetro de rareza (100 = lo más raro). La zona roja (≥ 95) es anomalía: GMM y LOF entran en ella, Isolation Forest no.",
                  "completo"),
          tarjeta(G$g_f_indice, "Depósitos de Fassil vs los demás bancos",
                  "Depósitos con base enero 2022 = 100, hasta abril de 2023. Línea azul: Fassil; gris: cada banco; blanca: mediana. Fassil se despega hacia abajo desde fines de 2022."),
          tarjeta(G$g_f_part, "Peso de Fassil en la banca",
                  "Porcentaje de los depósitos bancarios que tenía Fassil. Cae antes de la intervención y desaparece después."),
          tarjeta(G$g_f_var, "Variación mensual: Fassil vs la mediana",
                  "Barras grises: banco típico; azules: Fassil (rojas si cae más de 2 %). Marzo de 2023 es la caída más fuerte."),
          tarjeta(G$g_f_costo, "Costo de los depósitos de Fassil",
                  "Costo anual de sus obligaciones con el público frente al 50 % central de los bancos (franja gris)."),
          tarjeta(G$g_f_radar, "¿En qué era raro Fassil?",
                  "Rareza de cada variable de Fassil frente a todos los banco-mes (0 = típico, 100 = extremo). En marzo de 2023 la variación de depósitos se dispara al extremo.",
                  "completo"),
          tarjeta(G$g_fassil_mod, "Percentil de rareza de Fassil por modelo",
                  "Más abajo = más raro. La franja roja es la zona de anomalía (≤ 5 %). Izquierda: todo el sistema; derecha: solo bancos.",
                  "completo"),
          tarjeta(G$g_fassil, "Depósitos de Fassil 2020-2023",
                  "Línea: depósitos en millones de Bs. Barras: variación mensual. Franja: corrida de depósitos (13-mar a 26-abr-2023).",
                  "completo"),
          tarjeta(tabla_dt(consenso), "Otras anomalías del sistema (≥ 2 votos)",
                  "Entidad-mes marcados por al menos dos de los tres modelos. Ojo: desde mayo de 2023 la cuenta 210 cae ~40 % en casi todos los bancos a la vez, lo que apunta a un cambio de registro contable y no a retiros reales; las alertas de ese periodo deben tomarse con cautela.",
                  "completo"),
          clase = "tema-azul"),

  seccion("bitacora", "08", "Bitácora",
          paste0("Registro de las <b>", nrow(bitacora) + 1, "</b> decisiones del preprocesamiento y el modelado."),
          tarjeta(tabla_dt(bitacora[, c("n", "bloque", "accion", "decision", "justificacion",
                                        "filas_antes", "filas_despues")], 15),
                  "Bitácora del proyecto", NULL, "completo"))
)

# ---------- 7.7 Estilo ----------
css <- "
:root{--rojo:#E50914;--rojo2:#FF3B3B;--negro:#070707;--carbon:#101010;--panel:#141414;--borde:#242424;
 --texto:#EDEDED;--suave:#9A9A9A;--azul:#1F5BFF;--azul2:#4C8DFF;--nav:270px}
*{box-sizing:border-box}
html{scroll-behavior:smooth}
body{margin:0;background:radial-gradient(1200px 600px at 80% -10%,rgba(229,9,20,.12),transparent 60%),var(--negro);
 color:var(--texto);font-family:Inter,'Segoe UI',system-ui,sans-serif;overflow-x:hidden}
#progreso{position:fixed;top:0;left:0;height:3px;width:0;background:linear-gradient(90deg,var(--rojo),#ff8a8a);z-index:50}
nav{position:fixed;top:0;left:0;bottom:0;width:var(--nav);background:linear-gradient(180deg,#0c0c0c,#060606);
 border-right:1px solid var(--borde);padding:22px 0;z-index:20;transition:width .45s cubic-bezier(.7,0,.2,1);overflow:hidden}
nav .marca{display:flex;align-items:center;gap:12px;padding:0 22px 22px;white-space:nowrap}
nav .logo{width:34px;height:34px;flex:none;border-radius:9px;background:var(--rojo);position:relative;
 box-shadow:0 0 22px rgba(229,9,20,.55);animation:latido 2.4s infinite}
nav .logo::after{content:'';position:absolute;inset:9px;border:2px solid #fff;border-radius:50%}
nav .marca b{font-size:18px;letter-spacing:.5px}
nav .marca small{display:block;color:var(--suave);font-size:11px}
#toggle{position:absolute;top:24px;right:-1px;width:30px;height:30px;border:1px solid var(--borde);border-right:0;
 background:#151515;color:#fff;border-radius:8px 0 0 8px;cursor:pointer;font-size:14px;transition:transform .45s}
nav a{position:relative;display:flex;align-items:center;gap:14px;margin:4px 12px;padding:12px 14px;border-radius:10px;
 color:#cfcfcf;text-decoration:none;cursor:pointer;white-space:nowrap;transition:background .25s,color .25s,transform .25s}
nav a .n{flex:none;width:28px;height:28px;border-radius:8px;display:grid;place-items:center;font-size:12px;
 background:#1a1a1a;border:1px solid #2a2a2a;transition:all .3s}
nav a:hover{background:#161616;color:#fff;transform:translateX(4px)}
nav a.activo{background:linear-gradient(90deg,rgba(229,9,20,.22),rgba(229,9,20,.02));color:#fff}
nav a.activo .n{background:var(--rojo);border-color:var(--rojo);box-shadow:0 0 16px rgba(229,9,20,.6)}
nav a.activo::before{content:'';position:absolute;left:-12px;top:10px;bottom:10px;width:3px;border-radius:3px;background:var(--rojo)}
nav .t{transition:opacity .3s}
nav .pie{position:absolute;bottom:18px;left:22px;right:22px;color:var(--suave);font-size:11px;white-space:nowrap;transition:opacity .3s}
body.cerrado{--nav:78px}
body.cerrado nav .t,body.cerrado nav .marca div,body.cerrado nav .pie{opacity:0;pointer-events:none}
body.cerrado #toggle{transform:rotate(180deg);top:72px}
main,header,footer{margin-left:var(--nav);transition:margin-left .45s cubic-bezier(.7,0,.2,1)}
header{padding:34px 44px 26px;border-bottom:1px solid var(--borde);position:relative;overflow:hidden}
header::after{content:'';position:absolute;right:-80px;top:-80px;width:320px;height:320px;border-radius:50%;
 border:1px solid rgba(229,9,20,.35);box-shadow:0 0 0 40px rgba(229,9,20,.04),0 0 0 80px rgba(229,9,20,.03);animation:giro 18s linear infinite}
header h1{margin:0;font-size:32px;font-weight:800;letter-spacing:-.5px}
header h1 span{color:var(--rojo)}
header .sub{color:var(--suave);margin-top:6px}
header .autor{margin-top:12px;font-size:12px;color:#fff;display:inline-block;padding:6px 12px;border:1px solid #2a2a2a;border-radius:20px;background:#111}
main{padding:30px 44px 60px}
.seccion{display:none}
.seccion.visible{display:block;animation:entra .55s cubic-bezier(.2,.8,.2,1)}
@keyframes entra{from{opacity:0;transform:translateY(18px)}to{opacity:1;transform:none}}
.sec-cab{display:flex;align-items:center;gap:16px}
.sec-num{font-size:14px;font-weight:700;color:#fff;background:var(--rojo);padding:6px 10px;border-radius:8px;box-shadow:0 0 20px rgba(229,9,20,.45)}
.sec-cab h2{margin:0;font-size:28px;font-weight:800}
.sec-intro{color:#bdbdbd;max-width:1050px;line-height:1.65;margin:14px 0 26px}
.sec-intro b{color:#fff}
.grilla{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:22px;align-items:start}
.completo{grid-column:1 / -1}
.card{background:linear-gradient(180deg,#151515,#0f0f0f);border:1px solid var(--borde);border-radius:16px;padding:18px 18px 14px;
 position:relative;overflow:hidden;opacity:0;transform:translateY(24px) scale(.98);
 transition:opacity .6s ease,transform .6s cubic-bezier(.2,.8,.2,1),border-color .3s,box-shadow .3s}
.card.visto{opacity:1;transform:none}
.card::before{content:'';position:absolute;left:0;top:0;height:3px;width:0;background:linear-gradient(90deg,var(--rojo),transparent);transition:width .8s .2s}
.card.visto::before{width:100%}
.card:hover{border-color:rgba(229,9,20,.55);box-shadow:0 18px 40px -22px rgba(229,9,20,.55)}
.card-tit{display:flex;align-items:center;gap:10px;font-weight:700;font-size:16px;margin-bottom:8px}
.punto-rojo{width:9px;height:9px;border-radius:50%;background:var(--rojo);box-shadow:0 0 0 0 rgba(229,9,20,.7);animation:ping 2s infinite}
.desc{margin-top:10px;padding:10px 12px;border-radius:10px;background:#0b0b0b;border:1px solid #1f1f1f;color:#b5b5b5;font-size:13px;line-height:1.55}
.desc::before{content:'Descripción · ';color:var(--rojo);font-weight:700}
/* --- tema azul para Fassil --- */
.tema-azul .sec-num{background:var(--azul);box-shadow:0 0 22px rgba(31,91,255,.6)}
.tema-azul .card{background:linear-gradient(180deg,#0b1430,#070b18);border-color:#16244d}
.tema-azul .card::before{background:linear-gradient(90deg,var(--azul2),transparent)}
.tema-azul .card:hover{border-color:var(--azul2);box-shadow:0 18px 40px -22px rgba(31,91,255,.8)}
.tema-azul .punto-rojo{background:var(--azul2);animation:pingazul 2s infinite}
.tema-azul .desc{background:#060a16;border-color:#16244d}
.tema-azul .desc::before{color:var(--azul2)}
/* --- KPIs animados --- */
.kpis{grid-column:1 / -1;display:grid;grid-template-columns:repeat(5,minmax(0,1fr));gap:18px;perspective:1200px}
.kpi{height:230px;cursor:pointer;outline:none;opacity:0;transform:translateY(30px) rotateX(-12deg)}
.kpi.visto{animation:sube .8s cubic-bezier(.2,.8,.2,1) forwards;animation-delay:calc(var(--d)*120ms)}
@keyframes sube{to{opacity:1;transform:none}}
.kpi-in{position:relative;width:100%;height:100%;transform-style:preserve-3d;transition:transform .7s cubic-bezier(.2,.8,.2,1)}
.kpi.girada .kpi-in{transform:rotateY(180deg)}
.kpi-cara{position:absolute;inset:0;border-radius:18px;padding:18px;backface-visibility:hidden;overflow:hidden;
 background:linear-gradient(160deg,#1a1a1a,#0c0c0c);border:1px solid #262626}
.kpi-frente::after{content:'';position:absolute;inset:0;background:radial-gradient(300px 140px at var(--mx,50%) var(--my,0%),rgba(229,9,20,.25),transparent 60%);pointer-events:none}
.kpi-reverso{transform:rotateY(180deg);background:linear-gradient(160deg,#2a0306,#0c0c0c);border-color:var(--rojo)}
.kpi-reverso h4{margin:0 0 8px;color:var(--rojo2)} .kpi-reverso p{margin:0;font-size:13px;line-height:1.55;color:#ddd}
.kpi-num{font-size:34px;font-weight:800;letter-spacing:-.5px;margin-top:10px}
.kpi-num .sep{color:var(--suave)}
.kpi-lab{font-size:13px;color:#cfcfcf;margin-top:4px}
.kpi-sub{font-size:11px;color:var(--suave);margin-top:4px}
.kpi-tip{position:absolute;bottom:12px;right:14px;font-size:10px;color:#666}
.kpi-icono{height:58px;display:flex;align-items:flex-end;gap:4px}
.flujo span{flex:1;background:linear-gradient(180deg,var(--rojo),#5a0005);border-radius:3px 3px 0 0;
 animation:barra 1.4s ease-in-out infinite alternate;animation-delay:calc(var(--i)*-110ms)}
@keyframes barra{from{height:15%}to{height:100%}}
.radar{position:relative;width:58px;height:58px;align-items:center;justify-content:center}
.radar span{position:absolute;inset:0;border:2px solid var(--rojo);border-radius:50%;animation:onda 2.2s ease-out infinite}
.radar span:nth-child(2){animation-delay:1.1s}
.radar .punto{inset:22px;background:var(--rojo);border:0;animation:latido 1.1s infinite}
@keyframes onda{from{transform:scale(.3);opacity:1}to{transform:scale(1.4);opacity:0}}
.rejilla{display:grid;grid-template-columns:repeat(17,1fr);gap:3px;height:auto;align-items:start}
.rejilla span{aspect-ratio:1;border-radius:2px;background:#2a2a2a;animation:enciende 3s infinite;animation-delay:calc(var(--i)*45ms)}
@keyframes enciende{0%,70%,100%{background:#2a2a2a}10%{background:var(--rojo);box-shadow:0 0 6px var(--rojo)}}
.anillo{position:relative;width:110px;height:110px;margin:0 auto}
.anillo svg{width:100%;height:100%;transform:rotate(-90deg)}
.anillo circle{fill:none;stroke-width:10}
.anillo .fondo{stroke:#262626}
.anillo .barra{stroke:var(--rojo);stroke-linecap:round;stroke-dasharray:314.16;stroke-dashoffset:314.16;
 transition:stroke-dashoffset 1.8s cubic-bezier(.2,.8,.2,1);filter:drop-shadow(0 0 6px rgba(229,9,20,.7))}
.anillo-txt{position:absolute;inset:0;display:grid;place-items:center;font-size:22px;font-weight:800}
.k-gmm .kpi-lab,.k-gmm .kpi-sub{text-align:center}
.spark{width:100%;height:62px}
.spark path{fill:none;stroke:var(--azul2);stroke-width:2.5;stroke-dasharray:600;stroke-dashoffset:600;
 filter:drop-shadow(0 0 5px rgba(76,141,255,.8))}
.kpi.visto .spark path{animation:dibuja 2.6s .6s ease forwards}
@keyframes dibuja{to{stroke-dashoffset:0}}
.k-fassil .kpi-frente{background:linear-gradient(160deg,#0d1a3d,#070b18);border-color:#1a2c5c}
.k-fassil .kpi-reverso{background:linear-gradient(160deg,#0d1f55,#070b18);border-color:var(--azul2)}
.k-fassil .kpi-reverso h4{color:var(--azul2)}

.circuito{display:grid;grid-template-columns:repeat(17,1fr);gap:4px;height:58px;align-content:center}
.circuito span{aspect-ratio:1;border-radius:2px;background:#2a2a2a;animation:chispa 2.4s linear infinite}
@keyframes chispa{0%{background:var(--rojo);box-shadow:0 0 8px var(--rojo);transform:scale(1.25)}
 20%{background:#8a0a12;box-shadow:none;transform:scale(1)}40%,100%{background:#2a2a2a;transform:scale(1)}}
@keyframes gira{to{transform:rotate(360deg)}}
.kpi.girada *{animation-play-state:paused!important}
.kpi .kpi-num{transition:filter .4s}
@keyframes latido{0%,100%{transform:scale(1)}50%{transform:scale(1.12)}}
@keyframes ping{0%{box-shadow:0 0 0 0 rgba(229,9,20,.7)}100%{box-shadow:0 0 0 10px rgba(229,9,20,0)}}
@keyframes pingazul{0%{box-shadow:0 0 0 0 rgba(76,141,255,.7)}100%{box-shadow:0 0 0 10px rgba(76,141,255,0)}}
@keyframes giro{to{transform:rotate(360deg)}}
footer{padding:20px 44px 40px;color:#777;font-size:12px;border-top:1px solid var(--borde)}
/* --- tablas oscuras --- */
table.dataTable{color:#ddd!important;background:transparent!important;font-size:12px}
table.dataTable thead th{color:#fff!important;border-bottom:1px solid #333!important;background:#111!important}
table.dataTable tbody tr{background:transparent!important}
table.dataTable tbody tr:hover{background:rgba(229,9,20,.12)!important}
table.dataTable tbody td{border-top:1px solid #1d1d1d!important}
.dataTables_wrapper .dataTables_filter input{background:#0b0b0b;color:#fff;border:1px solid #333;border-radius:8px;padding:5px 8px}
.dataTables_wrapper .dataTables_info,.dataTables_wrapper .dataTables_filter,.dataTables_wrapper .dataTables_paginate{color:#aaa!important}
.dataTables_wrapper .dataTables_paginate .paginate_button{color:#ddd!important;border-radius:6px}
.dataTables_wrapper .dataTables_paginate .paginate_button.current{background:var(--rojo)!important;color:#fff!important;border:0!important}
::-webkit-scrollbar{width:10px;height:10px}::-webkit-scrollbar-thumb{background:#2a2a2a;border-radius:8px}
::-webkit-scrollbar-track{background:#0a0a0a}
@media(max-width:1200px){.kpis{grid-template-columns:repeat(3,1fr)}}
@media(max-width:1000px){.grilla{grid-template-columns:1fr}.kpis{grid-template-columns:repeat(2,1fr)}}
"

js <- "
function redimensionar(raiz){
  (raiz||document).querySelectorAll('.js-plotly-plot').forEach(function(el){
    if (el.offsetParent !== null && window.Plotly) { try { Plotly.Plots.resize(el); } catch(e){} }
  });
  window.dispatchEvent(new Event('resize'));   // las tablas DT se dibujan al hacerse visibles
}
function formatear(v, dec){
  var s = Number(v).toFixed(dec).split('.');
  s[0] = s[0].replace(/\\B(?=(\\d{3})+(?!\\d))/g, ' ');
  return s.join(',');
}
function contar(el){
  if (el.dataset.hecho) return; el.dataset.hecho = 1;
  var fin = parseFloat(el.dataset.fin), dec = parseInt(el.dataset.dec || 0), suf = el.dataset.suf || '';
  var t0 = null, dur = 1800;
  function paso(t){ if(!t0) t0 = t; var p = Math.min((t - t0) / dur, 1), e = 1 - Math.pow(1 - p, 4);
    el.textContent = formatear(fin * e, dec) + suf; if (p < 1) requestAnimationFrame(paso); }
  requestAnimationFrame(paso);
}
function animarKpis(){
  document.querySelectorAll('.kpi').forEach(function(k, i){
    k.style.setProperty('--d', i); k.classList.add('visto');
    setTimeout(function(){ k.querySelectorAll('.contar').forEach(contar);
      k.querySelectorAll('.anillo').forEach(function(a){
        a.querySelector('.barra').style.strokeDashoffset = 314.16 * (1 - parseFloat(a.dataset.pct) / 100); });
    }, 250 + i * 120);
  });
}
var observador = new IntersectionObserver(function(ents){
  ents.forEach(function(e){ if (e.isIntersecting){ e.target.classList.add('visto'); observador.unobserve(e.target);
    setTimeout(function(){ redimensionar(e.target); }, 120); } });
}, {threshold: 0.08});
function mostrar(id, subir){
  document.querySelectorAll('.seccion').forEach(function(s){ s.classList.toggle('visible', s.id === id); });
  document.querySelectorAll('nav a').forEach(function(a){ a.classList.toggle('activo', a.dataset.sec === id); });
  var sec = document.getElementById(id);
  sec.querySelectorAll('.card').forEach(function(c, i){ c.style.transitionDelay = (Math.min(i, 6) * 70) + 'ms'; observador.observe(c); });
  if (id === 'inicio') animarKpis();
  setTimeout(function(){ redimensionar(sec); }, 60);
  setTimeout(function(){ redimensionar(sec); }, 600);
  if (subir) window.scrollTo({top: 0, behavior: 'smooth'});
  history.replaceState(null, '', '#' + id);
}
document.addEventListener('DOMContentLoaded', function(){
  document.querySelectorAll('nav a').forEach(function(a){ a.addEventListener('click', function(){ mostrar(a.dataset.sec, true); }); });
  document.getElementById('toggle').addEventListener('click', function(){
    document.body.classList.toggle('cerrado');
    setTimeout(function(){ redimensionar(document.querySelector('.seccion.visible')); }, 480);
  });
  document.querySelectorAll('.kpi').forEach(function(k){
    k.addEventListener('click', function(){ k.querySelector('.kpi-in').style.transform = ''; k.classList.toggle('girada'); });
    k.addEventListener('mousemove', function(ev){
      if (k.classList.contains('girada')) return;
      var r = k.getBoundingClientRect(), x = (ev.clientX - r.left) / r.width, y = (ev.clientY - r.top) / r.height;
      k.querySelector('.kpi-in').style.transform = 'rotateY(' + ((x - .5) * 16) + 'deg) rotateX(' + ((.5 - y) * 12) + 'deg) translateZ(10px)';
      k.querySelector('.kpi-frente').style.setProperty('--mx', (x * 100) + '%');
      k.querySelector('.kpi-frente').style.setProperty('--my', (y * 100) + '%');
    });
    k.addEventListener('mouseleave', function(){ if (!k.classList.contains('girada')) k.querySelector('.kpi-in').style.transform = ''; });
  });
  window.addEventListener('scroll', function(){
    var h = document.documentElement, p = h.scrollTop / Math.max(1, h.scrollHeight - h.clientHeight);
    document.getElementById('progreso').style.width = (p * 100) + '%';
  });
  var ini = location.hash.replace('#', '');
  mostrar(document.getElementById(ini) ? ini : 'inicio', false);
});
window.addEventListener('load', function(){ window.scrollTo(0, 0); redimensionar(document.querySelector('.seccion.visible')); });
"

menu <- list(c("inicio", "01", "Inicio"), c("calidad", "02", "Calidad de datos"),
             c("exploracion", "03", "Exploración"), c("gmm", "04", "GMM"), c("lof", "05", "LOF"),
             c("comparacion", "06", "Comparación"), c("fassil", "07", "Caso Fassil"),
             c("bitacora", "08", "Bitácora"))

pagina <- tagList(
  tags$head(tags$meta(charset = "utf-8"),
            tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
            tags$title("Detección de anomalías · GMM y LOF"),
            tags$link(rel = "stylesheet",
                      href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;600;700;800&display=swap"),
            tags$style(HTML(css)), tags$script(HTML(js))),
  div(id = "progreso"),
  tags$nav(tags$button(id = "toggle", title = "Abrir / cerrar menú", HTML("&#10094;")),
           div(class = "marca", span(class = "logo"), div(tags$b("Anomalías"), tags$small("GMM · LOF · Bolivia"))),
           lapply(menu, function(m) tags$a(`data-sec` = m[1], span(class = "n", m[2]), span(class = "t", m[3]))),
           div(class = "pie", "Aduviri Monasterios B. U. · UMSA")),
  tags$header(h1("Detección de anomalías con ", span("GMM"), " y ", span("LOF")),
              div(class = "sub", "Validación en fraude con tarjetas y aplicación al sistema financiero boliviano · caso Banco Fassil"),
              div(class = "autor", "Aduviri Monasterios Brandon Ulises · Carrera de Informática · UMSA · ",
                  format(Sys.Date(), "%d/%m/%Y"))),
  tags$main(secciones),
  tags$footer("Fuentes: Credit Card Fraud Detection (ULB / Worldline, 2013) · Estados financieros ASFI 2020-2023 ",
              "(compilación PRIMERO-LOS-DATOS) · R: mclust, dbscan, isotree, plotly, DT.")
)

# ---------- 7.8 Guardar como UN solo archivo HTML ----------
# Las librerías JS/CSS se incrustan en base64 (así no se rompen al copiarlas dentro del HTML)
carpeta <- file.path(getwd(), "dashboard")
tmp <- file.path(carpeta, "tmp_dashboard.html")
save_html(pagina, file = tmp, libdir = "lib", lang = "es")

a_base64 <- function(f) jsonlite::base64_enc(readBin(f, "raw", file.info(f)$size))
x <- readLines(tmp, encoding = "UTF-8", warn = FALSE)
for (i in seq_along(x)) {
  m <- regmatches(x[i], regexec('<script src="(lib/[^"]+)"></script>', x[i]))[[1]]
  if (length(m) == 2)
    x[i] <- paste0('<script src="data:text/javascript;base64,', a_base64(file.path(carpeta, m[2])), '"></script>')
  m <- regmatches(x[i], regexec('<link href="(lib/[^"]+)" rel="stylesheet" />', x[i]))[[1]]
  if (length(m) == 2)
    x[i] <- paste0('<link href="data:text/css;base64,', a_base64(file.path(carpeta, m[2])), '" rel="stylesheet" />')
}
salida <- file.path(carpeta, "Dashboard_GMM_LOF.html")
con <- file(salida, open = "w", encoding = "UTF-8"); writeLines(x, con); close(con)
unlink(tmp); unlink(file.path(carpeta, "lib"), recursive = TRUE)
cat("Scripts que quedaron sin incrustar:", sum(grepl('src="lib/', x)), "(debe ser 0)\n")
cat("Dashboard creado:", salida, "| Tamaño:", round(file.size(salida) / 1e6, 1), "MB\n")
browseURL(salida)

registrar("B7", "Dashboard interactivo",
          "Dashboard_GMM_LOF.html: negro/rojo, caso Fassil en azul (8 gráficos), 31 gráficos y 3 tablas",
          "Presentar los resultados de forma visual e interactiva en un solo archivo")
