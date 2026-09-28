# Agrupamos calculando los numeradores y denominadores con cuidado de los NA
PORT <- PORTFOLIO[, .(
  # 1. Componentes de la PD (Usan todos los registros, ignorando NAs de saldo si los hubiera)
  PD_NUM = sum(PD * BAL_PERFORMING, na.rm = TRUE),
  BAL_PERFORMING = sum(BAL_PERFORMING, na.rm = TRUE),
  BAL_DEFAULT = sum(BAL_DEFAULT, na.rm = TRUE),
  
  # 2. Componentes de la LGD (Requieren un manejo especial)
  # Numerador: Ignoramos la fila en la suma si la LGD es nula
  LGD_NUM = sum(LGD * PD * BAL_PERFORMING, na.rm = TRUE),
  
  # Denominador de la LGD: ¡Solo debe sumar el saldo ponderado de aquellos contratos que SÍ tienen LGD!
  LGD_DEN = sum((PD * BAL_PERFORMING)[!is.na(LGD)], na.rm = TRUE)
  
), keyby = .(DATASET, PORT_ID, END_DATE)]


# Calculamos los Ratios Finales (PD_TRIM y LGD_TRIM)
PORT[, PD_TRIM := PD_NUM / BAL_PERFORMING]
PORT[BAL_PERFORMING == 0, PD_TRIM := 1] # Corrección de división por cero

PORT[, LGD_TRIM := LGD_NUM / LGD_DEN]
# Si quieres asegurar que no falle por división por 0 en la LGD:
PORT[LGD_DEN == 0 | is.na(LGD_DEN), LGD_TRIM := 0]
