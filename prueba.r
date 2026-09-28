# 1. Calculamos la PD_TRIM usando MARKOV (tu código original que funciona)
PORT_PD <- MARKOV[, .(
  PD_NUM = sum(PD * BAL_PERFORMING, na.rm = TRUE),
  BAL_PERFORMING = sum(BAL_PERFORMING, na.rm = TRUE),
  BAL_DEFAULT = sum(BAL_DEFAULT, na.rm = TRUE)
), keyby = .(DATASET, END_DATE, PORT_ID)]

PORT_PD[, PD_TRIM := PD_NUM / BAL_PERFORMING]
PORT_PD[BAL_PERFORMING == 0, PD_TRIM := 1]

# 2. Preparamos PORTFOLIO (Cruce y filtro de nulos para la LGD)
PORTFOLIO <- MARKOV[BU_LGD, on = .(DATASET, END_DATE, PORT_ID, ACC_ID)]
PORTFOLIO <- PORTFOLIO[!is.na(LGD), ]

# 3. Calculamos la LGD_TRIM usando únicamente PORTFOLIO
PORT_LGD <- PORTFOLIO[, .(
  LGD_TRIM = sum(LGD * (PD * BAL_PERFORMING), na.rm = TRUE) / sum(PD * BAL_PERFORMING, na.rm = TRUE)
), keyby = .(DATASET, END_DATE, PORT_ID)]

# 4. Unimos los resultados a nivel de cartera
# Esto te dará una tabla PORT final con la PD_TRIM correcta y la LGD_TRIM correcta
PORT_FINAL <- merge(PORT_PD, PORT_LGD, by = c("DATASET", "END_DATE", "PORT_ID"), all.x = TRUE)
