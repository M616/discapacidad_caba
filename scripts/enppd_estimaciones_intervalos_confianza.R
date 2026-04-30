# Datos
p <- 0.012             # proporción central de ENPPD 2018
EE <- 0.0029           # error estándar de la proporción
pob_caba_2025 <- 3079674  # población total proyectada CABA 2025
prop_may6 <- 0.948         # proporción de población >=6 años

# Población >=6 años en CABA 2025
pob_may6 <- pob_caba_2025 * prop_may6

# Estimación central
N_central <- p * pob_may6

# IC 95% de la proporción (del estudio)
IC_proporcion <- c(p - 1.96 * EE, p + 1.96 * EE)

# Extrapolar IC a número de personas en CABA
IC_personas <- IC_proporcion * pob_may6

# Mostrar resultados
cat("Estimación central (personas):", round(N_central), "\n")
cat("IC 95% (personas):", round(IC_personas[1]), "-", round(IC_personas[2]), "\n")



######estimacion para quienes responden todos los apoyos en pregunta d14_n
# Datos
p <- 0.003             # proporción central de ENPPD 2018
EE <- 0.0006           # error estándar de la proporción
pob_caba_2025 <- 3079674  # población total proyectada CABA 2025
prop_may6 <- 0.948         # proporción de población >=6 años

# Población >=6 años en CABA 2025
pob_may6 <- pob_caba_2025 * prop_may6

# Estimación central
N_central <- p * pob_may6

# IC 95% de la proporción (del estudio)
IC_proporcion <- c(p - 1.96 * EE, p + 1.96 * EE)

# Extrapolar IC a número de personas en CABA
IC_personas <- IC_proporcion * pob_may6

# Mostrar resultados
cat("Estimación central (personas):", round(N_central), "\n")
cat("IC 95% (personas):", round(IC_personas[1]), "-", round(IC_personas[2]), "\n")
