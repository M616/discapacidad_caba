# distribución de la variable
table(base$entrea_md, useNA = "ifany")

# prevalencia de discapacidad entre quienes respondieron el módulo
svymean(
  ~I(dd_con_dif == 1),
  subset(disenio, entrea_md == 1),
  na.rm = TRUE
)


svymean(
  ~I(entrea_md == 1),
  design = disenio,
  na.rm = TRUE
)
