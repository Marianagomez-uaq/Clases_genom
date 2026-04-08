
# Paso 1: Crea un proyecto en R

# Por alguna razón, no reconoce los proyectos que se creen en la memoria (D:), tienen que crearse directamente en la computadora (C:).

# Paso 2: Inicializa git
usethis::use_git()

# Paso 3: Crea un token para la computadora
usethis::create_github_token() 

# SOLO SE VUELVE A CORRER SI SE CAMBIA DE COMPUTADORA, NO CADA QUE SE HAGA UN NUEVO REPOSITORIO

# NUNCA PONER EL TOKEN DENTRO DE UN CODIGO QUE SE SUBIRA A GITHUB
# ESO GENERA UN ERROR QUE NO PERMITE QUE SE SUBA EL ARCHIVO, PORQUE INCLUYE UN "SECRETO"


# Paso 4: Darle permiso a la computadora de conectarse con la cuenta de github
gitcreds::gitcreds_set() # Si la computadora ya está configurada no hay que volver a correr esto

# Paso 5: # Enlaza la computadora con tu usuario
usethis::use_git_config(
  user.name = "Marianagomez-uaq",
  user.email = "mgomez110@alumnos.uaq.mx"
)

# Paso 6: Sube el repositorio a github, solo se corre una vez por repositorio
usethis::use_github()
# Por default el repositorio es público, si se quiere que sea privado, usar:
# usethis::use_github(private = TRUE)
# Fin
