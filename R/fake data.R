# =========================================================
#  BASE DE DATOS FICTICIA PARA EL NUEVO PROYECTO
#  Nivel de observación: individuo
#  Tratamientos: human_only y ai_assisted
#  Se usa academic_level como medida de seniority
#  Se añade referee_rating como nuevo outcome cualitativo
# =========================================================

set.seed(123)      # reproducibilidad

# Tamaño de la muestra
n <- 300           # ajusta según tu PAP

# ------------ Generación de variables ------------
fake_data <- data.frame(
  id = 1:n,
  
  # Evento (ubicaciones del estudio previo como ejemplo)
  event = sample(
    c("Toronto", "Ottawa", "Sheffield", "Cornell", "Bogota",
      "Tilburg", "Virtual_Europe", "Virtual_North_America", "Virtual_2025"),
    n, replace = TRUE
  ),
  
  # Tratamiento (solo dos brazos)
  treatment = sample(
    c("human_only", "ai_assisted"),
    n, replace = TRUE,
    prob = c(0.5, 0.5)
  ),
  
  # Participación en experimento anterior
  previous_experiment = rbinom(n, 1, 0.3),
  
  # Academic level (define la seniority)
  academic_level = sample(
    c("Student", "Researcher", "Postdoc", "Professor"),
    n, replace = TRUE,
    prob = c(0.6, 0.2, 0.1, 0.1)
  ),
  
  # Años de experiencia en codificación
  years_coding = pmax(0, round(rnorm(n, 8, 4), 1)),
  
  # Experiencia previa con ChatGPT
  chatgpt_experience = sample(
    c("Never", "Beginner", "Intermediate", "Advanced"),
    n, replace = TRUE,
    prob = c(0.2, 0.4, 0.3, 0.1)
  ),
  
  # Software preferido
  software_preference = sample(c("Stata", "R"), n, replace = TRUE, prob = c(0.6, 0.4)),
  
  # Paper asignado
  paper = sample(paste0("Paper_", 1:6), n, replace = TRUE)
)

# ------------ Resultados simulados ------------
# Éxito en la reproducción
fake_data$reproduction_success <- rbinom(
  n, 1,
  ifelse(fake_data$treatment == "ai_assisted", 0.7, 0.75)
)

# Errores menores y mayores
fake_data$minor_errors <- rpois(
  n, lambda = ifelse(fake_data$treatment == "ai_assisted", 0.9, 1.0)
)
fake_data$major_errors <- rpois(
  n, lambda = ifelse(fake_data$treatment == "ai_assisted", 0.5, 0.7)
)

# Robustness checks
fake_data$ran_one_robustness <- rbinom(
  n, 1,
  ifelse(fake_data$treatment == "ai_assisted", 0.7, 0.8)
)
fake_data$ran_two_robustness <- rbinom(
  n, 1,
  ifelse(fake_data$treatment == "ai_assisted", 0.4, 0.5)
)
fake_data$two_good_robustness <- rbinom(
  n, 1,
  ifelse(fake_data$treatment == "ai_assisted", 0.3, 0.4)
)

# Nueva variable: calificación cualitativa del referee report
# Escala: Poor, Fair, Good, Excellent
fake_data$referee_rating <- with(
  fake_data,
  ifelse(reproduction_success == 1,
         sample(c("Good", "Excellent"), n, replace = TRUE, prob = c(0.6, 0.4)),
         sample(c("Poor", "Fair"), n, replace = TRUE, prob = c(0.4, 0.6))
  )
)

# ------------ Actividad de prompts (solo AI-Assisted) ------------
is_ai <- fake_data$treatment == "ai_assisted"

fake_data$prompts <- NA_integer_
fake_data$files   <- NA_integer_
fake_data$images  <- NA_integer_
fake_data$words   <- NA_integer_

fake_data$prompts[is_ai] <- rpois(sum(is_ai), 8)          # media ≈ 8 prompts
fake_data$files[is_ai]   <- rpois(sum(is_ai), 1)          # 0–3 archivos
fake_data$images[is_ai]  <- rpois(sum(is_ai), 0.3)        # mayoría 0
fake_data$words[is_ai]   <- round(rgamma(sum(is_ai), 4, 1/40))  # media ≈ 160

# Vista rápida
head(fake_data)

# Guardar la base si lo necesitas
saveRDS(fake_data, "data/fake_data.rds")
