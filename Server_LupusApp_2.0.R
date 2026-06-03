## ----setup, include=FALSE--------------------------------------------------------------------
# Packages used in the project
required_packages <- c(
  "tidyverse",
  "shiny",
  "bslib",
  "nnet"
  )

# Feature for automatically installing and downloading packages
load_or_install <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }
}

load_or_install(required_packages)


## --------------------------------------------------------------------------------------------
#| label: Load dataset

path = "Data/base_de_datos_sin_registros_duplicados_ LupusProjectProducti_DATA_2026-05-11_2035.csv sin_col_vacias_con_suma_SLICC_SLEDAI_pred_dosis_categ_dx_time_curado.csv"
path_2 = "Data/Personas_escaneadas_Lupus_Base_NeurolupusApp.csv"

load_lupus_data <- function(path) {
  raw_data <- read_csv(path, show_col_types = FALSE)
  
  curated_data <- raw_data %>%
    mutate(
      across(
        where(is.character),
        ~ na_if(., "")
      )
    )
  
  return(curated_data)
}

lupus_data <- load_lupus_data(path)
lupus_data_2 <- load_lupus_data(path_2)



## --------------------------------------------------------------------------------------------
#| label: Selecting variables
lupus_data <- lupus_data %>% 
  select(
    dx_time,
    time_symptoms,
    scl90r_score_som,
    scl90r_score_oc,
    scl90r_score_si,
    scl90r_score_dep,
    scl90r_score_ans,
    scl90r_score_hos,
    scl90r_score_fob,
    scl90r_score_para,
    sci90r_score_psi,
    family_sle,
    clasi_ali,
    salud_mental_trans,
    calc_psicologico,
    cal_totalcv,
    comorbidities,
    aditional_treatment,
    prednisolona,
    socioeconomic_status,
    ocupation,
    school,
    home,
    calculated_age,
    cal_glob,
    dolor_corporal_trans,
    record_id,
    sex___1,
    place_of_birth,
    treatment___0,
    treatment___1,
    treatment___2,
    treatment___3,
    treatment___4,
    treatment___5,
    treatment___6,
    treatment___7,
    treatment___8,
    family_member_sle___0,
    family_member_sle___1,
    family_member_sle___2,
    family_member_sle___3,
    family_member_sle___4,
    family_member_sle___5,
    family_member_sle___6,
    family_member_sle___7,
    family_member_sle___8,
    Ocular,
    Neuropsychiatric,
    Renal,
    Pulmonary,
    Cardiovascular,
    Cardiomyopathy,
    `Peripheral vascular`,
    Gastrointestinal,
    Musculoskeletal,
    Skin,
    `Premature gonadal`,
    Diabetes,
    Malignancy,
    `total slicc`,
    `total sledai`,
    `Disease activity classification`
  ) %>% 
  na.omit()


## --------------------------------------------------------------------------------------------
#| label: Formating dataset 01

lupus_data <- lupus_data %>% 
  rename(
    total.slicc = `total slicc`,
    total.sledai = `total sledai`,
    Disease.activity.classification = `Disease activity classification`,
    Peripheral.vascular = `Peripheral vascular`,
    Premature.gonadal = `Premature gonadal`
  )

formated_lupus_data <- lupus_data %>% 
  mutate(
    Disease.activity.classification = factor(
      Disease.activity.classification,
      levels = c(
        "No activity",
        "Mild activity",
        "Moderate activity",
        "High activity",
        "Very high activity"
      ),
      
      # Aplicamos la traducción directa al español
      labels = c(
        "Sin actividad", 
        "Leve", 
        "Moderada", 
        "Alta", 
        "Muy alta"
      ),
      
      ordered = TRUE
    ),
    
    
    clasi_ali = suppressWarnings(as.numeric(clasi_ali)),
    
    clasi_ali_es = factor(
      clasi_ali,
      levels = c(1, 2, 3),
      labels = c(
        "Hábitos inadecuados",
        "Hábitos parcialmente inadecuados",
        "Hábitos adecuados"
      ),
      ordered = TRUE
    ),
    
    comorbidities = factor(
      suppressWarnings(as.numeric(comorbidities)),
      levels = c(1,2,3,4,5,6,7,8,9,11),
      labels = c(
        "Diabetes mellitus",
        "Hipertensión",
        "Cáncer",
        "Enfermedad cardiovascular",
        "Osteoporosis",
        "Artritis reumatoide",
        "Esclerosis múltiple",
        "Problemas de tiroides",
        "Otra",
        "Ninguna"
      )
    ),
    
    home = factor(
      suppressWarnings(as.numeric(home)),
      levels = 1:32,
      labels = c(
        "Aguascalientes",
        "Baja California",
        "Baja California Sur",
        "Campeche",
        "Coahuila de Zaragoza",
        "Colima",
        "Chiapas",
        "Chihuahua",
        "Durango",
        "Ciudad de México",
        "Guanajuato",
        "Guerrero",
        "Hidalgo",
        "Jalisco",
        "México",
        "Michoacán de Ocampo",
        "Morelos",
        "Nayarit",
        "Nuevo León",
        "Oaxaca",
        "Puebla",
        "Querétaro",
        "Quintana Roo",
        "San Luis Potosí",
        "Sinaloa",
        "Sonora",
        "Tabasco",
        "Tamaulipas",
        "Tlaxcala",
        "Veracruz de Ignacio de la Llave",
        "Yucatán",
        "Zacatecas"
      )
    ),
    
    place_of_birth = factor(
      suppressWarnings(as.numeric(place_of_birth)),
      levels = 1:32,
      labels = c(
        "Aguascalientes",
        "Baja California",
        "Baja California Sur",
        "Campeche",
        "Coahuila de Zaragoza",
        "Colima",
        "Chiapas",
        "Chihuahua",
        "Durango",
        "Ciudad de México",
        "Guanajuato",
        "Guerrero",
        "Hidalgo",
        "Jalisco",
        "México",
        "Michoacán de Ocampo",
        "Morelos",
        "Nayarit",
        "Nuevo León",
        "Oaxaca",
        "Puebla",
        "Querétaro",
        "Quintana Roo",
        "San Luis Potosí",
        "Sinaloa",
        "Sonora",
        "Tabasco",
        "Tamaulipas",
        "Tlaxcala",
        "Veracruz de Ignacio de la Llave",
        "Yucatán",
        "Zacatecas"
      )
    ),
    
    school = factor(
      suppressWarnings(as.numeric(school)),
      levels = c(1, 2, 3, 4, 5, 6),
      labels = c(
        "Ninguno",
        "Primaria",
        "Secundaria",
        "Preparatoria/Carrera técnica",
        "Licenciatura",
        "Posgrado"
      )
    ),
    
    ocupation = factor(
      suppressWarnings(as.numeric(ocupation)),
      levels = c(1, 2, 3, 4),
      labels = c(
        "Estudiante",
        "Empleado",
        "Desempleado",
        "Jubilado/Retirado"
      )
    ),
    
    aditional_treatment = factor(
      suppressWarnings(as.numeric(aditional_treatment)),
      levels = c(1, 2),
      labels = c("Sí", "No")
    ),
    family_sle = factor(
  suppressWarnings(as.numeric(family_sle)),
  levels = c(0, 1),
  labels = c("Sí", "No")
    ),
  sex___1 = factor(
  suppressWarnings(as.numeric(sex___1)),
  levels = c(1, 0),
  labels = c("Mujer", "Hombre")
  ),
  prednisolona = factor(
  suppressWarnings(as.numeric(prednisolona)),
  levels = c(0,1,2,3,4,5,6,7,8),
  labels = c(
    "Prednisona",
    "Prednisolona",
    "Deflazacort",
    "Meticorten",
    "Metilprednisolona",
    "Calcort",
    "Betametazona",
    "No",
    "Otro"
  )
),

uses_corticosteroids = prednisolona != "No"
  )

treatment_labels <- c(
  "Antimaláricos",
  "Corticoesteroides",
  "Ácido micofenólico",
  "Azatioprina",
  "Metotrexato",
  "Rituximab",
  "Ciclosporina",
  "Ciclofosfamida",
  "Otros"
)

formated_lupus_data <- formated_lupus_data %>% 
  rowwise() %>% 
  mutate(
    treatment_summary = paste(
      treatment_labels[
        c_across(starts_with("treatment___")) == 1
      ],
      collapse = ", "
    ),
    
    treatment_summary = ifelse(
      treatment_summary == "",
      "Ninguno",
      treatment_summary
    ),
    
    num_treatments = sum(c_across(starts_with("treatment___"))),
    
    any_treatment = num_treatments > 0
  ) %>% 
  ungroup()

family_labels <- c(
  "Papá","Mamá","Herman@s","Abuel@",
  "Tí@","Prim@","Sobrin@","Hij@","Otro"
)

formated_lupus_data <- formated_lupus_data %>% 
  rowwise() %>% 
  mutate(
    # resumen de familiares con lupus
    family_member_sle_summary = if (family_sle == "Sí") {
      vals <- family_labels[
        c_across(starts_with("family_member_sle___")) == 1
      ]
      if (length(vals) == 0) "No especificado" else paste(vals, collapse = ", ")
    } else {
      NA_character_
    },
    
    # número de familiares
    num_family_members_sle = if (family_sle == "Sí") {
      sum(c_across(starts_with("family_member_sle___")))
    } else {
      NA_real_
    },
    
    # familiares de primer grado
    has_first_degree_sle = if (family_sle == "Sí") {
      sum(c_across(c(
        family_member_sle___0,
        family_member_sle___1,
        family_member_sle___2,
        family_member_sle___7
      ))) > 0
    } else {
      NA
    }
  ) %>% 
  ungroup()


formated_lupus_data <- formated_lupus_data %>% 
  mutate(
    # tratamiento total REAL (incluye corticoide)
    total_treatments = num_treatments + ifelse(uses_corticosteroids, 1, 0),
    
    treatment_profile = case_when(
      
      total_treatments == 0 ~ "Sin tratamiento",
      
      total_treatments == 1 & uses_corticosteroids ~ 
        paste0("Monoterapia: Corticoide (", prednisolona, ")"),
      
      total_treatments == 1 ~ 
        paste0("Monoterapia: ", treatment_summary),
      
      total_treatments > 1 ~ 
        paste0(
          "Politerapia: ",
          ifelse(
            treatment_summary == "Ninguno",
            paste0("Corticoide (", prednisolona, ")"),
            paste0(
              treatment_summary,
              ifelse(
                uses_corticosteroids,
                paste0(", Corticoide (", prednisolona, ")"),
                ""
              )
            )
          )
        )
    )
  )

glimpse(formated_lupus_data)


## --------------------------------------------------------------------------------------------
#| label: Formatind Dataset 02
glimpse(lupus_data_2)
lupus_data_2 <- lupus_data_2 %>% 
  select(RedCapID,
         Grado_de_estudios,
         Anios_de_escolaridad,
         Puntaje_MoCA,
         MoCa_identificacion,
         MoCA_atencion,
         MoCA_lenguaje,
         MoCA_abstraccion,
         MoCA_recuerdo_diferido,
         MoCA_orientacion,
         Quality.control.T1,
         Quality.control.FLAIR,
         White.Matter..WM..volume.cm3,
         Total.lesion.count,
         Total.lesion.volume..absolute..cm3,
         Total.lesion.volume..normalized...,
         Total.lesion.burden,
         Caracteristicas_conservadas,
         Variantes_anatomicas,
         Aracnoide_selar,
         Quistes,
         Atrofia_cambios_involutivos,
         Gliosis_sustancia_blanca,
         Malacia_cortical,
         Lesiones_ocupantes_malformaciones
        
         )
glimpse(lupus_data_2)
formated_lupus_data_2 <- lupus_data_2 %>% 
  mutate(
    Grado_de_estudios = factor(
      Grado_de_estudios,
      levels = c(
        "Secundaria",
        "Preparatoria",
        "TSU",
        "Carrera_tecnica",
        "Licenciatura",
        "Maestria",
        "Posgrado",
        "Doctorado"),
      ordered = T
      ),
    
    Quality.control.T1 = factor(
      Quality.control.T1,
      levels = c(
        "A",
        "B",
        "C"),
      ordered = T
      ),
    Quality.control.FLAIR = factor(
      Quality.control.FLAIR,
      levels = c(
        "A",
        "B",
        "C"),
      ordered = T
      ),
    
    )

glimpse(formated_lupus_data_2)



## --------------------------------------------------------------------------------------------
#| label: Renaming variables

formated_lupus_data <- formated_lupus_data %>%
   rename(Anios_viviendo_con_LES=dx_time,
          Anios_retraso_diagnostico=time_symptoms,
          Somatizacion=scl90r_score_som,
          Obsesion_y_compulsion=scl90r_score_oc,
          Sensitividad_interpersonal=scl90r_score_si,
          Depresion=scl90r_score_dep,
          Ansiedad=scl90r_score_ans,
          Hostilidad=scl90r_score_hos,
          Fobia=scl90r_score_fob,
          Ideacion_paranoide=scl90r_score_para,
          Psicoticismo=sci90r_score_psi,
          Tiene_familiar_con_LES=family_sle,
          Clasificacion_habitos_alimentacion=clasi_ali,
          Salud_mental=salud_mental_trans,
          Calidad_de_vida_dominio_psicologico=calc_psicologico,
          Calidad_de_vida_total=cal_totalcv,
          Comorbilidades=comorbidities,
          Tratamiento_adicional_a_LES=aditional_treatment,
          Tratamiento_corticoide=prednisolona,
          Nivel_socioeconomico=socioeconomic_status,
          Ocupacion=ocupation,
          Escolaridad=school,
          Estado_de_residencia=home,
          Edad=calculated_age,
          Indice_calidad_sueno=cal_glob,
          Dolor_corporal=dolor_corporal_trans,
          record_id=record_id,
          Sexo=sex___1,
          Estado_de_nacimiento=place_of_birth,
          Tratamiento_con_antimalaricos=treatment___0,
          Tratamiento_con_corticosteroides=treatment___1,
          Tratamiento_con_micofenolato=treatment___2,
          Tratamiento_con_azatioprina=treatment___3,
          Tratamiento_con_metotrexato=treatment___4,
          Tratamiento_con_rituximab=treatment___5,
          Tratamiento_con_ciclosporina=treatment___6,
          Tratamiento_con_ciclofosfamida=treatment___7,
          Tratamiento_con_otros_farmacos=treatment___8,
          Padre_con_LES=family_member_sle___0,
          Madre_con_LES=family_member_sle___1,
          Hermanos_con_LES=family_member_sle___2,
          Abuelos_con_LES=family_member_sle___3,
          Tios_con_LES=family_member_sle___4,
          Primos_con_LES=family_member_sle___5,
          Sobrinos_con_LES=family_member_sle___6,
          Hijos_con_LES=family_member_sle___7,
          Otros_familiares_con_LES=family_member_sle___8,
          Danio_ocular=Ocular,
          Danio_neuropsiquiatrico=Neuropsychiatric,
          Danio_renal=Renal,
          Danio_pulmonar=Pulmonary,
          Danio_cardiovascular=Cardiovascular,
          Danio_cardiomiopatia=Cardiomyopathy,
          Danio_vascular_periferico=Peripheral.vascular,
          Danio_gastrointestinal=Gastrointestinal,
          Danio_musculoesquletico=Musculoskeletal,
          Danio_cutaneo=Skin,
          Fallo_gonadal_prematuro=Premature.gonadal,
          Padece_diabetes=Diabetes,
          Presenta_cancer=Malignancy,
          Slicc=total.slicc,
          Sledai=total.sledai,
          Nivel_de_actividad_del_LES=Disease.activity.classification
)

formated_lupus_data
glimpse(formated_lupus_data)


## --------------------------------------------------------------------------------------------
#| label: Joint Neurolupus Dataset

# Unimos la base de datos de neuroimagen con los datos clínicos generales
formated_neurolupus_data <- formated_lupus_data_2 %>%
  left_join(
    formated_lupus_data, 
    by = c("RedCapID" = "record_id") # Igualamos las llaves identificadoras
  ) %>%
  # Opcional: Reordenar para tener el ID al inicio y limpiar columnas redundantes si las hubiera
  relocate(RedCapID)

# Verificamos la integración correcta de las variables
glimpse(formated_neurolupus_data)

# Seleccionamos variables

formated_neurolupus_data_02 <- formated_neurolupus_data %>% 
  select(Puntaje_MoCA,
         MoCA_atencion,
         MoCa_identificacion,
         MoCA_lenguaje,
         MoCA_abstraccion,
         MoCA_recuerdo_diferido,
         MoCA_orientacion,
         Total.lesion.count,
         Total.lesion.burden,
         Caracteristicas_conservadas,
         Variantes_anatomicas,
         Aracnoide_selar,
         Quistes,
         Atrofia_cambios_involutivos,
         Gliosis_sustancia_blanca,
         Malacia_cortical,
         Lesiones_ocupantes_malformaciones,
         Anios_viviendo_con_LES,
         Anios_retraso_diagnostico,
         Anios_de_escolaridad,
         Somatizacion,
         Obsesion_y_compulsion,
         Sensitividad_interpersonal,
         Depresion,
         Ansiedad,
         Hostilidad,
         Fobia,
         Ideacion_paranoide,
         Psicoticismo,
         Salud_mental,
         Calidad_de_vida_dominio_psicologico,
         Calidad_de_vida_total,
         Nivel_socioeconomico,
         Edad,
         Indice_calidad_sueno,
         Dolor_corporal,
         Sexo,
         Tratamiento_con_antimalaricos,
         uses_corticosteroids,
         Tratamiento_con_ciclofosfamida,
         Tratamiento_con_rituximab,
         has_first_degree_sle,
         num_family_members_sle,
         num_treatments,
         any_treatment,
         Danio_neuropsiquiatrico,
         Danio_cardiovascular,
         Danio_vascular_periferico,
         Slicc,
         Sledai
         )
glimpse(formated_neurolupus_data_02)



## --------------------------------------------------------------------------------------------
#| label: Pestaña 1 - Pre-procesamiento de variables de daño orgánico
# ---------------------------------------------------------------------
# IMPORTANTE: según data_curating_app_1_2.qmd, las variables de daño
# orgánico (Ocular, Renal, Pulmonary, Cardiovascular, Cardiomyopathy,
# Peripheral.vascular, Gastrointestinal, Musculoskeletal, Skin,
# Premature.gonadal, Diabetes, Malignancy) son INDICADORES BINARIOS
# (1 = presente, 0 = ausente). En el archivo original llegan como
# numéricas 0/1, lo que provoca que el reporte las describa con
# "promedio 0", "min 0", "max 1", una salida no interpretable.
#
# Aquí las convertimos a factor con etiquetas "Presente"/"Ausente"
# SOLO para los efectos de presentación en la Pestaña 1. Los nombres
# internos de las variables NO cambian, manteniendo la regla sobre inmutabilidad de nombres.
# ---------------------------------------------------------------------

danio_organico_vars <- c(
  "Danio_ocular",
  "Danio_neuropsiquiatrico",
  "Danio_renal",
  "Danio_pulmonar",
  "Danio_cardiovascular",
  "Danio_cardiomiopatia",
  "Danio_vascular_periferico",
  "Danio_gastrointestinal",
  "Danio_musculoesquletico",
  "Danio_cutaneo",
  "Fallo_gonadal_prematuro",
  "Padece_diabetes",
  "Presenta_cancer"
)

# Creamos una copia local del dataset SOLO para la Pestaña 1.
# El dataset global (formated_lupus_data) sigue intacto para las
# demás pestañas y modelos.
datos_pestana1 <- formated_lupus_data %>%
  mutate(
    across(
      all_of(danio_organico_vars),
      ~ factor(
          suppressWarnings(as.numeric(.x)),
          levels = c(0, 1),
          labels = c("Ausente", "Presente")
        )
    )
  )

glimpse(
  datos_pestana1 %>% select(all_of(danio_organico_vars))
)


## --------------------------------------------------------------------------------------------
#| label: Neurolupus dictionary

# Diccionario general para el módulo de Modelado Estadístico
diccionario_modelado <- list(
  
  "Actividad y Daño de la Enfermedad" = c(
    "Puntaje SLEDAI (Actividad Continua)" = "Sledai",
    "Nivel de Actividad (Categórica)" = "Nivel_de_actividad_del_LES",
    "Puntaje SLICC (Daño Acumulado)" = "Slicc",
    "Años viviendo con LES" = "Anios_viviendo_con_LES",
    "Años de retraso en diagnóstico" = "Anios_retraso_diagnostico"
  ),
  
  "Daño a Órganos Específicos" = c(
    "Daño Renal" = "Danio_renal",
    "Daño Neuropsiquiátrico" = "Danio_neuropsiquiatrico",
    "Daño Pulmonar" = "Danio_pulmonar",
    "Daño Cardiovascular" = "Danio_cardiovascular",
    "Daño Musculoesquelético" = "Danio_musculoesquletico",
    "Daño Cutáneo" = "Danio_cutaneo",
    "Fallo Gonadal Prematuro" = "Fallo_gonadal_prematuro"
  ),
  
  "Salud Mental y Psicológica" = c(
    "Depresión" = "Depresion",
    "Ansiedad" = "Ansiedad",
    "Somatización" = "Somatizacion",
    "Obsesión y Compulsión" = "Obsesion_y_compulsion",
    "Sensitividad Interpersonal" = "Sensitividad_interpersonal",
    "Hostilidad" = "Hostilidad",
    "Ideación Paranoide" = "Ideacion_paranoide",
    "Psicoticismo" = "Psicoticismo",
    "Percepción de Salud Mental" = "Salud_mental"
  ),
  
  "Calidad de Vida y Estilo de Vida" = c(
    "Calidad de Vida Total" = "Calidad_de_vida_total",
    "Calidad de Vida (Psicológica)" = "Calidad_de_vida_dominio_psicologico",
    "Índice de Calidad de Sueño" = "Indice_calidad_sueno",
    "Dolor Corporal" = "Dolor_corporal",
    "Hábitos de Alimentación" = "clasi_ali_es"
  ),
  
  "Tratamientos Médicos" = c(
    "Uso de Corticoesteroides (Sí/No)" = "uses_corticosteroids",
    "Tipo de Corticoesteroide" = "Tratamiento_corticoide",
    "Total de tratamientos" = "total_treatments",
    "Uso de Antimaláricos" = "Tratamiento_con_antimalaricos",
    "Uso de Rituximab" = "Tratamiento_con_rituximab"
  ),
  
  "Sociodemográficos y Antecedentes" = c(
    "Edad" = "Edad",
    "Sexo" = "Sexo",
    "Escolaridad" = "Escolaridad",
    "Ocupación" = "Ocupacion",
    "Nivel Socioeconómico" = "Nivel_socioeconomico",
    "Estado de Residencia" = "Estado_de_residencia",
    "Comorbilidades Adicionales" = "Comorbilidades",
    "Tiene familiar con LES" = "Tiene_familiar_con_LES"
  )
)

# Diccionario completo para el módulo de Neurolupus
diccionario_neuro_completo <- list(
  
  "Evaluación Cognitiva (MoCA)" = c(
    "Puntaje Total MoCA" = "Puntaje_MoCA",
    "Atención" = "MoCA_atencion",
    "Identificación" = "MoCa_identificacion",
    "Lenguaje" = "MoCA_lenguaje",
    "Abstracción" = "MoCA_abstraccion",
    "Recuerdo Diferido" = "MoCA_recuerdo_diferido",
    "Orientación" = "MoCA_orientacion"
  ),
  
  "Hallazgos de Neuroimagen (RM)" = c(
    "Conteo Total de Lesiones" = "Total.lesion.count",
    "Carga Lesional (Burden)" = "Total.lesion.burden",
    "Características Conservadas" = "Caracteristicas_conservadas",
    "Variantes Anatómicas" = "Variantes_anatomicas",
    "Aracnoide Selar" = "Aracnoide_selar",
    "Quistes" = "Quistes",
    "Atrofia / Cambios Involutivos" = "Atrofia_cambios_involutivos",
    "Gliosis en Sustancia Blanca" = "Gliosis_sustancia_blanca",
    "Malacia Cortical" = "Malacia_cortical",
    "Lesiones Ocupantes / Malformaciones" = "Lesiones_ocupantes_malformaciones"
  ),
  
  "Salud Mental y Psicológica (SCL-90-R y PROMs)" = c(
    "Percepción Global de Salud Mental" = "Salud_mental",
    "Somatización" = "Somatizacion",
    "Obsesión y Compulsión" = "Obsesion_y_compulsion",
    "Sensitividad Interpersonal" = "Sensitividad_interpersonal",
    "Depresión" = "Depresion",
    "Ansiedad" = "Ansiedad",
    "Hostilidad" = "Hostilidad",
    "Fobia" = "Fobia",
    "Ideación Paranoide" = "Ideacion_paranoide",
    "Psicoticismo" = "Psicoticismo",
    "Calidad de Vida (Dominio Psicológico)" = "Calidad_de_vida_dominio_psicologico",
    "Calidad de Vida Total" = "Calidad_de_vida_total",
    "Índice de Calidad de Sueño" = "Indice_calidad_sueno",
    "Dolor Corporal" = "Dolor_corporal"
  ),
  
  "Actividad y Daño Acumulado" = c(
    "Puntaje SLEDAI (Actividad)" = "Sledai",
    "Puntaje SLICC (Daño Total)" = "Slicc",
    "Daño Neuropsiquiátrico" = "Danio_neuropsiquiatrico",
    "Daño Cardiovascular" = "Danio_cardiovascular",
    "Daño Vascular Periférico" = "Danio_vascular_periferico"
  ),
  
  "Perfil Clínico y Tratamiento" = c(
    "Años viviendo con LES" = "Anios_viviendo_con_LES",
    "Años de Retraso Diagnóstico" = "Anios_retraso_diagnostico",
    "Número Total de Tratamientos" = "num_treatments",
    "Tiene Tratamiento Activo" = "any_treatment",
    "Uso de Corticoesteroides" = "uses_corticosteroids",
    "Tratamiento con Antimaláricos" = "Tratamiento_con_antimalaricos",
    "Tratamiento con Ciclofosfamida" = "Tratamiento_con_ciclofosfamida",
    "Tratamiento con Rituximab" = "Tratamiento_con_rituximab"
  ),
  
  "Sociodemográficos y Antecedentes" = c(
    "Edad" = "Edad",
    "Sexo" = "Sexo",
    "Nivel Socioeconómico" = "Nivel_socioeconomico",
    "Años de Escolaridad" = "Anios_de_escolaridad",
    "Familiar de 1er Grado con LES" = "has_first_degree_sle",
    "Número de Familiares con LES" = "num_family_members_sle"
  )
)

# ---------------------------------------------------------------------
# Diccionario para la Pestaña 1: Reporte General
# ---------------------------------------------------------------------
# Cubre todas las variables del dataset renombrado, organizadas en
# grupos legibles para la persona usuaria. Los nombres internos
# (lado derecho) NO cambian; solo las etiquetas humanas (lado izquierdo).
diccionario_reporte_gen <- list(

  "Datos sociodemográficos" = c(
    "Edad" = "Edad",
    "Sexo" = "Sexo",
    "Escolaridad" = "Escolaridad",
    "Ocupación" = "Ocupacion",
    "Nivel socioeconómico" = "Nivel_socioeconomico",
    "Estado de residencia" = "Estado_de_residencia",
    "Estado de nacimiento" = "Estado_de_nacimiento"
  ),

  "Historia clínica de LES" = c(
    "Años viviendo con LES" = "Anios_viviendo_con_LES",
    "Años de retraso en diagnóstico" = "Anios_retraso_diagnostico",
    "Nivel de actividad del LES" = "Nivel_de_actividad_del_LES",
    "Índice SLEDAI (actividad)" = "Sledai",
    "Índice SLICC (daño acumulado)" = "Slicc",
    "Comorbilidades" = "Comorbilidades",
    "Padece diabetes" = "Padece_diabetes",
    "Presenta cáncer" = "Presenta_cancer"
  ),

  "Tratamientos" = c(
    "Resumen de tratamientos" = "treatment_summary",
    "Número de tratamientos" = "num_treatments",
    "¿Recibe algún tratamiento?" = "any_treatment",
    "Número total de tratamientos (incluye corticoide)" = "total_treatments",
    "Perfil de tratamiento" = "treatment_profile",
    "¿Usa corticoesteroides?" = "uses_corticosteroids",
    "Tipo de corticoesteroide" = "Tratamiento_corticoide",
    "Tratamiento adicional a LES" = "Tratamiento_adicional_a_LES"
  ),

  "Daño a órganos" = c(
    "Daño ocular" = "Danio_ocular",
    "Daño neuropsiquiátrico" = "Danio_neuropsiquiatrico",
    "Daño renal" = "Danio_renal",
    "Daño pulmonar" = "Danio_pulmonar",
    "Daño cardiovascular" = "Danio_cardiovascular",
    "Daño cardiomiopatía" = "Danio_cardiomiopatia",
    "Daño vascular periférico" = "Danio_vascular_periferico",
    "Daño gastrointestinal" = "Danio_gastrointestinal",
    "Daño musculoesquelético" = "Danio_musculoesquletico",
    "Daño cutáneo" = "Danio_cutaneo",
    "Fallo gonadal prematuro" = "Fallo_gonadal_prematuro"
  ),

  "Salud mental y calidad de vida" = c(
    "Percepción de salud mental" = "Salud_mental",
    "Somatización" = "Somatizacion",
    "Obsesión y compulsión" = "Obsesion_y_compulsion",
    "Sensitividad interpersonal" = "Sensitividad_interpersonal",
    "Depresión" = "Depresion",
    "Ansiedad" = "Ansiedad",
    "Hostilidad" = "Hostilidad",
    "Fobia" = "Fobia",
    "Ideación paranoide" = "Ideacion_paranoide",
    "Psicoticismo" = "Psicoticismo",
    "Calidad de vida total" = "Calidad_de_vida_total",
    "Calidad de vida (dominio psicológico)" = "Calidad_de_vida_dominio_psicologico",
    "Índice de calidad de sueño" = "Indice_calidad_sueno",
    "Dolor corporal" = "Dolor_corporal",
    "Hábitos de alimentación" = "clasi_ali_es"
  ),

  "Antecedentes familiares" = c(
    "Tiene familiar con LES" = "Tiene_familiar_con_LES",
    "Resumen de familiares con LES" = "family_member_sle_summary",
    "Número de familiares con LES" = "num_family_members_sle",
    "Familiar de primer grado con LES" = "has_first_degree_sle"
  )
)



## --------------------------------------------------------------------------------------------
#| label: Lupus App

# Procesamiento de datos

# Interfaz de Usuario (UI)
ui <- page_navbar(
  title = "LupusRGMX Data App",
  # Tema personalizado "Sitio Hermano LupusRGMX"
  theme = bs_theme(
    version = 5,
    bg = "#FAFAFA",         
    fg = "#2D2D2D",         
    primary = "#5A2A7A",    # Morado oscuro del logo oficial
    secondary = "#D1C4E9",  
    success = "#D1C4E9",    # <-- NUEVO: Dorado/Ámbar (Contraste perfecto para descargas/links)
    
    base_font = font_google("Open Sans"), 
    heading_font = font_google("Montserrat") 
  ),
  
  tags$head(
    tags$style(HTML("
      .navbar-nav .nav-link {
        color: #FFFFFF !important;    /* Texto blanco en los inactivos */
        opacity: 0.8 !important;     /* Ligeramente transparente pero muy legible */
        font-weight: 500;            /* Un poco más de grosor para que resalten */
      }
      .navbar-nav .nav-link.active {
        opacity: 1 !important;       /* El activo se mantiene al 100% */
        font-weight: 700;            /* El activo se pone en negrita */
      }
    "))
  ),
  
  # Esto fuerza a que la barra de navegación superior tome el color "primary"
  bg = "#5A2A7A",
  
  
  # Pestaña 1: Reporte General
  nav_panel(title = "Reporte General",
            sidebarLayout(
              sidebarPanel(
                h4("Variables a describir"),
                helpText(
                  "Selecciona una o varias variables del registro. ",
                  "El reporte describe a toda la población del Registro ",
                  "Mexicano de Lupus para las variables elegidas. ",
                  "Los valores calculados se resaltan en morado."
                ),
                selectizeInput(
                  inputId  = "var_gen",
                  label    = "Variables:",
                  choices  = diccionario_reporte_gen,
                  selected = c("Edad", "Sexo", "Nivel_de_actividad_del_LES"),
                  multiple = TRUE,
                  options  = list(
                    placeholder = "Escribe o elige variables..."
                  )
                ),
                actionButton("run_gen", "Generar reporte", class = "btn-primary")
              ),
              mainPanel(
                h3("Resumen descriptivo del registro"),
                htmlOutput("txt_reporte_gen")
              )
            )
  ),
  
  
  # Pestaña 2: Modelado Estadístico
  nav_panel(title = "Modelado Estadístico",
            sidebarLayout(
              sidebarPanel(
                h4("Configuración del Modelo General"),
                
                # Selección de Variable Objetivo (Target)
                selectInput("var_target", "Variable Objetivo (Resultado):", 
                            choices = diccionario_modelado),
                
                # Selección de Predictores (Máximo 10)
                selectizeInput("var_predict", "Variables Predictoras (Máximo 10):", 
                               choices = diccionario_modelado, 
                               multiple = TRUE, 
                               options = list(maxItems = 10)),
                
                hr(),
                actionButton("run_model", "Ejecutar Modelo Estadístico", class = "btn-primary")
              ),
              mainPanel(
                h3("Reporte del Modelo Estructural"),
                wellPanel(
                  # Usamos verbatimTextOutput para respetar los saltos de línea (\n)
                  verbatimTextOutput("txt_reporte_modelo") 
                ),
                br(),
                downloadButton("download_modelo_summary", "Descargar Resumen Técnico (.txt)", class = "btn-success")
              )
            )
  ),
  
  # Pestaña 3: Neurolupus
  nav_panel(title = "Neurolupus",
            # En la sección de la UI -> nav_panel(title = "Neurolupus", ...)
sidebarLayout(
  sidebarPanel(
    h4("Análisis de Predicción Neurolupus"),
    # Selección de Variable Objetivo
    selectInput("neuro_target", "Variable Objetivo (Resultado):", 
                choices = diccionario_neuro_completo),
    
    # Selección de Predictores (Máximo 5)
    selectizeInput("neuro_predictors", "Variables Predictoras (Máximo 5):", 
                   choices = diccionario_neuro_completo, 
                   multiple = TRUE, 
                   options = list(maxItems = 5)),
    
    hr(),
    actionButton("run_neuro_model", "Efectuar Análisis Estadístico", class = "btn-primary")
  ),
  mainPanel(
    h3("Reporte de Análisis Neurocognitivo"),
    wellPanel(
      verbatimTextOutput("txt_reporte_neuro_model")
    ),
    br(), # Un pequeño salto de línea visual
    # Nuevo botón de descarga para el output técnico
    downloadButton("download_neuro_summary", "Descargar Resumen Técnico (.txt)", class = "btn-success")
  )
)
  ),
  
  
  # Pestaña 4: Acceso a Datos
  nav_panel(title = "Acceso a Datos",
            div(class = "container mt-5",
                h2("Solicitud de Datos"),
                p("Los datos crudos de este estudio se encuentran resguardados para proteger la privacidad de las y los participantes."),
                p("Si eres investigador y deseas acceder a la base de datos para análisis colaborativos, por favor completa el formulario de solicitud formal."),
                br(),
                a(href = "URL_DE_TU_REDCAP", target = "_blank", class = "btn btn-lg btn-success", "Ir al Formulario en RedCap")
            )
  )
)

# Lógica del Servidor (Server)
# Lógica del Servidor (Server)
server <- function(input, output, session) {
  
  # ==================================================================
  # Lógica Pestaña 1: Reporte General descriptivo
  # ==================================================================
  # Estrategia:
  # - El reporte describe SIEMPRE al registro completo (sin filtros).
  # - La persona usuaria elige una o varias variables.
  # - Para cada variable se produce un párrafo narrativo en español.
  # - Los valores calculados (porcentajes, conteos, promedios SOLO
  #   cuando aplican) se resaltan con la etiqueta <mark> de HTML para
  #   que sean fáciles de identificar visualmente cuando cambia la
  #   selección.
  # - htmlOutput permite renderizar el HTML directamente.
  #
  # 
  datos_reporte_gen <- datos_pestana1

  # Diccionario plano (etiqueta humana -> nombre interno)
  etiquetas_planas_p1 <- unlist(diccionario_reporte_gen, use.names = TRUE)
  # Inverso para obtener etiqueta humana a partir del nombre interno
  inv_etiquetas_p1 <- setNames(
    sub("^[^.]+\\.", "", names(etiquetas_planas_p1)),
    unname(etiquetas_planas_p1)
  )

  # ------------------------------------------------------------------
  # Helper: etiqueta humana para una variable interna
  # ------------------------------------------------------------------
  etiqueta_humana_p1 <- function(var_interna) {
    if (var_interna %in% names(inv_etiquetas_p1)) {
      inv_etiquetas_p1[[var_interna]]
    } else {
      gsub("_", " ", var_interna)
    }
  }

  # ------------------------------------------------------------------
  # Helper: formatear número con separador de miles y 1 decimal cuando aplica
  # ------------------------------------------------------------------
  formatear_num <- function(x, digitos = 1) {
    if (is.na(x)) return("ND")
    if (x == round(x) && abs(x) < 1e5) {
      format(x, big.mark = ",", scientific = FALSE)
    } else {
      formatC(x, format = "f", digits = digitos, big.mark = ",")
    }
  }

  # ------------------------------------------------------------------
  # Helper: envolver un valor en <mark> para resaltarlo en HTML
  # ------------------------------------------------------------------
  resaltar <- function(x) {
    paste0("<mark style='background-color:#e0c3fc; padding:0 3px; ",
           "border-radius:3px;'><b>", x, "</b></mark>")
  }

  # Operador %||% por si no está disponible (R < 4.4)
  `%||%` <- function(a, b) if (is.null(a)) b else a

  # ==================================================================
  # CATÁLOGO DE VARIABLES NUMÉRICAS CON INTERPRETACIÓN CLÍNICA
  # ==================================================================
  # Tres tipos:
  #
  # - "conteo": variables donde el promedio sí tiene sentido (edad,
  #   años con LES, número de tratamientos). Se reportan promedio,
  #   mediana y rango.
  #
  # - "umbral_simple": un solo umbral validado en el data curating
  #   (ej. SLEDAI ≥ 6 = activa). Se reporta % por encima del umbral.
  #
  # - "categorias_clinicas": el data curating define varios cortes
  #   clínicos (ej. PSQI <5 / 5-7 / 8-14 / ≥15). Se reporta el % de
  #   personas en cada categoría clínica.
  #
  # - "scl90": puntajes SCL-90R con rango teórico definido. Se reporta
  #   el % de personas en bajo, intermedio y alto según terciles del
  #   rango teórico (no del registro). Mayor puntaje = mayor severidad.
  # ==================================================================
  catalogo_numerico_p1 <- list(
    # --- Variables de conteo / tiempo: tiene sentido el promedio ---
    Edad                          = list(tipo = "conteo", unidad = "años"),
    Anios_viviendo_con_LES        = list(tipo = "conteo", unidad = "años"),
    Anios_retraso_diagnostico     = list(tipo = "conteo", unidad = "años"),
    num_treatments                = list(tipo = "conteo", unidad = "tratamientos"),
    total_treatments              = list(tipo = "conteo", unidad = "tratamientos"),
    num_family_members_sle        = list(tipo = "conteo", unidad = "familiares"),

    # --- SLEDAI: umbral validado (>= 6 enfermedad activa) ---
    Sledai = list(
      tipo = "umbral_simple",
      umbral = 6,
      comparador = ">=",
      etiqueta_sobre = "presenta enfermedad activa (SLEDAI ≥ 6)",
      etiqueta_bajo  = "presenta baja actividad o enfermedad inactiva (SLEDAI < 6)"
    ),

    # --- SLICC: umbral validado (>= 1 con daño acumulado) ---
    Slicc = list(
      tipo = "umbral_simple",
      umbral = 1,
      comparador = ">=",
      etiqueta_sobre = "presenta algún daño acumulado (SLICC ≥ 1)",
      etiqueta_bajo  = "no presenta daño acumulado (SLICC = 0)"
    ),

    # --- PSQI (cal_glob / Indice_calidad_sueno) ---
    # Data curating: <5 sin problemas, 5-7 atención médica,
    # 8-14 evaluación clínica, ≥15 alteraciones graves.
    Indice_calidad_sueno = list(
      tipo = "categorias_clinicas",
      cortes = c(0, 5, 8, 15, Inf),
      cerrado_izq = TRUE,  # intervalos [a, b)
      etiquetas = c(
        "Sin problemas de sueño (PSQI < 5)",
        "Posible necesidad de atención médica (PSQI 5-7)",
        "Requiere evaluación clínica (PSQI 8-14)",
        "Alteraciones graves del sueño (PSQI ≥ 15)"
      ),
      nota = "Puntajes más altos indican peor calidad de sueño."
    ),

    # --- Calidad de vida total (WHOQOL, cal_totalcv) ---
    # Data curating: > 40 indica alta calidad de vida.
    Calidad_de_vida_total = list(
      tipo = "umbral_simple",
      umbral = 40,
      comparador = ">",
      etiqueta_sobre = "reporta alta calidad de vida (WHOQOL > 40)",
      etiqueta_bajo  = "reporta calidad de vida no alta (WHOQOL ≤ 40)"
    ),

    # --- Calidad de vida psicológica (WHOQOL, calc_psicologico) ---
    # Data curating: máximo 24, mayor = mejor satisfacción.
    # Cortamos en terciles del rango teórico [0, 24].
    Calidad_de_vida_dominio_psicologico = list(
      tipo = "categorias_clinicas",
      cortes = c(0, 8, 16, 24.001),
      cerrado_izq = TRUE,
      etiquetas = c(
        "Baja satisfacción psicológica (0-7)",
        "Satisfacción psicológica intermedia (8-15)",
        "Alta satisfacción psicológica (16-24)"
      ),
      nota = "Escala WHOQOL (máximo 24): puntajes más altos reflejan mayor satisfacción."
    ),

    # --- Salud mental SF-36 (salud_mental_trans) ---
    # Data curating: 0-100, mayor = mejor salud mental percibida.
    Salud_mental = list(
      tipo = "categorias_clinicas",
      cortes = c(0, 34, 67, 100.001),
      cerrado_izq = TRUE,
      etiquetas = c(
        "Salud mental percibida baja (0-33)",
        "Salud mental percibida intermedia (34-66)",
        "Salud mental percibida alta (67-100)"
      ),
      nota = "Escala SF-36 (0-100): puntajes más altos indican mejor percepción de salud mental."
    ),

    # --- Dolor corporal SF-36 (dolor_corporal_trans) ---
    # Data curating: 0-100, mayor = mejor (menos dolor).
    Dolor_corporal = list(
      tipo = "categorias_clinicas",
      cortes = c(0, 34, 67, 100.001),
      cerrado_izq = TRUE,
      etiquetas = c(
        "Dolor corporal alto / mal estado (0-33)",
        "Dolor corporal intermedio (34-66)",
        "Sin dolor corporal o leve (67-100)"
      ),
      nota = "Escala SF-36 (0-100): puntajes más altos indican menor dolor y mejor estado."
    ),

    # --- SCL-90R: rangos teóricos del data curating ---
    # En todos: puntajes más altos = mayor severidad sintomática.
    Somatizacion = list(
      tipo = "scl90",
      rango = c(11, 55),
      nota = "Escala SCL-90R (11-55): puntajes más altos indican mayor severidad de síntomas somáticos."
    ),
    Obsesion_y_compulsion = list(
      tipo = "scl90",
      rango = c(7, 35),
      nota = "Escala SCL-90R (7-35): puntajes más altos indican mayor severidad de síntomas obsesivo-compulsivos."
    ),
    Sensitividad_interpersonal = list(
      tipo = "scl90",
      rango = c(7, 35),
      nota = "Escala SCL-90R (7-35): puntajes más altos indican mayor sensitividad interpersonal."
    ),
    Depresion = list(
      tipo = "scl90",
      rango = c(12, 60),
      nota = "Escala SCL-90R (12-60): puntajes más altos indican mayor severidad de síntomas depresivos."
    ),
    Ansiedad = list(
      tipo = "scl90",
      rango = c(9, 45),
      nota = "Escala SCL-90R (9-45): puntajes más altos indican mayor severidad de síntomas ansiosos."
    ),
    Hostilidad = list(
      tipo = "scl90",
      rango = c(5, 25),
      nota = "Escala SCL-90R (5-25): puntajes más altos indican mayor hostilidad."
    ),
    Fobia = list(
      tipo = "scl90",
      rango = c(3, 15),
      nota = "Escala SCL-90R (3-15): puntajes más altos indican mayor ansiedad fóbica."
    ),
    Ideacion_paranoide = list(
      tipo = "scl90",
      rango = c(5, 25),
      nota = "Escala SCL-90R (5-25): puntajes más altos indican mayor ideación paranoide."
    ),
    Psicoticismo = list(
      tipo = "scl90",
      rango = c(7, 35),
      nota = "Escala SCL-90R (7-35): puntajes más altos indican mayor psicoticismo."
    )
  )

  # ==================================================================
  # FUNCIONES DESCRIPTORAS POR TIPO DE VARIABLE
  # ==================================================================

  # ------------------------------------------------------------------
  # Variable numérica de tipo "conteo" -> promedio, mediana, rango
  # ------------------------------------------------------------------
  describir_conteo <- function(df, var, unidad = "") {
    valores <- df[[var]]
    valores <- valores[!is.na(valores)]
    n <- length(valores)
    etiqueta <- etiqueta_humana_p1(var)
    if (n == 0) {
      return(paste0("<p><b>", etiqueta,
                    ":</b> no hay datos disponibles en el registro.</p>"))
    }
    promedio <- mean(valores)
    mediana  <- median(valores)
    minimo   <- min(valores)
    maximo   <- max(valores)
    unidad_txt <- if (nzchar(unidad)) paste0(" ", unidad) else ""
    paste0(
      "<p><b>", etiqueta, ":</b> ",
      "Se cuenta con información de ", resaltar(formatear_num(n, 0)),
      " personas. En promedio son ",
      resaltar(paste0(formatear_num(promedio, 1), unidad_txt)),
      " (mediana ", resaltar(paste0(formatear_num(mediana, 1), unidad_txt)),
      "), con un rango que va de ",
      resaltar(paste0(formatear_num(minimo, 1), unidad_txt)),
      " a ", resaltar(paste0(formatear_num(maximo, 1), unidad_txt)), ".</p>"
    )
  }

  # ------------------------------------------------------------------
  # Variable numérica con umbral clínico simple
  # ------------------------------------------------------------------
  describir_umbral <- function(df, var, info) {
    valores <- df[[var]]
    valores <- valores[!is.na(valores)]
    n <- length(valores)
    etiqueta <- etiqueta_humana_p1(var)
    if (n == 0) {
      return(paste0("<p><b>", etiqueta,
                    ":</b> no hay datos disponibles en el registro.</p>"))
    }
    sobre <- switch(
      info$comparador,
      ">=" = valores >= info$umbral,
      ">"  = valores >  info$umbral,
      "<=" = valores <= info$umbral,
      "<"  = valores <  info$umbral
    )
    n_sobre <- sum(sobre)
    p_sobre <- 100 * n_sobre / n
    p_bajo  <- 100 - p_sobre
    paste0(
      "<p><b>", etiqueta, ":</b> ",
      "De las ", resaltar(formatear_num(n, 0)),
      " personas con dato disponible, ",
      resaltar(paste0(formatear_num(p_sobre, 1), "%")),
      " (", resaltar(formatear_num(n_sobre, 0)), " personas) ",
      info$etiqueta_sobre,
      ", mientras que ",
      resaltar(paste0(formatear_num(p_bajo, 1), "%")),
      " ", info$etiqueta_bajo, ".</p>"
    )
  }

  # ------------------------------------------------------------------
  # Variable numérica con varias categorías clínicas validadas
  # ------------------------------------------------------------------
  # Toma los cortes definidos en el catálogo y reporta % en cada
  # categoría clínica. NO reporta media porque no aporta significado
  # cuando hay categorías clínicas definidas.
  describir_categorias_clinicas <- function(df, var, info) {
    valores <- df[[var]]
    valores <- valores[!is.na(valores)]
    n <- length(valores)
    etiqueta <- etiqueta_humana_p1(var)
    if (n == 0) {
      return(paste0("<p><b>", etiqueta,
                    ":</b> no hay datos disponibles en el registro.</p>"))
    }
    cortes <- info$cortes
    etiquetas <- info$etiquetas
    # Asignamos categoría con findInterval (left.closed = info$cerrado_izq)
    if (isTRUE(info$cerrado_izq)) {
      idx <- findInterval(valores, cortes, left.open = FALSE,
                          rightmost.closed = FALSE)
    } else {
      idx <- findInterval(valores, cortes, left.open = TRUE,
                          rightmost.closed = TRUE)
    }
    # idx puede dar 0 si valores < cortes[1] o > length(cortes)-1 si excede;
    # los recortamos al rango válido y conteamos
    idx <- pmin(pmax(idx, 1), length(etiquetas))
    conteos <- vapply(seq_along(etiquetas), function(i) sum(idx == i),
                      integer(1))
    porcentajes <- 100 * conteos / n
    partes <- vapply(seq_along(etiquetas), function(i) {
      paste0(etiquetas[i], ": ",
             resaltar(paste0(formatear_num(porcentajes[i], 1), "%")),
             " (", resaltar(formatear_num(conteos[i], 0)), " personas)")
    }, character(1))
    nota_html <- if (!is.null(info$nota)) {
      paste0(" <small><i>", info$nota, "</i></small>")
    } else {
      ""
    }
    paste0(
      "<p><b>", etiqueta, ":</b> ",
      "De las ", resaltar(formatear_num(n, 0)),
      " personas con dato disponible, la distribución por categoría clínica es: ",
      paste(partes, collapse = "; "), ".",
      nota_html, "</p>"
    )
  }

  # ------------------------------------------------------------------
  # Variable numérica tipo SCL-90R: terciles del rango TEÓRICO
  # ------------------------------------------------------------------
  # Usamos el rango oficial de la escala (no del registro). Cortamos
  # ese rango en tres bandas iguales: bajo / intermedio / alto.
  describir_scl90 <- function(df, var, info) {
    valores <- df[[var]]
    valores <- valores[!is.na(valores)]
    n <- length(valores)
    etiqueta <- etiqueta_humana_p1(var)
    if (n == 0) {
      return(paste0("<p><b>", etiqueta,
                    ":</b> no hay datos disponibles en el registro.</p>"))
    }
    rmin <- info$rango[1]
    rmax <- info$rango[2]
    paso <- (rmax - rmin) / 3
    c1 <- rmin + paso
    c2 <- rmin + 2 * paso
    cortes <- c(rmin, c1, c2, rmax + 0.001)
    idx <- findInterval(valores, cortes, left.open = FALSE,
                        rightmost.closed = FALSE)
    idx <- pmin(pmax(idx, 1), 3)
    n_bajo  <- sum(idx == 1)
    n_med   <- sum(idx == 2)
    n_alto  <- sum(idx == 3)
    p_bajo  <- 100 * n_bajo / n
    p_med   <- 100 * n_med  / n
    p_alto  <- 100 * n_alto / n
    nota_html <- if (!is.null(info$nota)) {
      paste0(" <small><i>", info$nota, "</i></small>")
    } else {
      ""
    }
    paste0(
      "<p><b>", etiqueta, ":</b> ",
      "De las ", resaltar(formatear_num(n, 0)),
      " personas con dato disponible: ",
      "severidad baja (",
      formatear_num(rmin, 0), "-", formatear_num(c1, 0), "): ",
      resaltar(paste0(formatear_num(p_bajo, 1), "%")),
      " (", resaltar(formatear_num(n_bajo, 0)), " personas); ",
      "severidad intermedia (",
      formatear_num(c1, 0), "-", formatear_num(c2, 0), "): ",
      resaltar(paste0(formatear_num(p_med, 1), "%")),
      " (", resaltar(formatear_num(n_med, 0)), " personas); ",
      "severidad alta (",
      formatear_num(c2, 0), "-", formatear_num(rmax, 0), "): ",
      resaltar(paste0(formatear_num(p_alto, 1), "%")),
      " (", resaltar(formatear_num(n_alto, 0)), " personas).",
      nota_html, "</p>"
    )
  }

  # ------------------------------------------------------------------
  # Despachador para variables numéricas
  # ------------------------------------------------------------------
  # Si la variable NO está catalogada y es numérica, se trata como
  # conteo simple 
  describir_numerica <- function(df, var) {
    info <- catalogo_numerico_p1[[var]]
    if (is.null(info)) {
      return(describir_conteo(df, var, unidad = ""))
    }
    switch(
      info$tipo,
      "conteo"               = describir_conteo(df, var, unidad = info$unidad %||% ""),
      "umbral_simple"        = describir_umbral(df, var, info),
      "categorias_clinicas"  = describir_categorias_clinicas(df, var, info),
      "scl90"                = describir_scl90(df, var, info),
      describir_conteo(df, var, unidad = "")  # fallback
    )
  }

  # ------------------------------------------------------------------
  # Variable lógica (Sí/No)
  # ------------------------------------------------------------------
  describir_logica <- function(df, var) {
    valores <- df[[var]]
    valores <- valores[!is.na(valores)]
    n <- length(valores)
    etiqueta <- etiqueta_humana_p1(var)
    if (n == 0) {
      return(paste0("<p><b>", etiqueta,
                    ":</b> no hay datos disponibles en el registro.</p>"))
    }
    n_si  <- sum(valores == TRUE)
    p_si  <- 100 * n_si / n
    paste0(
      "<p><b>", etiqueta, ":</b> ",
      "De las ", resaltar(formatear_num(n, 0)),
      " personas con dato disponible, ",
      resaltar(formatear_num(n_si, 0)),
      " responden \"Sí\", lo que equivale al ",
      resaltar(paste0(formatear_num(p_si, 1), "%")),
      " del total.</p>"
    )
  }

  # ------------------------------------------------------------------
  # Variable categórica / factor / texto
  # ------------------------------------------------------------------
  # Las variables de daño orgánico
  # son factores "Ausente"/"Presente" (ver bloque de pre-procesamiento
  # de Pestaña 1). Para ese tipo de variable binaria con etiqueta
  # "Presente" preferimos un mensaje específico: "X% presenta daño".
  describir_categorica <- function(df, var) {
    valores <- df[[var]]
    valores <- valores[!is.na(valores) & valores != ""]
    n <- length(valores)
    etiqueta <- etiqueta_humana_p1(var)
    if (n == 0) {
      return(paste0("<p><b>", etiqueta,
                    ":</b> no hay datos disponibles en el registro.</p>"))
    }
    tabla <- sort(table(valores), decreasing = TRUE)
    niveles <- names(tabla)

    # Caso especial: variable de daño orgánico (binaria Presente/Ausente)
    # devuelve un mensaje clínico directo.
    if (var %in% danio_organico_vars &&
        all(niveles %in% c("Presente", "Ausente"))) {
      n_pres <- as.integer(tabla["Presente"] %||% 0)
      p_pres <- 100 * n_pres / n
      p_aus  <- 100 - p_pres
      return(paste0(
        "<p><b>", etiqueta, ":</b> ",
        "De las ", resaltar(formatear_num(n, 0)),
        " personas con dato disponible, ",
        resaltar(paste0(formatear_num(p_pres, 1), "%")),
        " (", resaltar(formatear_num(n_pres, 0)), " personas) ",
        "presenta esta afectación, mientras que ",
        resaltar(paste0(formatear_num(p_aus, 1), "%")),
        " no la presenta.</p>"
      ))
    }

    # Caso general: top 3 categorías
    top_n <- min(3, length(tabla))
    partes <- vapply(seq_len(top_n), function(i) {
      categoria <- names(tabla)[i]
      conteo <- as.integer(tabla[i])
      pct <- 100 * conteo / n
      paste0("\"", categoria, "\" con ",
             resaltar(paste0(formatear_num(pct, 1), "%")),
             " (", resaltar(formatear_num(conteo, 0)), " personas)")
    }, character(1))
    encabezado <- if (top_n == 1) {
      "La única categoría observada es "
    } else {
      "Las categorías más frecuentes son: "
    }
    paste0(
      "<p><b>", etiqueta, ":</b> ",
      "Se cuenta con información de ", resaltar(formatear_num(n, 0)),
      " personas. ", encabezado,
      paste(partes, collapse = "; "), ".</p>"
    )
  }

  # ------------------------------------------------------------------
  # Despachador principal: elige función según la clase de la variable
  # ------------------------------------------------------------------
  describir_variable <- function(df, var) {
    if (!var %in% names(df)) {
      return(paste0("<p><i>La variable <b>", var,
                    "</b> no está disponible en el registro.</i></p>"))
    }
    x <- df[[var]]
    if (is.logical(x)) {
      describir_logica(df, var)
    } else if (is.numeric(x)) {
      describir_numerica(df, var)
    } else {
      describir_categorica(df, var)
    }
  }

  # ------------------------------------------------------------------
  # Reporte reactivo: se recalcula al presionar "Generar reporte"
  # ------------------------------------------------------------------
  reporte_general_reactivo <- eventReactive(input$run_gen, {
    req(input$var_gen)
    vars <- input$var_gen
    n_total <- nrow(datos_reporte_gen)

    encabezado <- paste0(
      "<p>Este reporte describe a la población del ",
      "<b>Registro Mexicano de Lupus</b>, que incluye un total de ",
      resaltar(formatear_num(n_total, 0)),
      " personas participantes. A continuación se presentan las ",
      resaltar(formatear_num(length(vars), 0)),
      " variable(s) que seleccionaste:</p>"
    )

    bloques <- vapply(
      vars,
      function(v) describir_variable(datos_reporte_gen, v),
      character(1)
    )

    paste0(encabezado, paste(bloques, collapse = ""))
  })

  output$txt_reporte_gen <- renderUI({
    HTML(reporte_general_reactivo())
  })

  
  # --- Lógica Pestaña 2: Modelado Estadístico ---
  
  modelo_general_reactivo <- eventReactive(input$run_model, {
    req(input$var_target, input$var_predict)
    
    # 1. Preparación de datos 
    df_model <- formated_lupus_data 
    target_var <- input$var_target
    pred_vars <- input$var_predict
    
    # Prevenir auto-referencia
    if(target_var %in% pred_vars) {
      msg <- "Error: La variable objetivo no puede estar incluida entre las predictoras. Por favor, ajusta tu selección."
      return(list(texto_humano = msg, texto_tecnico = msg))
    }
    
    all_labels <- unlist(diccionario_modelado)
    target_label <- names(all_labels)[all_labels == target_var]
    
    # 2. Detección del tipo de modelo (Lineal / Binomial / Multinomial)
    y_data <- df_model[[target_var]]
    n_levels <- length(unique(na.omit(y_data)))
    is_categorical <- is.factor(y_data) || is.character(y_data)
    
    if (is_categorical && n_levels > 2) {
      tipo_regresion <- "multinomial"
      nombre_metodo <- "Regresión Logística Multinomial"
    } else if ((is_categorical && n_levels == 2) || (is.numeric(y_data) && n_levels == 2)) {
      tipo_regresion <- "binomial"
      nombre_metodo <- "Regresión Binomial (Logística)"
    } else {
      tipo_regresion <- "lineal"
      nombre_metodo <- "Regresión Lineal Múltiple"
    }
    
    formula_model <- as.formula(paste(target_var, "~", paste(pred_vars, collapse = " + ")))
    
    # 3. Ejecución segura del modelo
    tryCatch({
      if(tipo_regresion == "binomial") {
        fit <- glm(formula_model, data = df_model, family = "binomial")
        n_sujetos <- nobs(fit)
      } else if (tipo_regresion == "multinomial") {
        # nnet::multinom es silencioso gracias a trace = FALSE
        fit <- nnet::multinom(formula_model, data = df_model, trace = FALSE)
        n_sujetos <- nrow(model.frame(fit)) # multinom usa model.frame para el N exacto
      } else {
        fit <- lm(formula_model, data = df_model)
        n_sujetos <- nobs(fit)
      }
      
      # 4. Redacción del reporte humano
      reporte_lineas <- c(
        "### RESUMEN DEL MODELO ESTRUCTURAL GENERAL ###", "",
        paste("Objetivo: Evaluar el efecto sobre '", target_label, "'.", sep = ""),
        paste("Muestra: El análisis se ejecutó exitosamente con ", n_sujetos, " sujetos válidos.", sep = ""),
        paste("Método: Se aplicó un modelo de", nombre_metodo, "ajustado por", length(pred_vars), "variables."), ""
      )
      
      # --- PARSEO DE RESULTADOS DEPENDIENDO DEL MODELO ---
      
      if (tipo_regresion == "multinomial") {
        # Para multinomial: calcular p-values mediante Test de Wald
        summ <- summary(fit)
        z_vals <- summ$coefficients / summ$standard.errors
        p_matrix <- (1 - pnorm(abs(z_vals), 0, 1)) * 2
        
        reporte_lineas <- c(reporte_lineas, "RESULTADOS CLAVE (p <= 0.05 frente al grupo de referencia):")
        hay_sig <- FALSE
        
        clases <- rownames(p_matrix)
        predictores <- colnames(p_matrix)
        
        # Iterar sobre las clases (Ej: Nivel Actividad Alta vs Sin Actividad)
        for (i in 1:nrow(p_matrix)) {
          clase_actual <- clases[i]
          for (j in 1:ncol(p_matrix)) {
            if (predictores[j] != "(Intercept)" && p_matrix[i, j] <= 0.05) {
              hay_sig <- TRUE
              var_tec <- predictores[j]
              
              # Traductor de variables dummy
              clean_label <- var_tec
              for(v in pred_vars) {
                if(grepl(v, var_tec)) {
                  clean_label <- paste0(names(all_labels)[all_labels == v], " (", gsub(v, "", var_tec), ")")
                  break
                }
              }
              if(var_tec %in% all_labels) clean_label <- names(all_labels)[all_labels == var_tec]
              
              or_val <- round(exp(summ$coefficients[i, j]), 2)
              p_val <- round(p_matrix[i, j], 4)
              
              reporte_lineas <- c(reporte_lineas, paste("- '", clean_label, "': Aumenta ", or_val, " veces la probabilidad de presentar '", clase_actual, "' (Odds Ratio, p = ", p_val, ").", sep=""))
            }
          }
        }
        if(!hay_sig) reporte_lineas <- c(reporte_lineas, "No se detectaron asociaciones significativas para los predictores seleccionados.")
        
      } else {
        # Para Binomial y Lineal (Lógica estándar)
        stats <- summary(fit)$coefficients
        p_values <- stats[, 4]
        coef_vals <- stats[, 1]
        
        sig_idx <- which(p_values <= 0.05 & names(p_values) != "(Intercept)")
        
        if(length(sig_idx) > 0) {
          reporte_lineas <- c(reporte_lineas, "RESULTADOS CLAVE (Variables con significancia p <= 0.05):")
          
          for(idx in sig_idx) {
            row_name <- names(p_values)[idx]
            clean_label <- row_name
            
            for(var_tec in pred_vars) {
              if(grepl(var_tec, row_name)) {
                clean_label <- paste0(names(all_labels)[all_labels == var_tec], " (", gsub(var_tec, "", row_name), ")")
                break
              }
            }
            if(row_name %in% all_labels) clean_label <- names(all_labels)[all_labels == row_name]
            
            p_val <- round(p_values[idx], 4)
            
            if(tipo_regresion == "binomial") {
              or_val <- round(exp(coef_vals[idx]), 2)
              reporte_lineas <- c(reporte_lineas, paste("- '", clean_label, "': Aumenta la probabilidad ", or_val, " veces (Odds Ratio, p = ", p_val, ").", sep = ""))
            } else {
              coeff <- round(coef_vals[idx], 2)
              reporte_lineas <- c(reporte_lineas, paste("- '", clean_label, "': Genera un cambio estimado de ", coeff, " unidades (p = ", p_val, ").", sep = ""))
            }
          }
        } else {
          reporte_lineas <- c(reporte_lineas, "RESULTADOS: No se detectaron asociaciones estadísticamente significativas para los predictores seleccionados en esta muestra.")
        }
        
        if(tipo_regresion == "lineal") {
          r_sq <- round(summary(fit)$r.squared * 100, 1)
          reporte_lineas <- c(reporte_lineas, "", paste("AJUSTE: El modelo explica el", r_sq, "% de la variabilidad de la variable objetivo."))
        }
      }
      
      # 5. Generar reporte técnico crudo
      raw_summary <- capture.output(summary(fit))
      header_tecnico <- c(
        "==================================================",
        "REPORTE TÉCNICO DE MODELADO ESTADÍSTICO GENERAL",
        paste("Tipo de Modelo:", nombre_metodo),
        paste("Fecha de análisis:", Sys.time()),
        paste("Sujetos incluidos en la muestra (n):", n_sujetos),
        "==================================================",
        ""
      )
      
      return(list(
        texto_humano = paste(reporte_lineas, collapse = "\n"),
        texto_tecnico = c(header_tecnico, raw_summary)
      ))
      
    }, error = function(e) {
      msg <- paste("Error en el procesamiento: Los datos no permiten la convergencia del modelo.\nVerifica que no exista multicolinealidad extrema o falta de casos válidos.\nDetalle:", e$message)
      return(list(texto_humano = msg, texto_tecnico = msg))
    })
  })

  # Renderizar texto en pantalla de la Pestaña 2
  output$txt_reporte_modelo <- renderText({
    modelo_general_reactivo()$texto_humano
  })

  # Descargar output técnico de la Pestaña 2
  output$download_modelo_summary <- downloadHandler(
    filename = function() { paste("Resumen_Modelo_General_", Sys.Date(), ".txt", sep = "") },
    content = function(file) { writeLines(modelo_general_reactivo()$texto_tecnico, file) }
  )

  # --- Lógica Pestaña 3: Neurolupus ---
  
  modelo_neuro_reactivo <- eventReactive(input$run_neuro_model, {
    req(input$neuro_target, input$neuro_predictors)
    
    df_model <- formated_neurolupus_data_02 
    target_var <- input$neuro_target
    pred_vars <- input$neuro_predictors
    
    all_labels <- unlist(diccionario_neuro_completo)
    target_label <- names(all_labels)[all_labels == target_var]
    
    is_binary <- is.factor(df_model[[target_var]]) || length(unique(na.omit(df_model[[target_var]]))) == 2
    model_type <- if(is_binary) "Regresión Binomial (Logística)" else "Regresión Lineal Múltiple"
    
    formula_model <- as.formula(paste(target_var, "~", paste(pred_vars, collapse = " + ")))
    
    tryCatch({
      if(is_binary) {
        fit <- glm(formula_model, data = df_model, family = "binomial")
      } else {
        fit <- lm(formula_model, data = df_model)
      }
      
      # 1. Calcular el tamaño de la muestra efectiva (n)
      n_sujetos <- nobs(fit)
      
      stats <- summary(fit)$coefficients
      r_sq <- if(!is_binary) round(summary(fit)$r.squared * 100, 1) else NA
      
      # 2. Redacción del Reporte Humano
      reporte_lineas <- c(
        "### RESUMEN DEL MODELO ESTRUCTURAL ###", "",
        paste("Objetivo: Evaluar el efecto sobre '", target_label, "'.", sep = ""),
        paste("Muestra: El análisis incluyó a ", n_sujetos, " sujetos.", sep = ""),
        paste("Método: Se aplicó una", model_type, "ajustada por", length(pred_vars), "variables."), ""
      )
      
      sig_rows <- rownames(stats)[stats[,4] <= 0.05 & rownames(stats) != "(Intercept)"]
      if(length(sig_rows) > 0) {
        reporte_lineas <- c(reporte_lineas, "RESULTADOS CLAVE:")
        for(row in sig_rows) {
          clean_label <- names(all_labels)[all_labels == row]
          p_val <- round(stats[row, 4], 4)
          if(is_binary) {
            or_val <- round(exp(stats[row, 1]), 2)
            reporte_lineas <- c(reporte_lineas, paste("- La variable '", clean_label, "' es significativa (p = ", p_val, "). Por cada unidad de incremento, la probabilidad del evento aumenta ", or_val, " veces (Odds Ratio).", sep = ""))
          } else {
            coeff <- round(stats[row, 1], 2)
            reporte_lineas <- c(reporte_lineas, paste("- '", clean_label, "' muestra una asociación significativa (p = ", p_val, "). Un aumento en esta variable se traduce en un cambio de ", coeff, " unidades en '", target_label, "'.", sep = ""))
          }
        }
      } else {
        reporte_lineas <- c(reporte_lineas, "RESULTADOS: No se detectaron asociaciones con significancia estadística (p <= 0.05) para los predictores seleccionados en esta muestra.")
      }
      
      if(!is_binary) reporte_lineas <- c(reporte_lineas, "", paste("AJUSTE: El modelo explica el", r_sq, "% de la variabilidad observada."))
      
      # 3. Preparar el Resumen Técnico con el N de la muestra
      raw_summary <- capture.output(summary(fit))
      header_tecnico <- c(
        "==================================================",
        "REPORTE TÉCNICO DE NEUROLUPUS",
        paste("Fecha de análisis:", Sys.time()),
        paste("Sujetos incluidos en la muestra (n):", n_sujetos),
        "==================================================",
        ""
      )
      
      return(list(
        texto_humano = paste(reporte_lineas, collapse = "\n"),
        texto_tecnico = c(header_tecnico, raw_summary)
      ))
      
    }, error = function(e) {
      msg <- paste("Error en el procesamiento: Los datos seleccionados no permiten la convergencia del modelo.\nVerifique que las variables tengan suficientes casos válidos (N).\nDetalle:", e$message)
      return(list(texto_humano = msg, texto_tecnico = msg))
    })
  })

  # Renderizar el texto en la pantalla de la app (Pestaña 3)
  output$txt_reporte_neuro_model <- renderText({
    modelo_neuro_reactivo()$texto_humano
  })

  # Manejar la descarga del archivo técnico (Pestaña 3)
  output$download_neuro_summary <- downloadHandler(
    filename = function() {
      paste("Resumen_Estadistico_Neurolupus_", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      writeLines(modelo_neuro_reactivo()$texto_tecnico, file)
    }
  )
  
  # --- Lógica Pestaña 4: Expresión Génica ---
  observeEvent(input$run_gene, {
    output$txt_reporte_gene <- renderText({
      paste("Analizando la expresión de los genes:", paste(input$var_gene, collapse = ", "), 
            "mediante un", input$plot_type, 
            ". En esta sección se integrará el análisis de cuentas normalizadas o Fold-Change.")
    })
    
    # Placeholder para la gráfica
    output$plot_gene <- renderPlot({
      # Aquí irá la lógica de ggplot2 una vez se carguen los datos de expresión
    })
  })
}

shinyApp(ui, server)

