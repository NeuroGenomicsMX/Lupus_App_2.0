# 📖 LupusRGMX Data App - Guía de Usuario / User Guide

> **[Español]** Bienvenido/a a la Guía de Usuario de la LupusRGMX Data App. Este documento explica cómo instalar, navegar y utilizar los distintos módulos analíticos de la plataforma.
> 
> **[English]** Welcome to the LupusRGMX Data App User Guide. This document explains how to install, navigate, and use the different analytical modules of the platform.

---

## 🇲🇽 Guía en Español

### 1. Introducción
La **LupusRGMX Data App** es una plataforma interactiva diseñada para explorar, visualizar y modelar datos clínicos y epidemiológicos del Registro Mexicano de Lupus. Su objetivo es democratizar el acceso a la información científica para investigadores, personal médico y la comunidad de pacientes, traduciendo datos estadísticos complejos en reportes de texto legibles y amigables.

### 2. Uso Local (Instalación)
Si deseas ejecutar la aplicación en tu propia computadora utilizando RStudio:
1. Clona este repositorio o descarga los archivos.
2. Abre el archivo `Server_LupusApp_2.0.R` (o `Lupus_App_2.0.qmd`).
3. Asegúrate de tener instalados los paquetes requeridos: `tidyverse`, `shiny`, `bslib`, `nnet`, `viridis` y `coin`.
4. Ejecuta la aplicación usando el botón **Run App** en RStudio o mediante el comando `shiny::runApp()`.

### 3. Descripción de los Módulos

La aplicación está dividida en 5 pestañas principales accesibles desde la barra de navegación superior:

#### 📊 Pestaña 1: Reporte General
Genera resúmenes descriptivos de la población del registro.
*   **Cómo usar:** Selecciona una o varias variables clínicas, sociodemográficas o de tratamientos desde el menú desplegable (o marca "Seleccionar todas").
*   **Resultados:** Al hacer clic en "Generar reporte", la plataforma redactará un resumen narrativo donde los cálculos matemáticos (porcentajes, promedios) se resaltarán en color morado para facilitar su lectura.
*   **Exportar:** Puedes descargar el texto generado como un archivo `.txt`.

#### ⚖️ Pestaña 2: Comparativas
Realiza análisis bivariados para encontrar asociaciones estadísticamente significativas entre grupos.
*   **Cómo usar:** Elige una "Variable de Agrupación" (ej. Uso de corticoesteroides) y una "Variable a Comparar" (ej. Actividad del LES).
*   **Resultados:** El sistema seleccionará automáticamente la prueba matemática correcta (Chi-cuadrada para variables categóricas o Permutaciones de Monte Carlo para continuas/ordinales). Mostrará una interpretación clínica automatizada y un gráfico (barras agrupadas, apiladas o densidad).
*   **Exportar:** Permite descargar el análisis estadístico en `.txt` y el gráfico en alta resolución `.png`.

#### 📈 Pestaña 3: Modelado Estadístico
Permite evaluar el efecto de múltiples predictores sobre una variable objetivo clínica.
*   **Cómo usar:** Selecciona una variable resultado (Target) y hasta 10 variables predictoras.
*   **Resultados:** El algoritmo enrutará los datos hacia una Regresión Lineal Múltiple, Logística Binomial o Logística Multinomial, dependiendo del tipo de dato, y redactará los "Resultados Clave" detallando los *Odds Ratios* o tamaños de efecto.
*   **Exportar:** Se puede descargar un reporte técnico detallado en `.txt`.

#### 🧠 Pestaña 4: Neurolupus
Módulo especializado para cruzar evaluaciones cognitivas (ej. MoCA) con hallazgos de neuroimagen (RM).
*   **Cómo usar:** Selecciona una variable objetivo de neuroimagen o cognitiva y hasta 5 predictores clínicos.
*   **Resultados:** Ejecuta modelos predictivos especializados para la cohorte de neurolupus, mostrando las asociaciones estadísticamente significativas.
*   **Exportar:** Descarga del reporte técnico en `.txt`.

#### 📂 Pestaña 5: Acceso a Datos
Por motivos de privacidad y ética, los datos crudos no están expuestos públicamente. Esta pestaña provee el enlace directo al formulario oficial de REDCap para que investigadores e instituciones soliciten acceso colaborativo a la base de datos completa.

---

## 🇬🇧 English Guide

### 1. Introduction
The **LupusRGMX Data App** is an interactive platform designed to explore, visualize, and model clinical and epidemiological data from the Mexican Lupus Registry. Its primary goal is to democratize access to scientific information for researchers, healthcare professionals, and the patient community by translating complex statistical data into human-readable text reports.

### 2. Local Usage (Installation)
To run the application locally on your computer using RStudio:
1. Clone this repository or download the files.
2. Open the `Server_LupusApp_2.0.R` (or `Lupus_App_2.0.qmd`) file.
3. Ensure the required R packages are installed: `tidyverse`, `shiny`, `bslib`, `nnet`, `viridis`, and `coin`.
4. Launch the application by clicking **Run App** in RStudio or by running `shiny::runApp()`.

### 3. Module Descriptions

The application is organized into 5 main tabs accessible from the top navigation bar:

#### 📊 Tab 1: General Report (Reporte General)
Generates descriptive summaries of the registry's population.
*   **How to use:** Select one or multiple clinical, sociodemographic, or treatment variables from the dropdown menu (or check "Select all").
*   **Results:** Upon clicking "Generate report," the platform will draft a narrative summary where mathematical calculations (percentages, means) are highlighted in purple for easy reading.
*   **Export:** You can download the generated narrative as a `.txt` file.

#### ⚖️ Tab 2: Comparisons (Comparativas)
Performs bivariate analyses to find statistically significant associations between patient cohorts.
*   **How to use:** Choose a "Grouping Variable" (e.g., Corticosteroid use) and a "Variable to Compare" (e.g., SLE Activity).
*   **Results:** The system automatically selects the appropriate mathematical test (Chi-square for categorical variables or robust Monte Carlo permutations for continuous/ordinal variables). It outputs an automated clinical interpretation alongside a dynamic plot (grouped bars, 100% stacked bars, or density plots).
*   **Export:** Allows downloading the statistical analysis (`.txt`) and the high-resolution plot (`.png`).

#### 📈 Tab 3: Statistical Modeling (Modelado Estadístico)
Evaluates the effect of multiple predictors on a clinical target variable.
*   **How to use:** Select a target outcome and up to 10 predictor variables.
*   **Results:** The algorithm routes the data to a Multiple Linear, Binomial Logistic, or Multinomial Logistic Regression depending on the data type. It drafts "Key Results," detailing Odds Ratios or effect sizes.
*   **Export:** A comprehensive technical report can be downloaded as a `.txt` file.

#### 🧠 Tab 4: Neurolupus
A specialized module to cross-reference cognitive assessments (e.g., MoCA) with neuroimaging MRI findings.
*   **How to use:** Select a neuroimaging or cognitive target variable and up to 5 clinical predictors.
*   **Results:** Executes specialized predictive models for the neurolupus sub-cohort, displaying statistically significant associations.
*   **Export:** Download the technical report as a `.txt` file.

#### 📂 Tab 5: Data Access (Acceso a Datos)
To protect patient privacy and adhere to ethical standards, raw data is not publicly exposed. This tab provides a direct link to the official REDCap form for researchers and institutions to request collaborative access to the complete database.