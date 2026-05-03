# Lupus App 2.0
**Building accessible resources to empower communities: the case of the Lupus Mexican Registry**

Lupus App 2.0 is an interactive R Shiny application designed to empower communities and facilitate access to information from the Lupus Mexican Registry. The priority of this tool is to offer clear, human-readable explanations over technical descriptions, making the data interpretable for non-technical users. 

*Note: While this repository documentation is in English, all of the application's interface elements and user outputs are strictly presented in Spanish to serve its target demographic.*

## 👥 Authors
* Grecia Sevilla.
* Dris Sánchez.
* Anita Ledesma.
* Fernanda Bravo.
* Itzel Olivares.
* Domingo Martínez.

## 🚀 App Features and Modules

The application is logically divided into four main tabs:

* **1. General Report (Reporte General):** Allows users to apply general filters (such as Age, Sex, and Comorbidities) and generates a descriptive summary in text format.
* **2. Statistical Modeling (Modelado Estadístico):** An interface to configure and run statistical models (Linear Regression, Binomial Regression, Chi-square) to predict target variables like SLEDAI, Quality of Life, or Lupus Nephritis based on selected predictor variables.
* **3. Neurolupus:** A specialized module to generate neurocognitive and imaging reports, filtering for variables of interest such as Tract Lesions, Cerebrovascular Events, or MoCA Scores.
* **4. Raw Data Access (Acceso a Datos Crudos):** An information portal regarding participant privacy, including a direct link to a REDCap form for researchers to request formal access to the database.

## 🛠️ Technologies and Packages

The project relies heavily on the R `tidyverse` ecosystem. Main dependencies include:
* `tidyverse` (for data manipulation and structuring)
* `shiny` (for the interactive web framework)
* `bslib` (for UI design and theming)

*Note: The script includes a custom `load_or_install()` function that automatically downloads and installs the required packages if they are not found in your local environment.*

## 📖 Development and Contribution Guidelines

If you wish to contribute to the Lupus App 2.0 codebase, please adhere to the following fundamental guidelines established for the project:

* **Language Requirement:** All interface elements and outputs shown to users must be presented in Spanish and written in clear, easy-to-interpret text.
* **Variable Handling:** Variable names within the code must remain unchanged (immutable) in their original format. They should only be translated into human-readable Spanish concepts when presenting outputs to users. Data curation and manipulation must occur within the local environment immediately before each module.
* **Simplicity over Complexity:** Prefer simple, readable, and reproducible code over unnecessarily complex solutions. At this stage, the priority is a functional application; visual and aesthetic improvements will be implemented in future versions.
* **Ecosystem:** The use of the `tidyverse` ecosystem is strongly preferred for functions, data manipulation, naming conventions, and data structures.
* **Validation and Errors:** Validate inputs explicitly and handle missing or inconsistent data to prevent app crashes. Clearly define how missing values (`NA`) are treated.
* **User Experience (UX):** Use progressive disclosure: show detailed results only when needed to avoid overloading the interface with too much information at once.

## 🔒 Data Access

To protect the privacy of the participants, the raw data (`Lupus_App_dataset.csv`) is safeguarded. Communities and researchers wishing to access the database for collaborative analysis must complete the formal request form linked in Tab 4 of the application.
