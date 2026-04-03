# <span>🌿</span> Pollution Management Shiny App

## <span>📌</span> Overview

This project is a **Shiny web application** that helps analyze and solve pollution-related problems using optimization techniques. Users can select different projects and view computed results dynamically.

---

## <span>🚀</span> Features

* Interactive UI built with **Shiny**
* Data tables using **DT**
* Dynamic UI behavior using **shinyjs**
* Optimization-based solution generation
* Modular code structure (separated into `src/` files)

---

## <span>⚙️</span> Requirements

Make sure you have **R** installed along with the following packages:

```r
install.packages(c("shiny", "DT", "shinyjs"))
```

---

## <span>▶️</span> Running the App

### Option 1: Locally

1. Open R or RStudio
2. Set working directory to project folder
3. Run:

```r
shiny::runApp()
```

---

### Option 2: GitHub Codespaces

1. Open your repository in Codespaces
2. Open terminal
3. Run:

```bash
R
```

4. Then inside R:

```r
install.packages(c("shiny", "DT", "shinyjs"))
shiny::runApp(host = "0.0.0.0", port = 3838)
```

5. Open the forwarded port (3838) in browser

---

## <span>🛠️</span> Troubleshooting

### ❌ "could not find function 'useShinyjs'"

* Ensure `shinyjs` is installed
* Use: `shinyjs::useShinyjs()`

### ❌ "could not find function 'renderDT'"

* Ensure `DT` is installed
* Use: `DT::renderDT()`

### ❌ Data not loading

* Check file paths in `source()`
* Ensure `src/` folder exists

---

## <span>📌</span> Notes

* Packages may need to be reinstalled in cloud environments like Codespaces
* Always commit and push changes to GitHub to save progress
