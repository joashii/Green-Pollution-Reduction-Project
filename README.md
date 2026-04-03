# <span>🌿</span> Pollution Management Shiny App

## Overview

This project is a **Shiny web application** that helps analyze and solve pollution-related problems using optimization techniques. Users can select different projects and view computed results dynamically.

---

## Features

* Interactive UI built with **Shiny**
* Data tables using **DT**
* Dynamic UI behavior using **shinyjs**
* Optimization-based solution generation
* Modular code structure (separated into `src/` files)

---

## Requirements

Make sure you have **R** installed along with the following packages:

```r
install.packages(c("shiny", "DT", "shinyjs"))
```

---

## Running the App

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

### "could not find function 'useShinyjs'"

* Ensure `shinyjs` is installed
* Use: `shinyjs::useShinyjs()`

### "could not find function 'renderDT'"

* Ensure `DT` is installed
* Use: `DT::renderDT()`

### Data not loading

* Check file paths in `source()`
* Ensure `src/` folder exists

---

## Notes

* Packages may need to be reinstalled in cloud environments like Codespaces
* Always commit and push changes to GitHub to save progress

## <span>📄</span> License

This project is for academic purposes.

---

## 💻 Using GitHub Codespaces

### Starting / Opening a Codespace

1. Go to your repository on GitHub
2. Click **Code**
3. Go to the **Codespaces** tab
4. Click your existing Codespace (or **Create codespace on main** if none exists)

---

### Stopping a Codespace (Save Free Usage)

1. Go to your repository
2. Click **Code → Codespaces**
3. Click the **⋯ (three dots)** beside your Codespace
4. Click **Stop Codespace**

> Stopping pauses the environment and prevents it from consuming your free hours.

---

### Resuming a Codespace

1. Go to your repository
2. Click **Code → Codespaces**
3. Click your Codespace (status: *Stopped*)

> If no Codespace appears, create a new one. Your code is safe as long as it was committed and pushed.

---

### Running the App in Codespaces

Open a terminal and run:

```bash
R
```

Then inside R:

```r
install.packages(c("shiny", "DT", "shinyjs"))
shiny::runApp(host = "0.0.0.0", port = 3838)
```

Open the forwarded port (3838) in your browser.

