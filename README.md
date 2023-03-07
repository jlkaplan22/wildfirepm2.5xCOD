## `wildfirepm2.5 x COD`

Jordan Kaplan's 2023 MS thesis project.

## About 
This repository is Jordan Kaplan's (jlkaplan@stanford.edu) Epidemiology MS thesis project. I am studying the effect of wildfire smoke-related PM2.5
pollution on all-cause and cause-specific mortality.


## Project structure

- **`code/`**: This folder contains all code. Most files should begin with a number and all files should be ordered so they can be run sequentially (e.g., `01_clean_data.R`, `02_fit_models.R`, `03_analyze_results.R`, etc.). There are usually two exceptions: `utils.R` which contains short helper functions you'll use across multiple code files and `mk_nytimes.R` or other plot themes that you'll use across multiple plotting scripts. 
- **`data/`**: In this folder are data that *can be shared* or uploaded to Github. Make sure all data files here adhere to our data use agreement for each project. 
- **`data_private/`**: In this folder are data that **cannot be shared**. Data files here will be `gitignore`d and not shared. 
- **`data_raw/`**: In this folder are the **raw** data. You should treat this folder as *read-only* when interacting with it. For example, if you use the US Census web portal to download population estimates, the files you download go here. When you read in and clean up the files, the data should be saved in `data/`. The files in this folder should **never** be manipulated by hand. Use the `README.md` file in this folder to keep important information handy (e.g., when did you access the data, what URL did you use).
- **`lit/`**: This folder should contain either the PDF of important papers we should read related to this project or URLs to those papers. Optimally, there should be a spreadsheet or document that briefly notes how each paper relates to the project. This folder is crucial for making sure everybody has the same background information. 
- **`manuscript/`**: This is where manuscripts and manuscript-related files should go. Usually, this starts with a URL link to a shared Google Doc, but before submission, this folder should contain Word or latex files. 
- **`output/`**: Many journals ask for the underlying data for each plot. This folder should contain a csv file corresponding to a plot that would allow a third-party to replicate the plot exactly. 
- **`plots/`**: This folder should contain both PDF and JPG (dpi >= 600) versions of the figures used in the manuscript. They should be named according to their order, with a very short description. For example, `fig01_overall_prevalence.pdf` or `figS10_sensitivity_analysis_by_state.pdf`.
- **`rmds/`**: Should contain rmarkdown or quarto files for the tables used in the manuscript. Tables should never be entered by hand. This both minimizes mistakes and allows us to quickly update our numbers when we received new data or change the analysis. 
