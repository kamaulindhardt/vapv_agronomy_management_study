# Runs the full analysis + figure build. From a terminal (NOT with the project root as working
# directory, which would trigger renv):
#   Rscript --vanilla run_all.R "<path to project root>"
# Then build the Word/PDF report with:  python build_report_docx.py
options(warn = 1)   # print warnings where they happen
here <- dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1])))
source(file.path(here, "00_setup.R"))
source(file.path(here, "01_data_and_tables.R"))
if (file.exists(file.path(here, "02_figures.R"))) source(file.path(here, "02_figures.R"))
