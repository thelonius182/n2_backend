# Get project settings
setwd("~/R scripts")
source("project.r")
open_project("ana-2263_de_alliantie", "~/R scripts")

# Read config file
library(yaml)
config <- read_yaml("config.yml")

# Read suppliers
suppliers_raw <- paste0(dir_input, "/", config$file_suppliers_raw)
tbl_suppliers_to_report <- fread(suppliers_raw, data.table = FALSE, na.strings = c("")) %>% 
  mutate(leverancier_type = factor(leverancier_type, levels = unique(leverancier_type))) %>% 
  rename(id_graydon = graydon_id)

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# Kies uit 2 loops: lev_A groepeert bedr.per.sector op personeelsklasse
#                   lev_B groepeert bedr.per.sector op bedrijfstype (SMLX)
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# Report on all suppliers - use sector grouping = number of employees
# for (id_current_supplier in tbl_suppliers_to_report$id_graydon) {
#   config$id_reported_supplier <- id_current_supplier   # Adjust id_graydon of configuration
#   write_yaml(config, "config.yml", fileEncoding = "UTF-8")      # Write new configuration file
#   name_report <- paste0(dir_output_plots, "/Alliantie_lev_A_", id_current_supplier, ".html")  # Determine file name of output HTML
#   rmarkdown::render("report_group_by_awp.rmd", output_file = name_report, encoding = "UTF-8") # Render report
# }

# Report on all suppliers - use sector grouping = company type
for (id_current_supplier in tbl_suppliers_to_report$id_graydon) {
  config$id_reported_supplier <- id_current_supplier   # Adjust id_graydon of configuration
  write_yaml(config, "config.yml", fileEncoding = "UTF-8")      # Write new configuration file
  name_report <- paste0(dir_output_plots, "/Alliantie_lev_B_", id_current_supplier, ".html")  # Determine file name of output HTML
  rmarkdown::render("report_group_by_bdrtype.rmd", output_file = name_report, encoding = "UTF-8") # Render report
}
