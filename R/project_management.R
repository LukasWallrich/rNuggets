#' Set up analysis project folder and script files
#'
#' Set up simple folder structure and template files for analysis project.
#'
#' @param folder Root folder of the project to be set up. Defaults to here::here()
#' @param analyses Character vector of analysis steps. R files will be set up in order.
#' @param pipeline_name Name of folder for outputs from each analysis step
#' @param code_folder Logical. Should code files be placed in /code subfolder.
#' Otherwise, are placed in root folder
#' @param standard_packages Character vector of packages to be loaded at start of each analysis file.
#' @param github_packages Character vector of packages to be loaded and installed from Github if needed at start of each analysis file.
#' @export

setup_analysis_project <- function (folder = here::here(), analyses = c("data_prep", "analyses", "presentation"), pipeline_name = "outputs", code_folder = FALSE, standard_packages = c("magrittr", "here", "dplyr"), github_packages = NULL)
{

  folders <- paste0(folder, c("0_data", "1_tools", ifelse(code_folder, "2_code", NULL), paste0("3_", pipeline_name)))

  purrr::map(folders, function(x) {
  if (!dir.exists(x)) {
    dir.create(x)
  }})

  files <- paste0(1:length(analyses), "_", analyses, ".R")
  if (code_folder) files <- paste0("2_code/", files)

  code_template <- glue::glue(code_template)

  for (i in seq_along(analyses)) {
    filename <- analyses[i]
    previous_name <- paste0(analyses[i-1],"")
    code <- glue::glue(code_template, .open = "{{", .close = "}}")
    writeLines(code, file.path(folder, files[i]))
  }

  writeLines(management_functions_file, file.path(folder, "1_tools", "management_functions.R"))

  writeLines(glue::glue(run_all_file), file.path(folder, ifelse(code_folder, "2_code", ""), "0_run_all.R"))


}


code_template <- ('

# ------------
# Introduction
# ------------

# !! Describe file purpose !!

NAME <- "{{filename}}"

# ------------
# Sources
# ------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(char = {standard_packages})
{ifelse(is.null(github_packages), "", "pacman::p_load_gh(char = ")}
{ifelse(is.null(github_packages), "", github_packages}
{ifelse(is.null(github_packages), "", ")"}


source(here("1_tools/management_functions.R"))

#Set up pipeline folder if missing
pipeline <- createPipelineDir(NAME)
datadir <- "0_data"
pipelinedir <- "3_{pipeline_name}"

#df <- read_!!!(here(datadir, "!!!"))
#df <- read_!!!(here(pipelinedir, {{previous_name}} , "!!!"))

notes <- character()


# ------------
# STEP 1
# ------------


# ------------
# Save outputs
# ------------

# write_rds(df, here({pipeline_folder}, "XXX.RDS"))

writeLines(notes, here({pipeline_folder}, "notes.txt"))
                  ')

management_functions_file <- ("
  createPipelineDir <- function (NAME) {
  pipeline <- here('3_pipeline', NAME)
  if (!dir.exists(pipeline)) {
    dir.create(pipeline)
}
  }
  stringr::str_replace(stringr::str_replace(pipeline, here(), ''), '^/', '')
}

                              ")

run_all_file <- ('
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here, purrr)

files <- list.files(here({ifelse(code_folder, "2_code", "")}), pattern = "\\.R$")[-1]

map(here(files), source)

notes <- character()

notes <- c(notes, "Last complete run:", timestamp())

writeLines(notes, here("last_complete_run.txt"))

                 ')
