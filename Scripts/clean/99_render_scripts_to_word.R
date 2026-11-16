# =============================================================================
# 99_render_scripts_to_word.R
# -----------------------------------------------------------------------------
# PURPOSE  Turn every clean script into a Word document (code + output +
#          figures) so the analysis can be read without running R.
#          Each script is rendered in its own fresh R session (callr), the
#          same way as running it after a restart.
#
# NEEDS    packages rmarkdown and callr
#          Scripts/clean/print_template.docx   (Word styles for the print-outs)
# OUTPUT   Scripts/clean/printouts/<script name>.docx
#
# RUN      after 00_run_all.R (all data must already exist). Takes several
#          minutes. Not part of 00_run_all.R.
# =============================================================================

script_dir <- "Scripts/clean"

scripts <- c("01_data_cleaning.R",
             "02_mode_shares_RQ1.R",
             "02b_od_cbd_flows.R",
             "03_spatial_clustering_RQ1.R",
             "04a_pt_times_r5r.R",
             # "04b_car_times_traveltime_api.R",   # not used (licensing) -> don't re-run API
             "04c_car_freeflow_check.R",
             "05_travel_times.R",
             "06_predictors_RQ2.R",
             "07_ols_gwr_RQ2.R",
             "07.1_GWR_CoefMaps.R",
             "08_rq3_charge_burden.R")


# Any scripts in the folder that aren't in the list?
setdiff(list.files(script_dir, pattern = "\\.R$"), scripts)

for (s in scripts) {
  message("Rendering ", s)
  callr::r(  
    function(file, ref, root, out) {
      rmarkdown::render(file,
                        output_format = rmarkdown::word_document(reference_docx = ref),
                        knit_root_dir = root,
                        output_dir    = out)
    },
    args = list(file = normalizePath(file.path(script_dir, s)),
                ref  = normalizePath(file.path(script_dir, "print_template.docx")),
                root = getwd(),
                out  = normalizePath(file.path(script_dir, "printouts"))),
    show = TRUE)
}
