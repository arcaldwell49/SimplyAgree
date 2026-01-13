# Ensure the mess folder exists
if (!dir.exists("mess")) dir.create("mess")

# Run check and capture output
check_results <- devtools::check(
  document = FALSE,
  check_dir = "mess"
)

# Ensure the mess folder exists
if (!dir.exists("mess")) dir.create("mess")

# Capture output to a log file
log_file <- file.path("mess", paste0("check_log_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt"))

sink(log_file, split = TRUE)
check_results <- devtools::check(document = FALSE)
sink()
