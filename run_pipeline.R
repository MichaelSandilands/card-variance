# Entry point for scheduled runs (GitHub Actions, Posit Connect, cron).
# Runs the pipeline, then writes a short Markdown summary of the outcome.

targets::tar_make()

release <- targets::tar_read(release)
writeLines(release$summary_md, "pipeline_summary.md")
cat(release$summary_md, "\n")

# Exit non-zero when a stop-level check blocked a dataset, so the scheduler alerts you.
blocked <- vapply(release$datasets, function(e) !isTRUE(e$published), logical(1))
if (any(blocked)) {
  message("Blocked datasets: ", paste(names(blocked)[blocked], collapse = ", "))
  quit(status = 2)
}
