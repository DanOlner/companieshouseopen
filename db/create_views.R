# Create / refresh the clean-layer views (Phase 5).
# Views are always-live over the raw tables, so just re-run anytime.
# Run from the project root:  Rscript db/create_views.R
source("db/connect.R")

con <- ch_connect()
ch_init_views(con)

cat("\nViews now defined in", CH_DB_PATH, ":\n")
print(dbGetQuery(con, "SELECT table_name FROM information_schema.tables
                       WHERE table_type = 'VIEW' ORDER BY table_name"))

ch_disconnect(con)
