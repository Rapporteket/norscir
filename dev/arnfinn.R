devtools::install("../rapbase/.", upgrade = FALSE)
devtools::install(upgrade = FALSE)

# dekoding av database-dump
# sship::dec("c://Users/ast046/Downloads/norscir161e2af77b79.sql.gz__20250218_153829.tar.gz", keyfile = "p://.ssh/id_rsa", target_dir = "c://Users/ast046/Downloads/.")

Sys.setlocale(locale = 'nb_NO.UTF-8')
source("dev/sysSetenv.R")

Sys.setenv(MYSQL_HOST="localhost") # for mobilt kontor
Sys.setenv(R_RAP_CONFIG_PATH="C:/Users/ast046/repo/rapporteket/rygg/dev/config")
Sys.setenv(MYSQL_DB_DATA="norscirreportdatastaging")

norscir::kjorApp(browser = TRUE)
