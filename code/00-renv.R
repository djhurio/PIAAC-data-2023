getOption("repos")

library(renv)

options()[grep("renv", names(options()))]

options(renv.config.ppm.enabled = TRUE)
options(renv.config.pak.enabled = TRUE)
options(renv.config.ppm.default = TRUE)

status()
