#init_dirs.R

dir.IN <- 'DOWNLOADS/' %T>% dir.create(showWarnings = F)
dir.IN.single <- paste0(dir.IN, "SinglePools/")  %T>% dir.create(showWarnings = F)

dir.TABLES <- 'TABLES/' %T>% dir.create(showWarnings = F)
dir.TABLES.single <-  paste0(dir.TABLES, "SinglePools/") %T>% dir.create(showWarnings = F)

dir.REF <- 'REFDATA/' %T>% dir.create(showWarnings = F)
dir.REF.single <-  paste0(dir.REF, "SinglePools/") %T>% dir.create(showWarnings = F)
