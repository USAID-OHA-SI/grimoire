library(tidyverse)
library(gophr)
library(glamr)
library(glitr)

merdata <- si_path("path_msd")
msd <- return_latest(folderpath = merdata, pattern = "OU_IM")
df <- read_msd(msd)
