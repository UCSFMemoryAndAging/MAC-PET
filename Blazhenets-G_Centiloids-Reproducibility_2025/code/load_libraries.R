#Function to check if package is installed and load to current session

check_install <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

check_install('ggplot2')
check_install('mixtools')
check_install('cluster')
check_install('readxl')
check_install('dplyr')
check_install('lubridate')
check_install('gridExtra')
check_install('grid')
check_install('data.table')
check_install('cutpointr')
check_install('cluster')
check_install('ggpubr')
check_install('kableExtra')
check_install('boot')
check_install('pROC')
check_install('car')
check_install('tidyr')
check_install('MASS')
