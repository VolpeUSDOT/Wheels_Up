# Get all necessary packages across data prep and analysis scripts for Wheels Up project

loadpacks <- c(
            "aws.s3", # AWS convenience functions s3save and s3load
            "brms",
		"doParallel",
            "DT",
            "foreach", # for parallel implementation
            "kableExtra",
		"lme4",
            "lubridate",
            "pander",
            "pROC",
            "randomForest",
            "raster", 
            "rgdal",  # for readOGR(", , needed for reading in ArcM shapefiles
            "rgeos",  # for gIntersection, to clip two shapefiles
            "sjPlot",
		"sp",
            "tidyverse",
            "utils")


for(i in loadpacks){if(length(grep(i, (.packages(all.available=T))))==0) install.packages(i, dependencies =TRUE)}

# brms is an interface to Stan, and requires Rtools to run 'make' statements, to compile C++ code for the Stan models.
# Get Rtools35.exe here: https://cran.r-project.org/bin/windows/Rtools/
# When installed, this demo code will work (takes a few minutes):
TEST_STAN = F

if(TEST_STAN){

  # Make sure Rtools is in the PATH
  
  pth <-  Sys.getenv('PATH')

  # if(!grepl('Rtools', pth)){
  #   Sys.setenv(PATH =  paste0("C:\\Rtools\\bin;C:\\Rtools\\mingw_64\\bin;", pth))
  # }

  # Using RBuildTools now
  if(!grepl('RBuildTools', pth)){
    Sys.setenv(PATH =  paste0("C:\\RBuildTools\\3.5\\bin;C:\\RBuildTools\\3.5\\mingw_64\\bin;", pth))
  }
  
  system('where make')
  system('where sh')
  system('where g++')
  Sys.which('g++')
  pkgbuild::find_rtools()
  pkgbuild::rtools_path()
  pkgbuild::has_rtools(debug = TRUE)
  pkgbuild::has_build_tools(debug = TRUE) # Stuck here. 
  
  
  system('g++ -v')
  
  Sys.getenv('BINPREF')
  Sys.getenv('BINPREF64') # Blank
  # Tried this, doesn't help Sys.setenv(BINPREF64 = "C:/RBuildTools/3.5/mingw_64/bin/")
  
  # https://github.com/stan-dev/rstan/wiki/Installing-RStan-from-source-on-Windows

  dotR <- file.path(Sys.getenv("HOME"), ".R")
  if (!file.exists(dotR)) dir.create(dotR)
  M <- file.path(dotR, "Makevars.win")
  if (!file.exists(M)) {
    file.create(M)
    cat("\nCXX14FLAGS=-O3 -march=native",
      "CXX14=$(BINPREF)g++ -O2 -march=native -mtune=native",
      "CXX11FLAGS=-O3 -march=native",
      file = M, sep = "\n", append = TRUE)
  }
  system(paste('open', M))
  
  # cat('Sys.setenv(BINPREF = "C:/RBuildTools/3.5/mingw_$(WIN)/bin/")',
  #     file = file.path(Sys.getenv("HOME"), ".Rprofile"),
  #     sep = "\n", append = TRUE)

  # From https://github.com/eddelbuettel/inline/pull/7
  #Sys.setenv(BINPREF = "C:/RBuildTools/3.5/mingw_64/bin/")
  
  fx <-
       inline::cxxfunction(
               signature(x = "integer", y = "numeric") ,
               'return ScalarReal( INTEGER(x)[0] * REAL(y)[0] ) ;')
  
  
  
  remove.packages("rstan")
  if (file.exists(".RData")) file.remove(".RData")
  Sys.setenv(MAKEFLAGS = "-j4") # four cores used
  
  
  install.packages("rstan", type = "source")
  # WARNING: Rtools is required to build R packages but no version of Rtools compatible with the currently running version of R was found. Note that the following incompatible version(s) of Rtools were found:
  # - Rtools 3.4 (installed at c:\Rtools)

  library(brms)
  bprior1 <- prior(student_t(5,0,10), class = b) + 
		prior(cauchy(0,2), class = sd)
	fit1 <- brm(count ~ zAge + zBase * Trt + (1|patient),
		data = epilepsy, family = poisson(), prior = bprior1)
	
	summary(fit1)
	shinystan::launch_shinystan(fit1)
}
