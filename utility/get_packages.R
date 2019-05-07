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
	bprior1 <- prior(student_t(5,0,10), class = b) + 
		prior(cauchy(0,2), class = sd)
	fit1 <- brm(count ~ zAge + zBase * Trt + (1|patient),
		data = epilepsy, family = poisson(), prior = bprior1)
	
	summary(fit1)
	shinystan::launch_shinystan(fit1)
}
