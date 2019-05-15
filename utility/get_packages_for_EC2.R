# Get all necessary packages across data prep and analysis scripts for Wheels Up project

# Here loading the packages as a .zip of all packages to load into the user library on the EC2 instance, in /usr/lib/R/library.
# Log on to the EC2 instance and check sessioninfo() to make sure versions match between Windows and Linux machine
# Depends on having AWS credentials stored locally in '~/keys'


libarary(aws.s3)

key_loc = '~/keys'
key_file = 'accessKeysDF.csv'
destdir = '~/R_packs_for_Wheels'
bucket = 's3://volpe_aws_CAT_01'

key_info <- read.csv(file.path(key_loc, key_file))
Sys.setenv("AWS_ACCESS_KEY_ID" = key_info$Access.key.ID,
           "AWS_SECRET_ACCESS_KEY" = key_info$Secret.access.key,
           "AWS_DEFAULT_REGION" = "us-east-1",
           "AWS_SESSION_TOKEN" = "wheelsup")

if(!dir.exists(destdir)) {dir.create(destdir)}

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
		        "rstan",
            "rgdal",  # for readOGR(", , needed for reading in ArcM shapefiles
            "rgeos",  # for gIntersection, to clip two shapefiles
            "sjPlot",
	         	"sp",
            "tidyverse",
            "utils",
		        'xgboost')

getDependencies <- function(packs){
  dependencyNames <- unlist(
    tools::package_dependencies(packages = packs, db = available.packages(), 
                                which = c("Depends", "Imports"),
                                recursive = TRUE))
  packageNames <- union(packs, dependencyNames)
  packageNames
}

for(i in loadpacks){
  # Calculate dependencies
  i_plus_dep <- getDependencies(i)

  already_have <- unlist(lapply(strsplit(dir(destdir), "_"), function(x) x[[1]]))
  need_to_get <- i_plus_dep[!i_plus_dep %in% already_have]
  
  download.packages(pkgs = need_to_get, 
                   destdir = destdir,
                   type = 'source',
                   )
}

# Load to S3 ----

# save the packages all together and upload to S3
fn = "Wheels_Up_R_Packs.zip"

if(!file.exists(file.path(destdir, fn))){
  system(paste('zip -r', path.expand(file.path(destdir, fn)),
             path.expand(file.path(destdir, '.'))
             ))
}

aws.s3::bucket_exists(bucket = bucket)
