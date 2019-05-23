# Get all necessary packages across data prep and analysis scripts for Wheels Up project

# Here loading the packages as a .zip of all packages to load into the user library on the EC2 instance, in /usr/lib/R/library.
# Log on to the EC2 instance and check sessioninfo() to make sure versions match between Windows and Linux machine
# Depends on having AWS credentials stored locally in '~/keys'
# Also depends on AWS CLI tools to be installed on the local machine, see https://docs.aws.amazon.com/cli/latest/userguide/cli-chap-install.html


library(aws.s3)

code_loc = '~/git/Wheels_Up'

key_loc = '~/keys'
key_file = 'accessKeysDF.csv'
destdir = '~/R_packs_for_Wheels'
bucket = 'volpe-cat01'

key_info <- read.csv(file.path(key_loc, key_file))
Sys.setenv("AWS_ACCESS_KEY_ID" = as.character(key_info$Access.key.ID),
           "AWS_SECRET_ACCESS_KEY" = as.character(key_info$Secret.access.key),
           "AWS_DEFAULT_REGION" = "us-east-1")

# Both options work
system('aws s3 ls')
bucketlist()

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
  
  # Suppress warnings about base packages not in repositories, like utils, stats, graphics
  suppressWarnings( 
    download.packages(pkgs = need_to_get, 
                   destdir = destdir,
                   type = 'source',
                   )
    )
}

# Load to S3 ----

# save the packages all together and upload to S3
fn = "Wheels_Up_R_Packs.zip"

OVERWRITE = T

if(!file.exists(file.path(destdir, fn)) | OVERWRITE){
  system(paste('zip -r -j', path.expand(file.path(destdir, fn)),
             path.expand(file.path(destdir, '.'))
             ))
}

system(paste(
  'aws s3api put-object --bucket', bucket,
  '--key', fn,
  '--body', 
       path.expand(file.path(destdir, fn))
       )
  )


# Bundle code and send to S3 ----
fn = "Wheels_Up_Code.zip"

system(paste('zip -r', 
             path.expand(file.path('~', fn)),
             path.expand(file.path(code_loc))
             ))


system(paste(
  'aws s3api put-object --bucket', bucket,
  '--key', fn,
  '--body', 
  path.expand(file.path('~', fn))
)
)
