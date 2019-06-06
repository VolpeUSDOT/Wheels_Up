# Grab update results and code from S3.

library(aws.s3)

code_loc = '~/git/Wheels_Up'

key_loc = '~/keys'
key_file = 'accessKeysDF.csv'
bucket = 'volpe-cat01'
sharedloc = '//vntscex.local/DFS/Projects/PROJ-OR02A2/SDI/BTS_Flight_performance'

key_info <- read.csv(file.path(key_loc, key_file))
Sys.setenv("AWS_ACCESS_KEY_ID" = as.character(key_info$Access.key.ID),
           "AWS_SECRET_ACCESS_KEY" = as.character(key_info$Secret.access.key),
           "AWS_DEFAULT_REGION" = "us-east-1")

# Both options work
# system('aws s3 ls')
# bucketlist()

# Get from S3 ----

sync_dir <- paste0('Sync_', Sys.Date())
dir.create(file.path(sharedloc, 'Sync', sync_dir))

# Get the latest date sync
bucket_dirs = system(paste('aws s3 ls', paste0('s3://', bucket, '/')), intern = T)
spl = strsplit(bucket_dirs[grep('PRE', bucket_dirs)], " +")
dirs = unlist(lapply(spl, function(x) x[[3]]))
dirs = sub("\\/", "", dirs)
sync_from_EC2_dirs = dirs[grep("\\d{4}-\\d{2}-\\d{2}", dirs)]
latest_sync_from_EC2 = sync_from_EC2_dirs[which(as.Date(sync_from_EC2_dirs) == max(as.Date(sync_from_EC2_dirs)))] 

system(paste(
  'aws s3 cp', paste0('s3://', bucket, '/', latest_sync_from_EC2, '/Wheels_Up_Code_Update.zip'),
  file.path(sharedloc, 'Sync', sync_dir, 'Wheels_Up_Code.zip')
))

system(paste(
  'aws s3 cp', paste0('s3://', bucket,  '/', latest_sync_from_EC2, '/results.zip'),
  file.path(sharedloc, 'Sync', sync_dir, 'results.zip')
))

###


system(paste(
  'aws s3 cp', paste0('s3://', bucket,  '/', latest_sync_from_EC2, '/Data.zip'),
  file.path(sharedloc, 'Sync', sync_dir, 'Data.zip')
  ))



# Get OLS summary results -- summary csv first
olsget <- function(Analysis, summary_file_namepart = 'Validate_Internal.csv'){
  OLS_res = system(paste(
    'aws s3 ls', paste0('s3://', bucket, '/', Analysis,'/')), intern = T)
  
  spl = strsplit(OLS_res, " +")
  sizes = unlist(lapply(spl, function(x) x[[3]]))
  files = unlist(lapply(spl, function(x) x[[4]]))
  
  sum_files = files[grep(summary_file_namepart, files)]
  
  for(s in sum_files){
    system(
      paste(
        'aws s3 cp', paste0('s3://', bucket, '/', Analysis,'/', s),
        file.path(sharedloc, 'Sync', sync_dir, s))
    )
    
  }
}



olsget('OLS_Crossyear_1O_D')

olsget('OLS_Crossyear_1Carrier_Validate_Internal')

olsget('OLS_Crossyear_1Carrier_Validate_2019', 'Validate_2019.csv')

olsget('OLS_Crossyear_1O-D_Validate_Internal', 'Validate_Internal.csv')

olsget('OLS_Crossyear_1O-D_Validate_2019', 'Validate_2019.csv')
