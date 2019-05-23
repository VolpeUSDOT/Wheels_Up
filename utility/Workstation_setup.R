# Set up workstation on EC2

# # Sync with S3
# if(length(system('aws s3 ls', intern = T)) < 1) {
#   stop('Configure AWS credential on the terminal with `aws configure` first')
# }
# 
# system('aws s3 sync s3://volpe-cat01 .')

# Install packages if necessary
local_packs <- '~/R_packs_to_install'
#system(paste('unzip ~/Wheels_Up_R_Packs.zip -d', local_packs))

install_these <- dir(local_packs)

for(pack in install_these){
  pack_name <- unlist(lapply(strsplit(pack, "_"), function(x) x[[1]]))
  if(length(grep(pack_name, (.packages(all.available=T))))==0){
    install.packages(file.path(local_packs, pack), repos = NULL)
  }
}

# manual steps for several which need specific order of loading

# Test setup with test_stan.R