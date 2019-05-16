# Set up workstation on EC2

# Sync with S3
if(length(system('aws s3 ls', intern = T)) < 1) {
  stop('Configure AWS credential on the terminal with `aws configure` first')
}

system('aws s3 synch s3://volpe-cat01 .')

# Install packages if necessary
local_packs <- '~/R_packs_to_install'
system(paste('unzip ~/Wheels_Up_R_Packs.zip -d', local_packs))

install_these <- dir(local_packs)

for(pack in install_these){
  install.packages(file.path(local_packs, pack), repos = NULL)
}

# Test setup 

source()