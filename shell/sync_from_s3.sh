# aws s3 cp s3://volpe-cat01/data/ /home/rstudio/
#aws s3 cp s3://volpe-cat01/Wheels_Up_R_Packs.zip /home/rstudio/
# aws s3 cp s3://volpe-cat01/Wheels_Up_Code.zip /home/rstudio/
aws s3 cp s3://volpe-cat01/data.zip ~/.
aws s3 cp s3://volpe-cat01/Wheels_Up_R_Packs.zip ~/.
aws s3 cp s3://volpe-cat01/Wheels_Up_Code.zip ~/.

# Copy to rstudio user

sudo cp -a ~/data.zip /home/rstudio/.
sudo cp -a ~/Wheels_Up_R_Packs.zip /home/rstudio/.
sudo cp -a ~/Wheels_Up_Code.zip /home/rstudio/.

# Unzip
sudo unzip -j /home/rstudio/Wheels_Up_R_Packs.zip -d /home/rstudio/R_packs_to_install
sudo unzip /home/rstudio/Wheels_Up_Code.zip -d /home/rstudio/Wheels_Up
sudo unzip /home/rstudio/data.zip -d /home/rstudio/data

sudo chown -R rstudio /home/rstudio/.
