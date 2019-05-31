
# 1. zip
sudo zip -r /home/rstudio/results.zip /home/rstudio/results
# sudo zip -r /home/rstudio/data.zip /home/rstudio/data
sudo zip -r /home/rstudio/Wheels_Up_Code_Update.zip /home/rstudio/Wheels_Up

# 2. copy to root from rstudio user
# use -p flag to make directories only the first time
mkdir -p ~/to_s3/

sudo cp -a /home/rstudio/results.zip ~/to_s3/.
#sudo cp -a /home/rstudio/data.zip ~/to_s3/.
sudo cp -a /home/rstudio/Wheels_Up_Code_Update.zip ~/to_s3/.

# 3. copy to bucket
aws s3 cp ~/to_s3/results.zip s3://volpe-cat01/to_s3/results.zip
#aws s3 cp ~/to_s3/data.zip s3://volpe-cat01/to_s3/data.zip
aws s3 cp ~/to_s3/Wheels_Up_Code_Update.zip s3://volpe-cat01/to_s3/Wheels_Up_Code_Update.zip