ols_res=/home/rstudio/results/* | grep "OLS_"
echo $ols_res

for i in /home/rstudio/results/*;
do
  echo $i
  fn=$(basename $i)
  aws s3 cp $i s3://volpe-cat01/ols_all/$fn
done
