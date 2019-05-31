# complete="VX" "HA" "F9" "NK" "AS" "EV"
analysis="OLS_Crossyear_1Carrier"

for i in VX HA F9 NK AS EV;
do
  echo $i
  sumfile="/home/rstudio/results/${analysis}/${i}_Crossyear_OLS_Summary.csv"
  datfile="/home/rstudio/results/${analysis}/OLS_interax_${i}_Crossyear_Fitted.RData"
  echo $sumfile
  simplesum=$(basename $sumfile)
  simpledat=$(basename $datfile)
  echo$simplesum

  aws s3 cp $sumfile "s3://volpe-cat01/${analysis}/${simplesum}"
  aws s3 cp $datfile "s3://volpe-cat01/${analysis}/${simpledat}"

done
