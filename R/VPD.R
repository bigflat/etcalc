
#function to calculate vapor pressure deficit in Pa from Rh and temp (DegC)
VPD = function(RH,TC){

     SVP = 610.7*10^(7.5*TC/(237.3+TC)) #(Murray, 1967)
     vpd = ((100 - RH)/100)*SVP
     vpd #in Pa
}
