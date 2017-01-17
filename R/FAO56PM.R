# This is a penman monteith calc function, it needs some work still
# test variables
# Thr = 38
# RHhr = 52
# Rn = 1.749
# G = 0.1*Rn
# u2 = 3.33





PET.hourly = function(Thr = -9999, RHhr = -9999,Rn = -9999, G=-9999, u2 = -9999){
     stop('DO NOT USE THIS FUNCTION YET, it needs to be checked and debugged, it is in development')
     if(Thr < -999){stop('Error no Thr specified')}
     if(RHhr < -999){stop('Error no RHhr specified')}
     if(Rn < -999){stop('Error no Rn specified')}
     if(u2 < -999){stop('Error no u2 specified')}

     #constants
     cp = 1.013*10^-3 #spec. heat at constant pressure [MJ kg-1 degC-1]
     P = 101.325  #atm pressure [kPa] at sea level
     lam = 2.45 #lambda latent heat of vaporization [MJ kg-1]
     eps = 0.622 #ratio of molecular weight of water vapor/dry air
     gamma =   cp*P/(eps*lam)

     eo           =   0.6108*exp((17.27*Thr)/(Thr+237.3))
     ea           =    eo*RHhr/100

     # delta = slope of vapor pressure curve at air T(degC) Thr [kPa degC^-1]
     delta =   (4098*(0.6108*exp(17.27*Thr/(Thr+237.3))))/(Thr+237.3)^2

     delta.netrad =    0.408*delta*(Rn-G)

     gamma.masstx =    gamma*(37/(Thr+273))*u2*(eo-ea)

     PET.hr       = (delta.netrad+gamma.masstx)/(delta+gamma*(1+0.34*u2))

     return(PET.hr)
}
