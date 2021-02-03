# calculate leaf growth fraction
# Question: assuming there's no storage but the grasses have a minimum LAI,
# how soon could they grow back to high LAI?

# assumption 1: SLA = 200cm2 g-1 ; this is from pace grass data
sla = 0.02 #m2 g-1
# assumption 2: daily photo max = 20 mumol-1 C m-2 leaf s-1 ; or 1 g C m-2 leaf hr-1
photo.max = 1
day.length = 10 #hours of day light

# calculation
# we first need daily gpp,for which I assume for a tranglar shape
day.gpp.func <- function(photo.max,day.length){day.length * photo.max *0.5}

# then we need to assume how much of gpp goes to leaf growth
fraction.leaf <- 1 # 0-1; 1 means 100% of gpp goes to leaf

# then we can calculate the growth fraction of LAI
fraction.lai <- function(fraction.leaf,photo.max,day.length){
  fraction.leaf * day.gpp.func(photo.max,day.length)
}

# assuming we know the initial lai (the minimum) we can esimate the LAI at a given day
lai.day.func <- function(fraction.leaf,photo.max,day.length,day.nm,sla){
  rate.in <- fraction.lai(fraction.leaf,photo.max,day.length) * sla
  lai.day <- (1 + rate.in)^day.nm
  return(lai.day)
}

# assuming an initial of 0.1 
day.nm <- 10
lai.10 <- 0.1 * lai.day.func(fraction.leaf,photo.max,day.length,day.nm,sla)

day.nm <- 20
lai.20 <- 0.1 * lai.day.func(fraction.leaf,photo.max,day.length,day.nm,sla)


lai.day.func(0.8,0.5,day.length,45,sla)


lai.day.func(0.8,0.5,day.length,30,sla)

lai.day.func(0.8,0.5,day.length,20,sla)
