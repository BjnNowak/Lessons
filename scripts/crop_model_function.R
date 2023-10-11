# Outside the function:
# Required parameters to compute C
# Light extinction coefficient
K <- 0.56
# Individual leaf area (m-2)
S <- 0.05
# Plant density (m-2)
d <- 90000/10000

# Model function
model_fun <- function(
    name,           # Scenario name 
    data,           # Climatic variables to be used as inputs
    GDD_1leaf,      # Thermal requirement for the emergence of one leaf
    C,              # C=f(K,S,d)
    RUE,            # Radiation use efficiency (gDM.MJ-1)
    nthresh         # Number of leaves before grain filling
){      
  # Set parameters (without GDD_1leaf)
  max_nleaf<-20
  T0<-6 
  f<-0.5      # Active fraction of incoming radiation
  frac<-0.7   # Fraction of Net Primary Productivity dedicated to grain
  
  # Estimate yield
  model<-data%>%
    dplyr::mutate(
      TT=dplyr::case_when(
        tas<T0~0,
        tas>=T0~tas-T0
      ))%>%  
    mutate(
      GDD = cumsum(TT)
    )%>%
    mutate(
      pot_nleaf = GDD/GDD_1leaf
    )%>%
    mutate(
      nleaf = case_when(
        pot_nleaf<=max_nleaf~round(pot_nleaf),
        pot_nleaf>max_nleaf~max_nleaf
      )
    )%>%
    # Incoming photosynthetic active radiation (MJ.m-2.day-1)
    mutate(
      PAR_inc = f*rsds
    )%>%
    # Absorbed PAR by the canopy (MJ.m-2.day-1)
    mutate(
      APAR = PAR_inc*(1-exp(-C*nleaf))
    )%>%
    # Net primary productivity dedicated to the aboveground biomass 
    mutate(
      NPP = RUE*APAR
    )%>%
    # Sum of aboveground biomass
    mutate(
      biom = cumsum(NPP)
    )%>%
    # Net primary productivity dedicated to the variable grain
    mutate(
      NPPgrain = case_when(
        nleaf<nthresh ~ 0,
        nleaf>=nthresh ~ frac*NPP
      )
    )%>%
    # Total grain production (g.m-2)
    mutate(
      grain = cumsum(NPPgrain)
    )%>%
    # Total grain production (t.ha-1)
    mutate(
      grain_t = grain/100
    )%>%
    add_column(                                # To add scenario name to data
      Scenario = name                          # (set 'name' in argument)
    )
  return(model)
}