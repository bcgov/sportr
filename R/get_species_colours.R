
bc_species_colours <- function() {
  
  cols <- c(
    
    # Pines
    Pl = "#1B9E77",   # Lodgepole pine
    Pw = "#2E8B57",   # Whitebark pine
    Py = "#66A61E",   # Western white pine
    Pj = "#A6D854",   # Jack pine
    Pa = "#4DAF4A",   # Ponderosa pine
    
    # Spruces
    Sx = "#2166AC",   # Interior spruce
    Sw = "#4393C3",   # White spruce
    Ss = "#67A9CF",   # Sitka spruce
    Sb = "#053061",   # Black spruce
    
    # Firs
    Bl = "#762A83",   # Subalpine fir
    Ba = "#9970AB",   # Amabilis fir
    
    # Douglas-fir
    Fd = "#D95F02",
    
    # Hemlocks
    Hw = "#8C510A",   # Western hemlock
    Hm = "#BF812D",   # Mountain hemlock
    
    # Cedars
    Cw = "#E6AB02",   # Western redcedar
    
    # Larches
    Lw = "#FFD92F",   # Western larch
    
    # Poplars / Aspen
    At = "#A6CEE3",   # Trembling aspen
    Ep = "#B2DF8A",   # Cottonwood
    Ac = "#33A02C",   # Red alder
    
    # Birch
    Dr = "#FB9A99",
    
    # Maples
    Mb = "#E31A1C",
    
    # Yew
    Yc = "#6A3D9A",
    
    # Broadleaf mixed / unknown
    X  = "#999999"
  )
  
  cols
}
###################################

get_species_colours <- function(species) {
  
  fixed <- bc_species_colours()
  
  missing <- setdiff(species, names(fixed))
  
  if(length(missing) > 0) {
    
    extra <- scales::hue_pal()(length(missing))
    names(extra) <- missing
    
    fixed <- c(fixed, extra)
  }
  
  
  fixed[species]
}
