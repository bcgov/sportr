library(ggplot2)
require(ccissr)
require(tidyverse)
species_cols <- ccissr::TreeCols |> mutate(Species = as.factor(TreeCode))

  species_cols <- c(
    
    # Dry Temperate
    # Dry Temperate
    Fd  = "#FFD54F",  # Douglas-fir (golden yellow)
    Fdc = "#E8A317",  # coastal Douglas-fir
    Fdi = "#F6C141",  # interior Douglas-fir
    Lw =  "#D4E157", # balanced yellow-green
    Py  = "#FFD166",
    Bg  = "#AFCF5A",
    
    # Boreal
    Pl  = "#00A6A6",
    Plc = "#008585",
    Pli = "#00A6A6",
    Pj  = "#66CCCC" , # jack pine

    Sx  = "#3182BD",
    Sw  = "#6BAED6",
    Se  = "#2171B5",
    Ss  = "#08519C",
    Sxs = "#4292C6",
    Sb  = "#08306B",
    
    # ESSF affiliates
    Bl = "#8073AC",   # dark purple
    Pa = "#B39DDB",  # light purple
    
    # MH affiliates
    Ba = "#6A51A3",  # amabilis fir
    Bp = "#9E9AC8",  # paper birch? (or your Bp code)
    Hm = "#88419D",  # mountain hemlock
    Yc = "#542788" ,  # yellow cedar

    
    # CWH/ICH
    Cw  = "#006D2C",
    Hw  = "#31A354",


    # Interior Broadleafs
    At = "#E6D5B8",  # aspen
    Ep = "#DCC9A8",  # cottonwood
    Ea = "#CBBBAF",  # birch
    
    # Alders
    Ac  = "#C89F72",
    Act = "#D6BC8C",
    Acb = "#A98274",
    
    # Coastal Broadleafs
    Mb = "#7A5C52",  # bigleaf maple
    Ra = "#5E3C2B",  # arbutus
    Qg = "#A67C52",  # garry oak
    Dr = "#8B6F63",   # red alder
    
    
    # Other
    X   = "#999999"
  ) |> as.data.frame() |> rownames_to_column("TreeCode") |> rename("HexColour" = 2)
fwrite(species_cols, "./new_species_colours.csv")

ggplot(species_cols,
       aes(x = TreeCode, y = 1, fill = HexColour)) +
  geom_tile() +
  scale_fill_identity() +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  )
