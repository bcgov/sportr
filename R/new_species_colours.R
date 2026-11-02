library(ggplot2)
require(ccissr)
require(tidyverse)
species_cols <- ccissr::TreeCols |> mutate(Species = as.factor(TreeCode))

  species_cols <- c(
    
    # Dry Temperate
    Fd  = "#F28E2B",
    Fdc = "#E17C05",
    Fdi = "#C76A00",
    Lw  = "#A6611A",
    Py  = "#FDB863",
    Bg =  "#A6D96A",
   
    
    # Boreal
    Pl  = "#E7298A",  # lodgepole pine
    Plc = "#CE1256",  # coastal lodgepole pine
    Pli = "#F768A1",  # interior lodgepole pine
    Pj  = "#FBB4D8",  # jack pine

    Sx  = "#3182BD",
    Sw  = "#6BAED6",
    Se  = "#2171B5",
    Ss  = "#08519C",
    Sxs = "#4292C6",
    Sb  = "#08306B",
    
    # ESSF affiliates
    Bl = "#88419D",  # dark purple
    Pa = "#B39DDB",  # light purple
    
    # MH affiliates
    Ba = "#6A51A3",  # amabilis fir
    Bp = "#9E9AC8",  # paper birch? (or your Bp code)
    Hm = "#8073AC",  # mountain hemlock
    Yc = "#542788" ,  # yellow cedar
    
    # CWH/ICH
    Cw  = "#006D2C",
    Hw  = "#31A354",


    # Interior or Provincial Broadleaf Species
    # Interior broadleafs
    At = "#D8C3A5",
    Ep = "#CDB79E",
    Ea = "#BCAAA4",
    
    # Alders
    Ac  = "#B08968",
    Act = "#C2A878",
    Acb = "#8D6E63",
    # Coastal broadleafs
    Mb = "#8C6D62",
    Ra = "#7F5539",
    Qg = "#D6C6A5",
    Dr = "#A1887F",
    
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
