# snapshot: sender coloring strategy structure

    Code
      structure
    Output
      $trace_type
      [1] "sankey"
      
      $node_count
      [1] 32
      
      $node_labels
       [1] "Macrophage"  "Fibroblast"  "T_cell"      "Endothelial" "B_cell"     
       [6] "TGFB1"       "TNF"         "IL6"         "IFNG"        "VEGFA"      
      [11] "PDGFB"       "IL2"         "IL4"         "CCL2"        "CXCL12"     
      [16] "IL10"        "TGFBR1"      "TNFRSF1A"    "IL6R"        "IFNGR1"     
      [21] "KDR"         "PDGFRB"      "IL2RA"       "IL4R"        "CCR2"       
      [26] "CXCR4"       "IL10R"       "T_cell"      "Endothelial" "Fibroblast" 
      [31] "Macrophage"  "B_cell"     
      attr(,"apiSrc")
      [1] TRUE
      
      $link_count
      [1] 45
      
      $layout_title
      NULL
      
      $layout_font_family
      [1] "Arial"
      
      $layout_font_size
      [1] 12
      

# snapshot: ligand coloring strategy structure

    Code
      structure
    Output
      $trace_type
      [1] "sankey"
      
      $node_count
      [1] 32
      
      $node_labels
       [1] "Macrophage"  "Fibroblast"  "T_cell"      "Endothelial" "B_cell"     
       [6] "TGFB1"       "TNF"         "IL6"         "IFNG"        "VEGFA"      
      [11] "PDGFB"       "IL2"         "IL4"         "CCL2"        "CXCL12"     
      [16] "IL10"        "TGFBR1"      "TNFRSF1A"    "IL6R"        "IFNGR1"     
      [21] "KDR"         "PDGFRB"      "IL2RA"       "IL4R"        "CCR2"       
      [26] "CXCR4"       "IL10R"       "T_cell"      "Endothelial" "Fibroblast" 
      [31] "Macrophage"  "B_cell"     
      attr(,"apiSrc")
      [1] TRUE
      
      $link_count
      [1] 45
      
      $layout_title
      NULL
      
      $layout_font_family
      [1] "Arial"
      
      $layout_font_size
      [1] 12
      

# snapshot: group coloring strategy structure

    Code
      structure
    Output
      $trace_type
      [1] "sankey"
      
      $node_count
      [1] 25
      
      $node_labels
       [1] "Macrophage"  "T_cell"      "Fibroblast"  "Endothelial" "TGFB1"      
       [6] "TNF"         "IFNG"        "VEGFA"       "IL2"         "PDGFB"      
      [11] "IL6"         "CCL2"        "TGFBR1"      "TNFRSF1A"    "IFNGR1"     
      [16] "KDR"         "IL2RA"       "PDGFRB"      "IL6R"        "CCR2"       
      [21] "T_cell"      "Fibroblast"  "Macrophage"  "Endothelial" "B_cell"     
      attr(,"apiSrc")
      [1] TRUE
      
      $link_count
      [1] 81
      
      $layout_title
      NULL
      
      $layout_font_family
      [1] "Arial"
      
      $layout_font_size
      [1] 12
      

# snapshot: condition coloring strategy structure

    Code
      structure
    Output
      $trace_type
      [1] "sankey"
      
      $node_count
      [1] 32
      
      $node_labels
       [1] "Macrophage"  "Fibroblast"  "T_cell"      "Endothelial" "B_cell"     
       [6] "TGFB1"       "TNF"         "IL6"         "IFNG"        "VEGFA"      
      [11] "PDGFB"       "IL2"         "IL4"         "CCL2"        "CXCL12"     
      [16] "IL10"        "TGFBR1"      "TNFRSF1A"    "IL6R"        "IFNGR1"     
      [21] "KDR"         "PDGFRB"      "IL2RA"       "IL4R"        "CCR2"       
      [26] "CXCR4"       "IL10R"       "T_cell"      "Endothelial" "Fibroblast" 
      [31] "Macrophage"  "B_cell"     
      attr(,"apiSrc")
      [1] TRUE
      
      $link_count
      [1] 45
      
      $layout_title
      NULL
      
      $layout_font_family
      [1] "Arial"
      
      $layout_font_size
      [1] 12
      

# snapshot: node colors are consistent

    Code
      node_colors
    Output
       [1] "#E41A1C" "#4DAF4A" "#377EB8" "#984EA3" "#FF7F00" "#E41A1C" "#E41A1C"
       [8] "#E41A1C" "#377EB8" "#4DAF4A" "#984EA3" "#377EB8" "#FF7F00" "#E41A1C"
      [15] "#984EA3" "#FF7F00" "#4DAF4A" "#377EB8" "#E41A1C" "#377EB8" "#4DAF4A"
      [22] "#984EA3" "#377EB8" "#FF7F00" "#E41A1C" "#984EA3" "#FF7F00" "#377EB8"
      [29] "#984EA3" "#4DAF4A" "#E41A1C" "#FF7F00"
      attr(,"apiSrc")
      [1] TRUE

# snapshot: link colors with sender strategy

    Code
      length(link_colors)
    Output
      [1] 45

# snapshot: custom margins with title

    Code
      structure
    Output
      $trace_type
      [1] "sankey"
      
      $node_count
      [1] 32
      
      $node_labels
       [1] "Macrophage"  "Fibroblast"  "T_cell"      "Endothelial" "B_cell"     
       [6] "TGFB1"       "TNF"         "IL6"         "IFNG"        "VEGFA"      
      [11] "PDGFB"       "IL2"         "IL4"         "CCL2"        "CXCL12"     
      [16] "IL10"        "TGFBR1"      "TNFRSF1A"    "IL6R"        "IFNGR1"     
      [21] "KDR"         "PDGFRB"      "IL2RA"       "IL4R"        "CCR2"       
      [26] "CXCR4"       "IL10R"       "T_cell"      "Endothelial" "Fibroblast" 
      [31] "Macrophage"  "B_cell"     
      attr(,"apiSrc")
      [1] TRUE
      
      $link_count
      [1] 45
      
      $layout_title
      [1] "Presentation Slide"
      
      $layout_font_family
      [1] "Arial"
      
      $layout_font_size
      [1] 16
      

