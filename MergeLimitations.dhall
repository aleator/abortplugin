λ(lim1 : ./Limitations.type) →
λ(lim2 : ./Limitations.type) → 
 { extensions = lim1.extensions # lim2.extensions
 , imports    = lim1.imports    # lim2.imports }

