sf_use_s2(FALSE)

a = function(x){
  row <- geoepr.ag[x,]
  cs.polys <- cs %>% 
    filter(lengths(st_intersects(., row))>0,
           int_overlaps(dateiv, row$dateiv))
  return(st_intersection(row, cs.polys))
}

a(1)
