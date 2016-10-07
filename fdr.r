pvals <- sapply ( 1 : m , function ( i ){
  control <- sample ( population , 6 )
  treatment <- sample ( population , 6 )
  if ( ! nullHypothesis [ i ]) treatment <- treatment + delta
  t.test ( treatment , control ) $ p.value
})
sum(pvals < 0.05/10000)
