program cdf
  use fgsl
  real(fgsl_double) :: x, p, q
!
  x = 2.0_fgsl_double
  p = fgsl_cdf_ugaussian_p(x)
  write(6, '(''Prob (x < '',F5.2,'') = '',F15.12)') x, p
  q = fgsl_cdf_ugaussian_q(x)
  write(6, '(''Prob (x > '',F5.2,'') = '',F15.12)') x, q
  x = fgsl_cdf_ugaussian_pinv(p)
  write(6, '(''Pinv ('',F15.12,'') = '',F15.12)') p, x
  x = fgsl_cdf_ugaussian_qinv(q)
  write(6, '(''Qinv ('',F15.12,'') = '',F15.12)') q, x
end program cdf
