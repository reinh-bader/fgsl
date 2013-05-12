program version
  use fgsl
  write(6, fmt='(''FGSL version is '',a)') fgsl_version
  write(6, fmt='(''based on GSL version '',a)') fgsl_gslbase
end program version
