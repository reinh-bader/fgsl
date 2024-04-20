!-*-f90-*-
module fgsl_sf_elljac
  !>  Special functions - Elliptic functions
  use fgsl_sf_types
  implicit none

  interface
     function fgsl_sf_elljac_e(u, m, sn, cn, dn) bind(c, name='gsl_sf_elljac_e')
       import
       real(c_double), value :: u, m
       real(c_double) :: sn, cn, dn
       integer(c_int) :: fgsl_sf_elljac_e
     end function fgsl_sf_elljac_e
  end interface
end module fgsl_sf_elljac
