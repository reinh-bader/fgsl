  real(c_double) function function(x, params) bind(c)
    real(c_double), value :: x
    type(c_ptr), value :: params
  end function func
