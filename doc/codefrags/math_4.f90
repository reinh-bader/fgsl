  real(c_double) function function_df(x, params) bind(c)
    real(c_double), value :: x
    type(c_ptr), value :: params
  end function function_df
