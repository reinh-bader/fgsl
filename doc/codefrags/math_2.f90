  function fgsl_function_fdf_init(f, df, fdf, params)
    procedure(function) :: f
    procedure(function_df) :: df
    procedure(function_fdf) :: df
    type(c_ptr), intent(in) :: params
    type(fgsl_function_fdf) :: fgsl_function_fdf_init
  end function fgsl_function_fdf_init
