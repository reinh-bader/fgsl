  function fgsl_function_init(func, params) 
    procedure(function) :: func
    type(c_ptr), intent(in) :: params
    type(fgsl_function) :: fgsl_function_init
  end function fgsl_function_init
