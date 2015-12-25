program specfunc
  use mod_unit
  use fgsl
  implicit none
  real(fgsl_double), parameter :: eps5 = 1.0E-5_fgsl_double
  real(fgsl_double), parameter :: eps7 = 1.0E-7_fgsl_double
  real(fgsl_double), parameter :: eps10 = 1.0E-10_fgsl_double
  real(fgsl_double), parameter :: eps15 = 1.0E-15_fgsl_double
  integer(fgsl_int) :: status, lmsize
  real(fgsl_double) :: ra, ra_2, ra_3, ra_exp, ra_exp_2, x11, x21, &
       ra_arr(1), ra_arr_der(1), ra_arr_2(1), ra_arr_2_der(1), &
       ra_arr_3(2), ra_arr_3_der(2)
  type(fgsl_sf_result) :: sfres, sfres_der, sfres_2, sfres_2_der
  type(fgsl_sf_result_e10) :: sfres10
!
! Test special functions
!
  call unit_init(800)
!
! Airy functions
!
  ra = fgsl_sf_airy_ai(0.0_fgsl_double, fgsl_prec_double)
  call unit_assert_equal_within('fgsl_sf_airy_ai',0.35502805d0,ra,eps7)
  status = fgsl_sf_airy_ai_e(0.0_fgsl_double, fgsl_prec_double, sfres)
  call unit_assert_equal('fgsl_sf_airy_ai_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_airy_ai_e',0.3550280538878172d0,&
       sfres%val,sfres%err)
  ra = fgsl_sf_airy_bi(0.0_fgsl_double, fgsl_prec_double)
  call unit_assert_equal_within('fgsl_sf_airy_bi',0.61492663d0,ra,eps7)
  status = fgsl_sf_airy_bi_e(0.0_fgsl_double, fgsl_prec_double, sfres)
  call unit_assert_equal('fgsl_sf_airy_bi_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_airy_bi_e',0.6149266274460007d0,&
       sfres%val,sfres%err)
  status = fgsl_sf_airy_bi_e(0.0_fgsl_double, fgsl_prec_single, sfres)
  call unit_assert_equal('fgsl_sf_airy_bi_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_airy_bi_e:single',&
       0.6149266274460007d0,sfres%val,sfres%err)
  status = fgsl_sf_airy_bi_e(0.0_fgsl_double, fgsl_prec_approx, sfres)
  call unit_assert_equal('fgsl_sf_airy_bi_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_airy_bi_e:approx',&
       0.6149266274460007d0,sfres%val,sfres%err)
  ra = fgsl_sf_airy_ai_scaled(0.0_fgsl_double, fgsl_prec_double)
  call unit_assert_equal_within('fgsl_sf_airy_ai_scaled',0.35502805d0,ra,eps7)
  status = fgsl_sf_airy_ai_scaled_e(0.0_fgsl_double, fgsl_prec_double, sfres)
  call unit_assert_equal('fgsl_sf_airy_ai_scaled_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_airy_ai_scaled_e',0.3550280538878172d0,&
       sfres%val,sfres%err)
  ra = fgsl_sf_airy_bi_scaled(0.0_fgsl_double, fgsl_prec_double)
  call unit_assert_equal_within('fgsl_sf_airy_bi_scaled',0.61492663d0,ra,eps7)
  status = fgsl_sf_airy_bi_scaled_e(0.0_fgsl_double, fgsl_prec_double, sfres)
  call unit_assert_equal('fgsl_sf_airy_bi_scaled_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_airy_bi_scaled_e',0.6149266274460007d0,&
       sfres%val,sfres%err)
  ra = fgsl_sf_airy_ai_deriv(0.0_fgsl_double, fgsl_prec_double)
  call unit_assert_equal_within('fgsl_sf_airy_ai_deriv',-0.25881940D0,ra,eps7)
  status = fgsl_sf_airy_ai_deriv_e(0.0_fgsl_double, fgsl_prec_double, sfres)
  call unit_assert_equal('fgsl_sf_airy_ai_deriv_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_airy_ai_deriv_e',-0.2588194037928068D0,&
       sfres%val,sfres%err)
  ra = fgsl_sf_airy_bi_deriv(0.0_fgsl_double, fgsl_prec_double)
  call unit_assert_equal_within('fgsl_sf_airy_bi_deriv',0.44828836d0,ra,eps7)
  status = fgsl_sf_airy_bi_deriv_e(0.0_fgsl_double, fgsl_prec_double, sfres)
  call unit_assert_equal('fgsl_sf_airy_bi_deriv_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_airy_bi_deriv_e',0.4482883573538263d0,&
       sfres%val,sfres%err)
  ra = fgsl_sf_airy_zero_ai(1)
  call unit_assert_equal_within('fgsl_sf_airy_zero_ai',-2.33810741d0,ra,eps7)
  status = fgsl_sf_airy_zero_ai_e(1, sfres)
  call unit_assert_equal('fgsl_sf_airy_zero_ai_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_airy_zero_ai_e',-2.338107410459767d0,&
       sfres%val,sfres%err)
  ra = fgsl_sf_airy_zero_bi(1)
  call unit_assert_equal_within('fgsl_sf_airy_zero_bi',-1.17371322d0,ra,eps7)
  status = fgsl_sf_airy_zero_bi_e(1, sfres)
  call unit_assert_equal('fgsl_sf_airy_zero_bi_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_airy_zero_bi_e',-1.173713222709128d0,&
       sfres%val,sfres%err)
  ra = fgsl_sf_airy_zero_ai_deriv(1)
  call unit_assert_equal_within('fgsl_sf_airy_zero_ai_deriv',&
       -1.01879297d0,ra,eps7)
  status = fgsl_sf_airy_zero_ai_deriv_e(1, sfres)
  call unit_assert_equal('fgsl_sf_airy_zero_ai_deriv_e:status',&
       fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_airy_zero_ai_deriv_e',&
       -1.018792971647471d0,sfres%val,sfres%err)
  ra = fgsl_sf_airy_zero_bi_deriv(1)
  call unit_assert_equal_within('fgsl_sf_airy_zero_bi_deriv',&
       -2.29443968d0,ra,eps7)
  status = fgsl_sf_airy_zero_bi_deriv_e(1, sfres)
  call unit_assert_equal('fgsl_sf_airy_zero_bi_deriv_e:status',&
       fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_airy_zero_bi_deriv_e',&
       -2.294439682614123d0,sfres%val,sfres%err)
!
! Bessel functions
!
  ra = fgsl_sf_bessel_jc0(1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_bessel_jc0',0.765197686557967d0,ra,eps10)
  status = fgsl_sf_bessel_jc0_e(1.0_fgsl_double, sfres)
  call unit_assert_equal('fgsl_sf_bessel_jc0_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_bessel_jc0_e',0.765197686557967d0,&
       sfres%val,sfres%err)
  ra = fgsl_sf_bessel_jc1(1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_bessel_jc1',0.4400505857d0,ra,eps10)
  status = fgsl_sf_bessel_jc1_e(1.0_fgsl_double, sfres)
  call unit_assert_equal('fgsl_sf_bessel_jc1_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_bessel_jc1_e',0.4400505857449335d0,&
       sfres%val,sfres%err)
  ra = fgsl_sf_bessel_jcn(1,1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_bessel_jcn',0.4400505857d0,ra,eps10)
  status = fgsl_sf_bessel_jcn_e(1,1.0_fgsl_double, sfres)
  call unit_assert_equal('fgsl_sf_bessel_jcn_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_bessel_jcn_e',0.4400505857449335d0,&
       sfres%val,sfres%err)
  status = fgsl_sf_bessel_jcn_array(1,1,1.0_fgsl_double,ra_arr)
  call unit_assert_equal('fgsl_sf_bessel_jcn_array:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_bessel_jcn_array',0.4400505857449335d0,&
       ra_arr(1),eps10)
  ra = fgsl_sf_bessel_yc0(1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_bessel_yc0',0.0882569642d0,ra,eps10)
  status = fgsl_sf_bessel_yc0_e(1.0_fgsl_double, sfres)
  call unit_assert_equal('fgsl_sf_bessel_yc0_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_bessel_yc0_e',0.08825696421567691d0,&
       sfres%val,sfres%err)
  ra = fgsl_sf_bessel_yc1(1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_bessel_yc1',-0.7812128213d0,ra,eps10)
  status = fgsl_sf_bessel_yc1_e(1.0_fgsl_double, sfres)
  call unit_assert_equal('fgsl_sf_bessel_yc1_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_bessel_yc1_e',-0.7812128213002888d0,&
       sfres%val,sfres%err)
  ra = fgsl_sf_bessel_ycn(1,1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_bessel_ycn',-0.7812128213d0,ra,eps10)
  status = fgsl_sf_bessel_ycn_e(1,1.0_fgsl_double, sfres)
  call unit_assert_equal('fgsl_sf_bessel_ycn_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_bessel_ycn_e',-0.7812128213002888d0,&
       sfres%val,sfres%err)
  status = fgsl_sf_bessel_ycn_array(1,1,1.0_fgsl_double,ra_arr)
  call unit_assert_equal('fgsl_sf_bessel_ycn_array:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_bessel_ycn_array',-0.7812128213002888d0,&
       ra_arr(1),eps10)
  ra = fgsl_sf_bessel_ic0(1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_bessel_ic0',1.2660658780d0,ra,eps7)
  status = fgsl_sf_bessel_ic0_e(1.0_fgsl_double, sfres)
  call unit_assert_equal('fgsl_sf_bessel_ic0_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_bessel_ic0_e',1.266065877752008d0,&
       sfres%val,sfres%err)
  ra = fgsl_sf_bessel_ic1(1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_bessel_ic1',.5651591041d0,ra,eps7)
  status = fgsl_sf_bessel_ic1_e(1.0_fgsl_double, sfres)
  call unit_assert_equal('fgsl_sf_bessel_ic1_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_bessel_ic1_e',.5651591039924850d0,&
       sfres%val,sfres%err)
  ra = fgsl_sf_bessel_icn(1,1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_bessel_icn',.5651591041d0,ra,eps7)
  status = fgsl_sf_bessel_icn_e(1,1.0_fgsl_double, sfres)
  call unit_assert_equal('fgsl_sf_bessel_icn_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_bessel_icn_e',.5651591039924850d0,&
       sfres%val,sfres%err)
  status = fgsl_sf_bessel_icn_array(1,1,1.0_fgsl_double,ra_arr)
  call unit_assert_equal('fgsl_sf_bessel_icn_array:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_bessel_icn_array',.5651591039924850d0,&
       ra_arr(1),eps10)
  ra = fgsl_sf_bessel_ic0_scaled(1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_bessel_ic0_scaled',&
       0.4657596077d0,ra,eps7)
  status = fgsl_sf_bessel_ic0_scaled_e(1.0_fgsl_double, sfres)
  call unit_assert_equal('fgsl_sf_bessel_ic0_scaled_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_bessel_ic0_scaled_e',&
       0.4657596075936405d0,sfres%val,sfres%err)
  ra = fgsl_sf_bessel_ic1_scaled(1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_bessel_ic1_scaled',&
       0.2079104154d0,ra,eps7)
  status = fgsl_sf_bessel_ic1_scaled_e(1.0_fgsl_double, sfres)
  call unit_assert_equal('fgsl_sf_bessel_ic1_scaled_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_bessel_ic1_scaled_e',&
       0.2079104153497085d0,sfres%val,sfres%err)
  ra = fgsl_sf_bessel_icn_scaled(1,1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_bessel_icn_scaled',&
       0.2079104154d0,ra,eps7)
  status = fgsl_sf_bessel_icn_scaled_e(1,1.0_fgsl_double, sfres)
  call unit_assert_equal('fgsl_sf_bessel_icn_scaled_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_bessel_icn_scaled_e',&
       0.2079104153497085d0,sfres%val,sfres%err)
  status = fgsl_sf_bessel_icn_scaled_array(1,1,1.0_fgsl_double,ra_arr)
  call unit_assert_equal('fgsl_sf_bessel_icn_scaled_array:status',&
       fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_bessel_icn_scaled_array',&
       0.2079104153497085d0,ra_arr(1),eps10)
  ra = fgsl_sf_bessel_kc0(1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_bessel_kc0',.42102443823d0,ra,eps7)
  status = fgsl_sf_bessel_kc0_e(1.0_fgsl_double, sfres)
  call unit_assert_equal('fgsl_sf_bessel_kc0_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_bessel_kc0_e',.4210244382407084d0,&
       sfres%val,sfres%err)
  ra = fgsl_sf_bessel_kc1(1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_bessel_kc1',.6019072302d0,ra,eps7)
  status = fgsl_sf_bessel_kc1_e(1.0_fgsl_double, sfres)
  call unit_assert_equal('fgsl_sf_bessel_kc1_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_bessel_kc1_e',.6019072301972346d0,&
       sfres%val,sfres%err)
  ra = fgsl_sf_bessel_kcn(1,1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_bessel_kcn',.6019072302d0,ra,eps7)
  status = fgsl_sf_bessel_kcn_e(1,1.0_fgsl_double, sfres)
  call unit_assert_equal('fgsl_sf_bessel_kcn_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_bessel_kcn_e',.6019072301972346d0,&
       sfres%val,sfres%err)
  status = fgsl_sf_bessel_kcn_array(1,1,1.0_fgsl_double,ra_arr)
  call unit_assert_equal('fgsl_sf_bessel_kcn_array:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_bessel_kcn_array',.6019072301972346d0,&
       ra_arr(1),eps10)
  ra = fgsl_sf_bessel_kc0_scaled(1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_bessel_kc0_scaled',&
       1.144463079806895d0,ra,eps7)
  status = fgsl_sf_bessel_kc0_scaled_e(1.0_fgsl_double, sfres)
  call unit_assert_equal('fgsl_sf_bessel_kc0_scaled_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_bessel_kc0_scaled_e',&
       1.144463079806895d0,sfres%val,sfres%err)
  ra = fgsl_sf_bessel_kc1_scaled(1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_bessel_kc1_scaled',&
       1.636153486263258d0,ra,eps7)
  status = fgsl_sf_bessel_kc1_scaled_e(1.0_fgsl_double, sfres)
  call unit_assert_equal('fgsl_sf_bessel_kc1_scaled_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_bessel_kc1_scaled_e',&
       1.636153486263258d0,sfres%val,sfres%err)
  ra = fgsl_sf_bessel_kcn_scaled(1,1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_bessel_kcn_scaled',&
       1.636153486263258d0,ra,eps7)
  status = fgsl_sf_bessel_kcn_scaled_e(1,1.0_fgsl_double, sfres)
  call unit_assert_equal('fgsl_sf_bessel_kcn_scaled_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_bessel_kcn_scaled_e',&
       1.636153486263258d0,sfres%val,sfres%err)
  status = fgsl_sf_bessel_kcn_scaled_array(1,1,1.0_fgsl_double,ra_arr)
  call unit_assert_equal('fgsl_sf_bessel_kcn_scaled_array:status',&
       fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_bessel_kcn_scaled_array',&
       1.636153486263258d0,ra_arr(1),eps10)
  ra = fgsl_sf_bessel_js0(1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_bessel_js0',0.84147098d0,ra,eps7)
  status = fgsl_sf_bessel_js0_e(1.0_fgsl_double, sfres)
  call unit_assert_equal('fgsl_sf_bessel_js0_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_bessel_js0_e',0.8414709848078965d0,&
       sfres%val,sfres%err)
  ra = fgsl_sf_bessel_js1(1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_bessel_js1',0.30116868d0,ra,eps7)
  status = fgsl_sf_bessel_js1_e(1.0_fgsl_double, sfres)
  call unit_assert_equal('fgsl_sf_bessel_js1_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_bessel_js1_e',0.3011686789397567d0,&
       sfres%val,sfres%err)
  ra = fgsl_sf_bessel_js2(1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_bessel_js2',0.062035052d0,ra,eps7)
  status = fgsl_sf_bessel_js2_e(1.0_fgsl_double, sfres)
  call unit_assert_equal('fgsl_sf_bessel_js2_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_bessel_js2_e',6.203505201137386D-02,&
       sfres%val,sfres%err)
  ra = fgsl_sf_bessel_jsl(0,1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_bessel_jsl',0.84147098d0,ra,eps7)
  status = fgsl_sf_bessel_jsl_e(0,1.0_fgsl_double, sfres)
  call unit_assert_equal('fgsl_sf_bessel_jsl_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_bessel_jsl_e',0.8414709848078965d0,&
       sfres%val,sfres%err)
  status = fgsl_sf_bessel_jsl_array(0,1.0_fgsl_double,ra_arr)
  call unit_assert_equal('fgsl_sf_bessel_jsl_array:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_bessel_jsl_array',0.8414709848078965d0,&
       ra_arr(1),eps10)
  status = fgsl_sf_bessel_jsl_steed_array(0,1.0_fgsl_double,ra_arr)
  call unit_assert_equal('fgsl_sf_bessel_jsl_steed_array:status',&
       fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_bessel_jsl_steed_array',&
       0.8414709848078965d0,ra_arr(1),eps10)
  ra = fgsl_sf_bessel_ys0(1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_bessel_ys0',-0.540302306d0,ra,eps7)
  status = fgsl_sf_bessel_ys0_e(1.0_fgsl_double, sfres)
  call unit_assert_equal('fgsl_sf_bessel_ys0_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_bessel_ys0_e',-.5403023058681397d0,&
       sfres%val,sfres%err)
  ra = fgsl_sf_bessel_ys1(1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_bessel_ys1',-1.3817733d0,ra,eps7)
  status = fgsl_sf_bessel_ys1_e(1.0_fgsl_double, sfres)
  call unit_assert_equal('fgsl_sf_bessel_ys1_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_bessel_ys1_e',-1.381773290676036d0,&
       sfres%val,sfres%err)
  ra = fgsl_sf_bessel_ys2(1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_bessel_ys2',-3.6050176d0,ra,eps7)
  status = fgsl_sf_bessel_ys2_e(1.0_fgsl_double, sfres)
  call unit_assert_equal('fgsl_sf_bessel_ys2_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_bessel_ys2_e',-3.605017566159969d0,&
       sfres%val,sfres%err)
  ra = fgsl_sf_bessel_ysl(1,1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_bessel_ysl',-1.3817733d0,ra,eps7)
  status = fgsl_sf_bessel_ysl_e(1,1.0_fgsl_double, sfres)
  call unit_assert_equal('fgsl_sf_bessel_ysl_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_bessel_ysl_e',-1.381773290676036d0,&
       sfres%val,sfres%err)
  status = fgsl_sf_bessel_ysl_array(0,1.0_fgsl_double,ra_arr)
  call unit_assert_equal('fgsl_sf_bessel_ysl_array:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_bessel_ysl_array',-.5403023058681397d0,&
       ra_arr(1),eps10)
  ra = fgsl_sf_bessel_is0_scaled(1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_bessel_is0',.432332357d0,ra,eps7)
  status = fgsl_sf_bessel_is0_scaled_e(1.0_fgsl_double, sfres)
  call unit_assert_equal('fgsl_sf_bessel_is0_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_bessel_is0_e',.4323323583816936d0,&
       sfres%val,sfres%err)
  ra = fgsl_sf_bessel_is1_scaled(1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_bessel_is1_scaled',.13533528d0,ra,eps7)
  status = fgsl_sf_bessel_is1_scaled_e(1.0_fgsl_double, sfres)
  call unit_assert_equal('fgsl_sf_bessel_is1_scaled_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_bessel_is1_scaled_e',&
       .1353352832366128d0,sfres%val,eps10)
! NOTE: replaced sfres%err by eps10
  ra = fgsl_sf_bessel_is2_scaled(1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_bessel_is2_scaled',.026326509d0,ra,eps7)
  status = fgsl_sf_bessel_is2_scaled_e(1.0_fgsl_double, sfres)
  call unit_assert_equal('fgsl_sf_bessel_is2_scaled_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_bessel_is2_scaled_e',&
       .02632650867185538d0,sfres%val,eps10)
! NOTE: replaced sfres%err by eps10
  ra = fgsl_sf_bessel_isl_scaled(0,1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_bessel_isl_scaled',.432332357d0,ra,eps7)
  status = fgsl_sf_bessel_isl_scaled_e(0,1.0_fgsl_double, sfres)
  call unit_assert_equal('fgsl_sf_bessel_isl_scaled_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_bessel_isl_scaled_e',&
       .4323323583816936d0,sfres%val,sfres%err)
  status = fgsl_sf_bessel_isl_scaled_array(0,1.0_fgsl_double,ra_arr)
  call unit_assert_equal('fgsl_sf_bessel_isl_scaled_array:status',&
       fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_bessel_isl_scaled_array',&
       .4323323583816936d0,ra_arr(1),eps10)
  ra = fgsl_sf_bessel_ks0_scaled(1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_bessel_ks0_scaled',1.57079631d0,ra,eps7)
  status = fgsl_sf_bessel_ks0_scaled_e(1.0_fgsl_double, sfres)
  call unit_assert_equal('fgsl_sf_bessel_ks0_scaled_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_bessel_ks0_scaled_e',&
       1.570796326794897d0,sfres%val,sfres%err)
  ra = fgsl_sf_bessel_ks1_scaled(1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_bessel_ks1_scaled',3.14159265d0,ra,eps7)
  status = fgsl_sf_bessel_ks1_scaled_e(1.0_fgsl_double, sfres)
  call unit_assert_equal('fgsl_sf_bessel_ks1_scaled_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_bessel_ks1_scaled_e',&
       3.141592653589793d0,sfres%val,sfres%err)
  ra = fgsl_sf_bessel_ks2_scaled(1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_bessel_ks2_scaled',10.99557422d0,ra,eps7)
  status = fgsl_sf_bessel_ks2_scaled_e(1.0_fgsl_double, sfres)
  call unit_assert_equal('fgsl_sf_bessel_ks2_scaled_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_bessel_ks2_scaled_e',&
       10.99557428756428d0,sfres%val,sfres%err)
  ra = fgsl_sf_bessel_ksl_scaled(0,1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_bessel_ksl_scaled',1.57079631d0,ra,eps7)
  status = fgsl_sf_bessel_ksl_scaled_e(0,1.0_fgsl_double, sfres)
  call unit_assert_equal('fgsl_sf_bessel_ksl_scaled_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_bessel_ksl_scaled_e',&
       1.570796326794897d0,sfres%val,sfres%err)
  status = fgsl_sf_bessel_ksl_scaled_array(0,1.0_fgsl_double,ra_arr)
  call unit_assert_equal('fgsl_sf_bessel_ksl_scaled_array:status',&
       fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_bessel_ksl_scaled_array',&
       1.570796326794897d0,ra_arr(1),eps10)
  ra = fgsl_sf_bessel_jnu(1.0_fgsl_double,1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_bessel_jnu',0.4400505857d0,ra,eps10)
  status = fgsl_sf_bessel_jnu_e(1.0_fgsl_double,1.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_bessel_jnu_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_bessel_jnu_e',0.4400505857449335d0,&
       sfres%val,sfres%err)
  ra_arr(1) = 1.0d0
  status = fgsl_sf_bessel_sequence_jnu_e(1.0_fgsl_double, fgsl_prec_double, &
       1_fgsl_size_t, ra_arr)
  call unit_assert_equal('fgsl_sf_bessel_sequence_jnu_e:status',&
       fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_bessel_sequence_jnu_e',&
       0.4400505857449335d0,ra_arr(1),eps10)
  ra = fgsl_sf_bessel_ynu(1.0_fgsl_double,1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_bessel_ynu',-0.781212821300d0,ra,eps10)
  status = fgsl_sf_bessel_ynu_e(1.0_fgsl_double,1.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_bessel_ynu_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_bessel_ynu_e',-0.7812128213002888d0,&
       sfres%val,sfres%err)
  ra = fgsl_sf_bessel_inu(1.0_fgsl_double,1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_bessel_inu',.5651591039924850d0,ra,eps10)
  status = fgsl_sf_bessel_inu_e(1.0_fgsl_double,1.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_bessel_inu_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_bessel_inu_e',.5651591039924850d0,&
       sfres%val,sfres%err)
  ra = fgsl_sf_bessel_inu_scaled(1.0_fgsl_double,1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_bessel_inu_scaled',&
       0.2079104153497085d0,ra,eps10)
  status = fgsl_sf_bessel_inu_scaled_e(1.0_fgsl_double,1.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_bessel_inu_scaled_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_bessel_inu_scaled_e',&
       0.2079104153497085d0,sfres%val,sfres%err)
  ra = fgsl_sf_bessel_knu(1.0_fgsl_double,1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_bessel_knu',.6019072301972346d0,ra,eps10)
  status = fgsl_sf_bessel_knu_e(1.0_fgsl_double,1.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_bessel_knu_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_bessel_knu_e',.6019072301972346d0,&
       sfres%val,sfres%err)
  ra = fgsl_sf_bessel_lnknu(1.0_fgsl_double,1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_bessel_lnknu',&
       log(.6019072301972346d0),ra,eps10)
  status = fgsl_sf_bessel_lnknu_e(1.0_fgsl_double,1.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_bessel_lnknu_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_bessel_lnknu_e',&
       log(.6019072301972346d0),sfres%val,sfres%err)
  ra = fgsl_sf_bessel_knu_scaled(1.0_fgsl_double,1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_bessel_knu_scaled',&
       1.636153486263258d0,ra,eps10)
  status = fgsl_sf_bessel_knu_scaled_e(1.0_fgsl_double,1.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_bessel_knu_scaled_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_bessel_knu_scaled_e',&
       1.636153486263258d0,sfres%val,sfres%err)
  ra = fgsl_sf_bessel_zero_jc0(1)
  call unit_assert_equal_within('fgsl_sf_bessel_zero_jc0',&
       2.4048255577d0,ra,eps10)
  status = fgsl_sf_bessel_zero_jc0_e(1,sfres)
  call unit_assert_equal('fgsl_sf_bessel_zero_jc0_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_bessel_zero_jc0_e',&
       2.404825557695771d0,sfres%val,sfres%err)
  ra = fgsl_sf_bessel_zero_jc1(1)
  call unit_assert_equal_within('fgsl_sf_bessel_zero_jc1',&
       3.83170597d0,ra,eps7)
  status = fgsl_sf_bessel_zero_jc1_e(1,sfres)
  call unit_assert_equal('fgsl_sf_bessel_zero_jc1_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_bessel_zero_jc1_e',&
       3.831705970207503d0,sfres%val,sfres%err)
  ra = fgsl_sf_bessel_zero_jnu(1.0_fgsl_double,1)
  call unit_assert_equal_within('fgsl_sf_bessel_zero_jnu',&
       3.83170597d0,ra,eps7)
  status = fgsl_sf_bessel_zero_jnu_e(1.0_fgsl_double,1,sfres)
  call unit_assert_equal('fgsl_sf_bessel_zero_jnu_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_bessel_zero_jnu_e',&
       3.831705970207513d0,sfres%val,sfres%err)
! NOTE: error estimates appear to be smaller than variances jnu vs jc1 ...
!
! Clausen function
!
  ra = fgsl_sf_clausen(m_pi/2.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_clausen',&
       0.915966d0,ra,eps5)
  status = fgsl_sf_clausen_e(m_pi/2.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_clausen_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_clausen_e',&
       0.9159655941772190d0,sfres%val,sfres%err)
!
! Coulomb functions
!
  ra = fgsl_sf_hydrogenicr_1(1.0_fgsl_double,1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_hydrogenicr_1',2.0d0/exp(1.0d0),ra,eps7)
  status = fgsl_sf_hydrogenicr_1_e(1.0_fgsl_double, 1.0_fgsl_double, sfres)
  call unit_assert_equal('fgsl_sf_hydrogenicr_1:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_hydrogenicr_1_e',2.0d0/exp(1.0d0),&
       sfres%val,sfres%err)
  ra = fgsl_sf_hydrogenicr(1,0,1.0_fgsl_double,1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_hydrogenicr',2.0d0/exp(1.0d0),ra,eps7)
  status = fgsl_sf_hydrogenicr_e(1,0,1.0_fgsl_double, 1.0_fgsl_double, sfres)
  call unit_assert_equal('fgsl_sf_hydrogenicr:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_hydrogenicr_e',2.0d0/exp(1.0d0),&
       sfres%val,sfres%err)
  status = fgsl_sf_coulomb_wave_fg_e(1.0_fgsl_double,1.0_fgsl_double,&
       0.0_fgsl_double,0,sfres,sfres_der,sfres_2,sfres_2_der,&
       ra_exp,ra_exp_2)
  call unit_assert_equal('fgsl_sf_coulomb_wave_fg_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_coulomb_wave_fg_e:F',&
       0.2275262105105601d0,sfres%val,sfres%err)
  call unit_assert_equal_within('fgsl_sf_coulomb_wave_fg_e:Fder',&
       0.348734422858357d0,sfres_der%val,sfres_der%err)
  call unit_assert_equal_within('fgsl_sf_coulomb_wave_fg_e:G',&
       2.043097162103555d0,sfres_2%val,sfres_2%err)
  call unit_assert_equal_within('fgsl_sf_coulomb_wave_fg_e:Gder',&
       -1.263598113312426d0,sfres_2_der%val,sfres_2_der%err)
  status = fgsl_sf_coulomb_wave_f_array (0.0_fgsl_double,0,1.0_fgsl_double, &
       1.0_fgsl_double,ra_arr,ra_exp)
  call unit_assert_equal('fgsl_sf_coulomb_wave_f_array:status',&
       fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_coulomb_wave_f_array',&
       0.2275262105105601d0,ra_arr(1),eps10)
  status = fgsl_sf_coulomb_wave_fg_array (0.0_fgsl_double,0,1.0_fgsl_double, &
       1.0_fgsl_double,ra_arr,ra_arr_2,ra_exp,ra_exp_2)
  call unit_assert_equal('fgsl_sf_coulomb_wave_fg_array:status',&
       fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_coulomb_wave_fg_array:F',&
       0.2275262105105601d0,ra_arr(1),eps10)
  call unit_assert_equal_within('fgsl_sf_coulomb_wave_fg_array:G',&
       2.043097162103555d0,ra_arr_2(1),eps10)
  status = fgsl_sf_coulomb_wave_fgp_array (0.0_fgsl_double,0,1.0_fgsl_double, &
       1.0_fgsl_double,ra_arr,ra_arr_der,ra_arr_2,ra_arr_2_der,ra_exp,ra_exp_2)
  call unit_assert_equal('fgsl_sf_coulomb_wave_fg_array:status',&
       fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_coulomb_wave_fg_array:F',&
       0.2275262105105601d0,ra_arr(1),eps10)
  call unit_assert_equal_within('fgsl_sf_coulomb_wave_fg_array:Fder',&
       0.348734422858357d0,ra_arr_der(1),eps10)
  call unit_assert_equal_within('fgsl_sf_coulomb_wave_fg_array:G',&
       2.043097162103555d0,ra_arr_2(1),eps10)
  call unit_assert_equal_within('fgsl_sf_coulomb_wave_fg_array:Gder',&
       -1.263598113312426d0,ra_arr_2_der(1),eps10)
  status = fgsl_sf_coulomb_wave_sphf_array (0.0_fgsl_double,0,1.0_fgsl_double, &
       1.0_fgsl_double,ra_arr,ra_exp)
  call unit_assert_equal('fgsl_sf_coulomb_wave_sphf_array:status',&
       fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_coulomb_wave_sphf_array',&
       0.2275262105105601d0,ra_arr(1),eps10)
  status = fgsl_sf_coulomb_cl_e(0.0_fgsl_double, 0.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_coulomb_cl_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_coulomb_cl_e',&
       1.0d0,sfres%val,sfres%err)
  status = fgsl_sf_coulomb_cl_array(0.0_fgsl_double, 0, 0.0_fgsl_double,ra_arr)
  call unit_assert_equal('fgsl_sf_coulomb_cl_array:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_coulomb_cl_array',&
       1.0d0,ra_arr(1),eps10)
!
! coupling coefficients
!
  ra = fgsl_sf_coupling_3j(2,2,0,0,0,0)
  call unit_assert_equal_within('fgsl_sf_coupling_3j',&
       -1.0d0/sqrt(3.0d0),ra,eps10)
  status = fgsl_sf_coupling_3j_e(2,2,0,0,0,0,sfres)
  call unit_assert_equal('fgsl_sf_coupling_3j_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_coupling_3j_e',&
       -1.0d0/sqrt(3.0d0),sfres%val,sfres%err)
  ra = fgsl_sf_coupling_6j(2,2,0,4,4,4)
  call unit_assert_equal_within('fgsl_sf_coupling_6j',&
       -1.0d0/sqrt(15.0d0),ra,eps10)
  status = fgsl_sf_coupling_6j_e(2,2,0,4,4,4,sfres)
  call unit_assert_equal('fgsl_sf_coupling_6j_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_coupling_6j_e',&
       -1.0d0/sqrt(15.0d0),sfres%val,sfres%err)
  ra = fgsl_sf_coupling_9j(2,2,0,4,4,0,4,4,0)
  call unit_assert_equal_within('fgsl_sf_coupling_9j',&
       1.0d0/(5.0d0*sqrt(3.0d0)),ra,eps10)
  status = fgsl_sf_coupling_9j_e(2,2,0,4,4,0,4,4,0,sfres)
  call unit_assert_equal('fgsl_sf_coupling_9j_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_coupling_9j_e',&
       1.0d0/(5.0d0*sqrt(3.0d0)),sfres%val,sfres%err)
!
! Dawson, Debye and Dilog functions
!
  ra = fgsl_sf_dawson(1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_dawson',&
       0.5380795069d0,ra,eps10)
  status = fgsl_sf_dawson_e(1.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_dawson_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_dawson_e',&
       0.5380795069127684d0,sfres%val,sfres%err)
  ra = fgsl_sf_debye_1(1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_debye_1',&
       0.7775046d0,ra,eps7)
  status = fgsl_sf_debye_1_e(1.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_debye_1_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_debye_1_e',&
       0.7775046341122482d0,sfres%val,sfres%err)
  ra = fgsl_sf_debye_2(1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_debye_2',&
       0.7078785d0,ra,eps7)
  status = fgsl_sf_debye_2_e(1.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_debye_2_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_debye_2_e',&
       7.078784756278293511D-01,sfres%val,sfres%err)
  ra = fgsl_sf_debye_3(1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_debye_3',&
       0.6744156d0,ra,eps7)
  status = fgsl_sf_debye_3_e(1.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_debye_3_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_debye_3_e',&
       6.744155640778146665D-01,sfres%val,sfres%err)
  ra = fgsl_sf_debye_4(1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_debye_4',&
       0.6548741d0,ra,eps7)
  status = fgsl_sf_debye_4_e(1.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_debye_4_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_debye_4_e',&
       6.548740688867370485D-01,sfres%val,sfres%err)
  ra = fgsl_sf_debye_5(1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_debye_5',&
       0.6421003d0,ra,eps7)
  status = fgsl_sf_debye_5_e(1.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_debye_5_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_debye_5_e',&
       6.421002580217789468D-01,sfres%val,sfres%err)
  ra = fgsl_sf_debye_6(1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_debye_6',&
       .6331114d0,ra,eps7)
  status = fgsl_sf_debye_6_e(1.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_debye_6_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_debye_6_e',&
       6.331114258349510582D-01,sfres%val,sfres%err)
  ra = fgsl_sf_dilog(1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_dilog',&
       1.644934067d0,ra,eps7)
  status = fgsl_sf_dilog_e(1.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_dilog_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_dilog_e',&
       1.644934066848226d0,sfres%val,sfres%err)
  status = fgsl_sf_complex_dilog_e(1.0_fgsl_double,0.0_fgsl_double,&
       sfres,sfres_2)
  call unit_assert_equal('fgsl_sf_complex_dilog_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_complex_dilog_e:Re',&
       1.644934066848226d0,sfres%val,sfres%err)
  call unit_assert_equal_within('fgsl_sf_complex_dilog_e:Im',&
       0.0d0,sfres_2%val,sfres_2%err)
!
! elliptic functions
!
  ra = fgsl_sf_ellint_kcomp(0.5_fgsl_double, fgsl_prec_double)
  call unit_assert_equal_within('fgsl_sf_ellint_kcomp',1.685750354812d0,ra,eps10)
  status = fgsl_sf_ellint_kcomp_e(0.5_fgsl_double,fgsl_prec_double,sfres)
  call unit_assert_equal('fgsl_sf_ellint_kcomp_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_ellint_kcomp_e',&
       1.685750354812596d0,sfres%val,sfres%err)
  ra = fgsl_sf_ellint_ecomp(0.5_fgsl_double, fgsl_prec_double)
  call unit_assert_equal_within('fgsl_sf_ellint_ecomp',1.46746220933d0,ra,eps10)
  status = fgsl_sf_ellint_ecomp_e(0.5_fgsl_double,fgsl_prec_double,sfres)
  call unit_assert_equal('fgsl_sf_ellint_ecomp_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_ellint_ecomp_e',&
       1.4674622093394274d0,sfres%val,sfres%err)
!
  ra = fgsl_sf_ellint_pcomp(0.5_fgsl_double, -0.2_fgsl_double, fgsl_prec_double)
  call unit_assert_equal_within('fgsl_sf_ellint_pcomp',1.89229d0,ra,eps5)
  status = fgsl_sf_ellint_pcomp_e(0.5_fgsl_double, -0.2_fgsl_double, &
       fgsl_prec_double,sfres)
  call unit_assert_equal('fgsl_sf_ellint_pcomp_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_ellint_pcomp_e',&
       1.892294761226402278d0,sfres%val,sfres%err)
!
  ra = fgsl_sf_ellint_f(m_pi/2,0.5_fgsl_double, fgsl_prec_double)
  call unit_assert_equal_within('fgsl_sf_ellint_f',1.685750354812d0,ra,eps10)
  status = fgsl_sf_ellint_f_e(m_pi/2,0.5_fgsl_double,&
       fgsl_prec_double,sfres)
  call unit_assert_equal('fgsl_sf_ellint_f_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_ellint_f_e',&
       1.685750354812596d0,sfres%val,sfres%err)
  ra = fgsl_sf_ellint_e(m_pi/2, 0.5_fgsl_double, fgsl_prec_double)
  call unit_assert_equal_within('fgsl_sf_ellint_e',1.46746220933d0,ra,eps10)
  status = fgsl_sf_ellint_e_e(m_pi/2,0.5_fgsl_double,&
       fgsl_prec_double,sfres)
  call unit_assert_equal('fgsl_sf_ellint_e_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_ellint_e_e',&
       1.4674622093394274d0,sfres%val,sfres%err)
  ra = fgsl_sf_ellint_p(m_pi/2, dsqrt(0.75d0), -0.5_fgsl_double, fgsl_prec_double)
  call unit_assert_equal_within('fgsl_sf_ellint_p',3.234773471249d0,ra,eps10)
  status = fgsl_sf_ellint_p_e(m_pi/2,dsqrt(0.75d0),-0.5_fgsl_double,&
       fgsl_prec_double,sfres)
  call unit_assert_equal('fgsl_sf_ellint_p_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_ellint_p_e',&
       3.234773471249465d0,sfres%val,eps10)
! NOTE: replaced sfres%err by eps10
  ra = fgsl_sf_ellint_d(m_pi/2,.0_fgsl_double, fgsl_prec_double)
  call unit_assert_equal_within('fgsl_sf_ellint_d',m_pi/4,ra,eps10)
  status = fgsl_sf_ellint_d_e(m_pi/2,.0_fgsl_double,.0_fgsl_double,&
       fgsl_prec_double,sfres)
  call unit_assert_equal('fgsl_sf_ellint_d_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_ellint_d_e',&
       m_pi/4,sfres%val,sfres%err)
  ra = fgsl_sf_ellint_rc(1.0_fgsl_double,1.0_fgsl_double,fgsl_prec_double)
  call unit_assert_equal_within('fgsl_sf_ellint_rc',1.0d0,ra,eps10)
  status = fgsl_sf_ellint_rc_e(1.0_fgsl_double,1.0_fgsl_double,&
       fgsl_prec_double,sfres)
  call unit_assert_equal('fgsl_sf_ellint_rc_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_ellint_rc_e',&
       1.0d0,sfres%val,sfres%err)
  ra = fgsl_sf_ellint_rd(1.0_fgsl_double,1.0_fgsl_double,1.0_fgsl_double,&
       fgsl_prec_double)
  call unit_assert_equal_within('fgsl_sf_ellint_rd',1.0d0,ra,eps10)
  status = fgsl_sf_ellint_rd_e(1.0_fgsl_double,1.0_fgsl_double,&
       1.0_fgsl_double,fgsl_prec_double,sfres)
  call unit_assert_equal('fgsl_sf_ellint_rd_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_ellint_rd_e',&
       1.0d0,sfres%val,sfres%err)
  ra = fgsl_sf_ellint_rf(1.0_fgsl_double,1.0_fgsl_double,1.0_fgsl_double,&
       fgsl_prec_double)
  call unit_assert_equal_within('fgsl_sf_ellint_rf',1.0d0,ra,eps10)
  status = fgsl_sf_ellint_rf_e(1.0_fgsl_double,1.0_fgsl_double,&
       1.0_fgsl_double,fgsl_prec_double,sfres)
  call unit_assert_equal('fgsl_sf_ellint_rf_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_ellint_rf_e',&
       1.0d0,sfres%val,sfres%err)
  ra = fgsl_sf_ellint_rj(1.0_fgsl_double,1.0_fgsl_double,1.0_fgsl_double,&
       1.0_fgsl_double,fgsl_prec_double)
  call unit_assert_equal_within('fgsl_sf_ellint_rj',1.0d0,ra,eps10)
  status = fgsl_sf_ellint_rj_e(1.0_fgsl_double,1.0_fgsl_double,&
       1.0_fgsl_double,1.0_fgsl_double,fgsl_prec_double,sfres)
  call unit_assert_equal('fgsl_sf_ellint_rj_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_ellint_rj_e',&
       1.0d0,sfres%val,sfres%err)
  status = fgsl_sf_elljac_e(m_pi/2,0.0_fgsl_double,ra,ra_2,ra_3)
  call unit_assert_equal('fgsl_sf_elljac_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_elljac',1.0d0,ra,eps10)
  call unit_assert_equal_within('fgsl_sf_elljac',0.0d0,ra_2,eps10)
  call unit_assert_equal_within('fgsl_sf_elljac',1.0d0,ra_3,eps10)
!
! Error and Exponential functions
!
  ra = fgsl_sf_erf(1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_erf',0.84270079295d0,ra,eps10)
  status = fgsl_sf_erf_e(1.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_erf_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_erf_e',&
       0.8427007929497149d0,sfres%val,sfres%err)
  ra = fgsl_sf_erfc(1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_erfc',1-0.84270079295d0,ra,eps10)
  status = fgsl_sf_erfc_e(1.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_erfc_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_erfc_e',&
       1-0.8427007929497149d0,sfres%val,sfres%err)
  ra = fgsl_sf_log_erfc(1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_log_erfc',log(1-0.84270079295d0),&
       ra,eps10)
  status = fgsl_sf_log_erfc_e(1.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_log_erfc_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_log_erfc_e',&
       log(1-0.8427007929497149d0),sfres%val,sfres%err)
  ra = fgsl_sf_erf_z(1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_erf_z',1/sqrt(2*m_pi)/exp(0.5d0),ra,eps10)
  status = fgsl_sf_erf_z_e(1.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_erf_z_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_erf_z_e',&
       1/sqrt(2*m_pi)/exp(0.5d0),sfres%val,sfres%err)
  ra = fgsl_sf_erf_q(sqrt(2.0_fgsl_double))
  call unit_assert_equal_within('fgsl_sf_erf_q',(1-0.84270079295d0)/2.0d0,&
       ra,eps10)
  status = fgsl_sf_erf_q_e(sqrt(2.0_fgsl_double),sfres)
  call unit_assert_equal('fgsl_sf_erf_q_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_erf_q_e',&
       (1-0.8427007929497149d0)/2.0d0,sfres%val,sfres%err)
  ra = fgsl_sf_hazard(sqrt(2.0_fgsl_double))
  call unit_assert_equal_within('fgsl_sf_hazard',&
       sqrt(2/m_pi)/exp(1.0d0)/(1-0.8427007929497149d0),ra,eps10)
  status = fgsl_sf_hazard_e(sqrt(2.0_fgsl_double),sfres)
  call unit_assert_equal('fgsl_sf_hazard_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_hazard_e',&
       sqrt(2/m_pi)/exp(1.0d0)/(1-0.8427007929497149d0),sfres%val,sfres%err)
  ra = fgsl_sf_exp(1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_exp',exp(1.d0),ra,eps10)
  status = fgsl_sf_exp_e(1.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_exp_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_exp_e',&
       exp(1.d0),sfres%val,sfres%err)
  status = fgsl_sf_exp_e10_e(1.0_fgsl_double,sfres10)
  call unit_assert_equal('fgsl_sf_exp_e10_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_exp_e10_e',&
       exp(1.d0),sfres10%val,sfres10%err)
  ra = fgsl_sf_exp_mult(1.0_fgsl_double,1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_exp_mult',exp(1.d0),ra,eps10)
  status = fgsl_sf_exp_mult_e(1.0_fgsl_double,1.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_exp_mult_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_exp_mult_e',&
       exp(1.d0),sfres%val,sfres%err)
  status = fgsl_sf_exp_mult_e10_e(1.0_fgsl_double,1.0_fgsl_double,sfres10)
  call unit_assert_equal('fgsl_sf_exp_mult_e10_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_exp_mult_e10_e',&
       exp(1.d0),sfres10%val,sfres10%err)
  ra = fgsl_sf_expm1(1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_expm1',exp(1.d0)-1,ra,eps10)
  status = fgsl_sf_expm1_e(1.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_expm1_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_expm1_e',&
       exp(1.d0)-1,sfres%val,sfres%err)
  ra = fgsl_sf_exprel(1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_exprel',exp(1.d0)-1,ra,eps10)
  status = fgsl_sf_exprel_e(1.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_exprel_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_exprel_e',&
       exp(1.d0)-1,sfres%val,sfres%err)
  ra = fgsl_sf_exprel_2(1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_exprel_2',2*(exp(1.d0)-2),ra,eps10)
  status = fgsl_sf_exprel_2_e(1.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_exprel_2_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_exprel_2_e',&
       2*(exp(1.d0)-2),sfres%val,sfres%err)
  ra = fgsl_sf_exprel_n(2,1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_exprel_n',2*(exp(1.d0)-2),ra,eps10)
  status = fgsl_sf_exprel_n_e(2,1.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_exprel_n_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_exprel_n_e',&
       2*(exp(1.d0)-2),sfres%val,sfres%err)
  status = fgsl_sf_exp_err_e(1.0_fgsl_double,eps7,sfres)
  call unit_assert_equal('fgsl_sf_exp_err_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_exp_err_e',&
       exp(1.d0),sfres%val,sfres%err)
  status = fgsl_sf_exp_err_e10_e(1.0_fgsl_double,eps7,sfres10)
  call unit_assert_equal('fgsl_sf_exp_err_e10_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_exp_err_e10_e',&
       exp(1.d0),sfres10%val,sfres10%err)
  status = fgsl_sf_exp_mult_err_e(1.0_fgsl_double,eps7,1.0_fgsl_double,eps7,sfres)
  call unit_assert_equal('fgsl_sf_exp_mult_err_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_exp_mult_err_e',&
       exp(1.d0),sfres%val,sfres%err)
  status = fgsl_sf_exp_mult_err_e10_e(1.0_fgsl_double,eps7,&
       1.0_fgsl_double,eps7,sfres10)
  call unit_assert_equal('fgsl_sf_exp_mult_err_e10_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_exp_mult_err_e10_e',&
       exp(1.d0),sfres10%val,sfres10%err)
!
! Exponential, Hyperbolic and Trigonometric Integrals
!
  ra = fgsl_sf_expint_e1(1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_expint_e1',0.219383934d0,ra,eps7)
  status = fgsl_sf_expint_e1_e(1.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_expint_e1_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_expint_e1_e',&
       2.193839343955202859D-01,sfres%val,sfres%err)
  ra = fgsl_sf_expint_e2(1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_expint_e2',0.1484955d0,ra,eps7)
  status = fgsl_sf_expint_e2_e(1.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_expint_e2_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_expint_e2_e',&
       0.1484955067759220d0,sfres%val,sfres%err)
  ra = fgsl_sf_expint_en(1_fgsl_int, 1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_expint_en',0.219383934d0,ra,eps7)
  status = fgsl_sf_expint_en_e(1_fgsl_int,1.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_expint_en_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_expint_en_e',&
       2.193839343955202859D-01,sfres%val,sfres%err)
  ra = fgsl_sf_expint_ei(1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_expint_ei',1.895117816d0,ra,eps7)
  status = fgsl_sf_expint_ei_e(1.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_expint_ei_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_expint_ei_e',&
       1.895117816355937d0,sfres%val,sfres%err)
  ra = fgsl_sf_shi(1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_shi',1.057250875375728372D+00,ra,eps7)
  status = fgsl_sf_shi_e(1.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_shi_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_shi_e',&
       1.057250875375728372D+00,sfres%val,sfres%err)
  ra = fgsl_sf_chi(1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_chi',8.378669409802081969D-01,ra,eps7)
  status = fgsl_sf_chi_e(1.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_chi_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_chi_e',&
       8.378669409802081969D-01,sfres%val,sfres%err)
  ra = fgsl_sf_expint_3(1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_expint_3',8.075111821396715461D-01,ra,eps7)
  status = fgsl_sf_expint_3_e(1.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_expint_3_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_expint_3_e',&
       8.075111821396715461D-01,sfres%val,sfres%err)
  ra = fgsl_sf_si(1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_si',0.9460830704D+00,ra,eps7)
  status = fgsl_sf_si_e(1.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_si_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_si_e',&
       9.460830703671829767D-01,sfres%val,sfres%err)
  ra = fgsl_sf_ci(1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_ci',0.3374039229D+00,ra,eps7)
  status = fgsl_sf_ci_e(1.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_ci_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_ci_e',&
       3.374039229009682117D-01,sfres%val,sfres%err)
  ra = fgsl_sf_atanint(1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_atanint',9.159655941D-01,ra,eps7)
  status = fgsl_sf_atanint_e(1.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_atanint_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_atanint_e',&
       9.159655941772190113D-01,sfres%val,sfres%err)
!
! Fermi-Dirac
!
  ra = fgsl_sf_fermi_dirac_m1(0.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_fermi_dirac_m1',0.5d0,ra,eps7)
  status = fgsl_sf_fermi_dirac_m1_e(0.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_fermi_dirac_m1_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_fermi_dirac_m1_e',&
       0.5d0,sfres%val,sfres%err)
  ra = fgsl_sf_fermi_dirac_0(1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_fermi_dirac_0',log(1+exp(1.0d0)),ra,eps7)
  status = fgsl_sf_fermi_dirac_0_e(1.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_fermi_dirac_0_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_fermi_dirac_0_e',&
       log(1+exp(1.0d0)),sfres%val,sfres%err)
  ra = fgsl_sf_fermi_dirac_1(0.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_fermi_dirac_1',8.22467033424D-01,ra,eps7)
  status = fgsl_sf_fermi_dirac_1_e(0.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_fermi_dirac_1_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_fermi_dirac_1_e',&
       8.224670334241132030D-01,sfres%val,sfres%err)
  ra = fgsl_sf_fermi_dirac_2(0.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_fermi_dirac_2',9.015426774D-01,ra,eps7)
  status = fgsl_sf_fermi_dirac_2_e(0.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_fermi_dirac_2_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_fermi_dirac_2_e',&
       9.015426773696957330D-01,sfres%val,sfres%err)
  ra = fgsl_sf_fermi_dirac_int(-1,0.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_fermi_dirac_int',0.5d0,ra,eps7)
  status = fgsl_sf_fermi_dirac_int_e(-1,0.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_fermi_dirac_int_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_fermi_dirac_int_e',&
       0.5d0,sfres%val,sfres%err)
  ra = fgsl_sf_fermi_dirac_mhalf(0.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_fermi_dirac_mhalf',6.0489864342D-01,&
       ra,eps7)
  status = fgsl_sf_fermi_dirac_mhalf_e(0.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_fermi_dirac_mhalf_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_fermi_dirac_mhalf_e',&
       6.048986434216304664D-01,sfres%val,sfres%err)
  ra = fgsl_sf_fermi_dirac_half(0.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_fermi_dirac_half',7.6514702463D-01,&
       ra,eps7)
  status = fgsl_sf_fermi_dirac_half_e(0.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_fermi_dirac_half_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_fermi_dirac_half_e',&
       7.651470246254079566D-01,sfres%val,sfres%err)
  ra = fgsl_sf_fermi_dirac_3half(0.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_fermi_dirac_3half',8.67199889012D-01,&
       ra,eps7)
  status = fgsl_sf_fermi_dirac_3half_e(0.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_fermi_dirac_3half_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_fermi_dirac_3half_e',&
       8.671998890121841441D-01,sfres%val,sfres%err)
  ra = fgsl_sf_fermi_dirac_inc_0(1.0_fgsl_double,0.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_fermi_dirac_inc_0',&
       log(1+exp(1.0d0)),ra,eps7)
  status = fgsl_sf_fermi_dirac_inc_0_e(1.0_fgsl_double,0.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_fermi_dirac_inc_0_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_fermi_dirac_inc_0_e',&
       log(1+exp(1.0d0)),sfres%val,sfres%err)
!
! Gamma and Beta functions
!
  ra = fgsl_sf_gamma(3.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_gamma',2.d0,ra,eps7)
  status = fgsl_sf_gamma_e(3.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_gamma_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_gamma_e',&
      2.0d0,sfres%val,sfres%err)
  ra = fgsl_sf_lngamma(3.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_lngamma',log(2.d0),ra,eps7)
  status = fgsl_sf_lngamma_e(3.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_lngamma_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_lngamma_e',&
      log(2.0d0),sfres%val,sfres%err)
  ra = fgsl_sf_gammastar(3.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_gammastar',&
       2.d0/sqrt(2*m_pi)/3**2.5d0*exp(3.0d0),ra,eps7)
  status = fgsl_sf_gammastar_e(3.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_gammastar_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_gammastar_e',&
      2.0d00/sqrt(2*m_pi)/3**2.5d0*exp(3.0d0),sfres%val,sfres%err)
  ra = fgsl_sf_gammainv(3.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_gammainv',.5d0,ra,eps7)
  status = fgsl_sf_gammainv_e(3.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_gammainv_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_gammainv_e',&
      .5d0,sfres%val,sfres%err)
  status = fgsl_sf_lngamma_complex_e(3.0_fgsl_double,0.0_fgsl_double, &
       sfres, sfres_2)
  call unit_assert_equal('fgsl_sf_lngamma_complex_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_lngamma_complex_e',&
      log(2.0d0),sfres%val,eps10)
! NOTE: replaced sfres%err by eps10
  call unit_assert_equal_within('fgsl_sf_lngamma_complex_e',&
      0.0d0,sfres_2%val,eps10)
  ra = fgsl_sf_fact(3)
  call unit_assert_equal_within('fgsl_sf_fact',6.d0,ra,eps7)
  status = fgsl_sf_fact_e(3,sfres)
  call unit_assert_equal('fgsl_sf_fact_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_fact_e',&
      6.0d0,sfres%val,sfres%err)
  ra = fgsl_sf_doublefact(3)
  call unit_assert_equal_within('fgsl_sf_doublefact',3.d0,ra,eps7)
  status = fgsl_sf_doublefact_e(3,sfres)
  call unit_assert_equal('fgsl_sf_doublefact_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_doublefact_e',&
      3.0d0,sfres%val,sfres%err)
  ra = fgsl_sf_lnfact(3)
  call unit_assert_equal_within('fgsl_sf_lnfact',log(6.d0),ra,eps7)
  status = fgsl_sf_lnfact_e(3,sfres)
  call unit_assert_equal('fgsl_sf_lnfact_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_lnfact_e',&
      log(6.0d0),sfres%val,sfres%err)
  ra = fgsl_sf_lndoublefact(3)
  call unit_assert_equal_within('fgsl_sf_lndoublefact',log(3.d0),ra,eps7)
  status = fgsl_sf_lndoublefact_e(3,sfres)
  call unit_assert_equal('fgsl_sf_lndoublefact_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_lndoublefact_e',&
      log(3.0d0),sfres%val,sfres%err)
  ra = fgsl_sf_choose(3,0)
  call unit_assert_equal_within('fgsl_sf_choose',1.d0,ra,eps7)
  status = fgsl_sf_choose_e(3,0,sfres)
  call unit_assert_equal('fgsl_sf_choose_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_choose_e',&
      1.0d0,sfres%val,sfres%err)
  ra = fgsl_sf_lnchoose(3,0)
  call unit_assert_equal_within('fgsl_sf_lnchoose',0.d0,ra,eps7)
  status = fgsl_sf_lnchoose_e(3,0,sfres)
  call unit_assert_equal('fgsl_sf_lnchoose_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_lnchoose_e',&
      0.0d0,sfres%val,sfres%err)
  ra = fgsl_sf_taylorcoeff(3,1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_taylorcoeff',1/6.d0,ra,eps7)
  status = fgsl_sf_taylorcoeff_e(3,1.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_taylorcoeff_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_taylorcoeff_e',&
      1/6.0d0,sfres%val,sfres%err)
  ra = fgsl_sf_poch(1.0d0,2.0d0)
  call unit_assert_equal_within('fgsl_sf_poch',2.d0,ra,eps7)
  status = fgsl_sf_poch_e(1.0d0,2.0d0,sfres)
  call unit_assert_equal('fgsl_sf_poch_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_poch_e',&
      2.d0,sfres%val,sfres%err)
  ra = fgsl_sf_lnpoch(1.0d0,2.0d0)
  call unit_assert_equal_within('fgsl_sf_lnpoch',log(2.d0),ra,eps7)
  status = fgsl_sf_lnpoch_e(1.0d0,2.0d0,sfres)
  call unit_assert_equal('fgsl_sf_lnpoch_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_lnpoch_e',&
      log(2.d0),sfres%val,sfres%err)
  status = fgsl_sf_lnpoch_sgn_e(1.0d0,2.0d0,sfres,ra)
  call unit_assert_equal('fgsl_sf_lnpoch_sgn_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_lnpoch_sgn_e',&
      log(2.d0),sfres%val,sfres%err)
  call unit_assert_equal_within('fgsl_sf_lnpoch_sgn_e:sign',&
      1.d0,ra,eps10)
  ra = fgsl_sf_pochrel(1.0d0,2.0d0)
  call unit_assert_equal_within('fgsl_sf_pochrel',.5d0,ra,eps7)
  status = fgsl_sf_pochrel_e(1.0d0,2.0d0,sfres)
  call unit_assert_equal('fgsl_sf_pochrel_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_pochrel_e',&
      .5d0,sfres%val,sfres%err)
  ra = fgsl_sf_gamma_inc(3.0_fgsl_double,0.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_gamma_inc',2.d0,ra,eps7)
  status = fgsl_sf_gamma_inc_e(3.0_fgsl_double,0.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_gamma_inc_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_gamma_inc_e',&
      2.0d0,sfres%val,sfres%err)
  ra = fgsl_sf_gamma_inc_q(3.0_fgsl_double,0.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_gamma_inc_q',1.d0,ra,eps7)
  status = fgsl_sf_gamma_inc_q_e(3.0_fgsl_double,0.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_gamma_inc_q_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_gamma_inc_q_e',&
      1.0d0,sfres%val,sfres%err)
  ra = fgsl_sf_gamma_inc_p(3.0_fgsl_double,0.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_gamma_inc_p',0.d0,ra,eps7)
  status = fgsl_sf_gamma_inc_p_e(3.0_fgsl_double,0.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_gamma_inc_p_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_gamma_inc_p_e',&
      0.0d0,sfres%val,sfres%err)
  ra = fgsl_sf_beta(2.0_fgsl_double,2.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_beta',1/6.d0,ra,eps7)
  status = fgsl_sf_beta_e(2.0_fgsl_double,2.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_beta_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_beta_e',&
      1/6.0d0,sfres%val,sfres%err)
  ra = fgsl_sf_lnbeta(2.0_fgsl_double,2.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_lnbeta',log(1/6.d0),ra,eps7)
  status = fgsl_sf_lnbeta_e(2.0_fgsl_double,2.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_lnbeta_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_lnbeta_e',&
      log(1/6.0d0),sfres%val,sfres%err)
  ra = fgsl_sf_beta_inc(2.0_fgsl_double,2.0_fgsl_double,1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_beta_inc',1.d0,ra,eps7)
  status = fgsl_sf_beta_inc_e(2.0_fgsl_double,2.0_fgsl_double,&
       1.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_beta_inc_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_beta_inc_e',&
       1.0d0,sfres%val,sfres%err)
!
! Gegenbauer functions
!
  ra = fgsl_sf_gegenpoly_1(3.0_fgsl_double,2.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_gegenpoly_1',1.2d1,ra,eps7)
  status = fgsl_sf_gegenpoly_1_e(3.0_fgsl_double,2.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_gegenpoly_1_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_gegenpoly_1_e',&
      1.2d1,sfres%val,sfres%err)
  ra = fgsl_sf_gegenpoly_2(1.0_fgsl_double,1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_gegenpoly_2',3.d0,ra,eps7)
  status = fgsl_sf_gegenpoly_2_e(1.0_fgsl_double,1.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_gegenpoly_2_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_gegenpoly_2_e',&
      3.0d0,sfres%val,sfres%err)
  ra = fgsl_sf_gegenpoly_3(1.0_fgsl_double,1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_gegenpoly_3',4.0d0,ra,eps7)
  status = fgsl_sf_gegenpoly_3_e(1.0_fgsl_double,1.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_gegenpoly_3_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_gegenpoly_3_e',&
      4.0d0,sfres%val,sfres%err)
  ra = fgsl_sf_gegenpoly_n(3,1.0_fgsl_double,1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_gegenpoly_n',4.0d0,ra,eps7)
  status = fgsl_sf_gegenpoly_n_e(3,1.0_fgsl_double,1.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_gegenpoly_n_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_gegenpoly_n_e',&
      4.0d0,sfres%val,sfres%err)
  status = fgsl_sf_gegenpoly_array(1,3.0_fgsl_double,2.0_fgsl_double,ra_arr_3)
  call unit_assert_equal('fgsl_sf_gegenpoly_array:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_gegenpoly_1',1.d0,ra_arr_3(1),eps10)
  call unit_assert_equal_within('fgsl_sf_gegenpoly_1',1.2d1,ra_arr_3(2),eps10)
!
! Hypergeometric functions
!
  ra = fgsl_sf_hyperg_0f1(1.0_fgsl_double,-.25_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_hyperg_0f1',0.765197686557967d0,ra,eps10)
  status = fgsl_sf_hyperg_0f1_e(1.0_fgsl_double,-.25_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_hyperg_0f1_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_hyperg_0f1_e',&
      0.765197686557967d0,sfres%val,sfres%err)
  ra = fgsl_sf_hyperg_1f1_int(2,2,2.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_hyperg_1f1_int',exp(2.d0),ra,eps10)
  status = fgsl_sf_hyperg_1f1_int_e(2,2,2.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_hyperg_1f1_int_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_hyperg_1f1_int_e',&
      exp(2.d0),sfres%val,sfres%err)
  ra = fgsl_sf_hyperg_1f1(2.0_fgsl_double,2.0_fgsl_double,2.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_hyperg_1f1',exp(2.d0),ra,eps10)
  status = fgsl_sf_hyperg_1f1_e(2.0_fgsl_double,2.0_fgsl_double,&
       2.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_hyperg_1f1_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_hyperg_1f1_e',&
      exp(2.d0),sfres%val,sfres%err)
  ra = fgsl_sf_hyperg_u_int(1,1,1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_hyperg_u_int',&
       2.193839343955202859D-01*exp(1.d0),ra,eps10)
  status = fgsl_sf_hyperg_u_int_e(1,1,1.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_hyperg_u_int_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_hyperg_u_int_e',&
      2.193839343955202859D-01*exp(1.d0),sfres%val,sfres%err)
  status = fgsl_sf_hyperg_u_int_e10_e(1,1,1.0_fgsl_double,sfres10)
  call unit_assert_equal('fgsl_sf_hyperg_u_int_e10_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_hyperg_u_int_e10_e',&
      2.193839343955202859D-01*exp(1.d0),sfres10%val,sfres10%err)
  ra = fgsl_sf_hyperg_u(1.0_fgsl_double,1.0_fgsl_double,1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_hyperg_u',&
       2.193839343955202859D-01*exp(1.d0),ra,eps10)
  status = fgsl_sf_hyperg_u_e(1.0_fgsl_double,1.0_fgsl_double,&
       1.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_hyperg_u_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_hyperg_u_e',&
      2.193839343955202859D-01*exp(1.d0),sfres%val,sfres%err)
  status = fgsl_sf_hyperg_u_e10_e(1.0_fgsl_double,&
       1.0_fgsl_double,1.0_fgsl_double,sfres10)
  call unit_assert_equal('fgsl_sf_hyperg_u_e10_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_hyperg_u_e10_e',&
      2.193839343955202859D-01*exp(1.d0),sfres10%val,sfres10%err)
  ra = fgsl_sf_hyperg_2f1(1.0_fgsl_double,1.0_fgsl_double,2.0_fgsl_double, &
       0.5_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_hyperg_2f1',-2*log(0.5d0),ra,eps10)
  status = fgsl_sf_hyperg_2f1_e(1.0_fgsl_double,1.0_fgsl_double,&
       2.0_fgsl_double,0.5_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_hyperg_2f1_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_hyperg_2f1_e',&
       -2*log(0.5d0),sfres%val,sfres%err)
  ra = fgsl_sf_hyperg_2f1_conj(1.0_fgsl_double,0.0_fgsl_double,2.0_fgsl_double, &
       0.5_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_hyperg_2f1_conj',-2*log(0.5d0),ra,eps10)
  status = fgsl_sf_hyperg_2f1_conj_e(1.0_fgsl_double,0.0_fgsl_double,&
       2.0_fgsl_double,0.5_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_hyperg_2f1_conj_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_hyperg_2f1_conj_e',&
       -2*log(0.5d0),sfres%val,sfres%err)
  ra = fgsl_sf_hyperg_2f1_renorm(1.0_fgsl_double,1.0_fgsl_double,2.0_fgsl_double, &
       0.5_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_hyperg_2f1_renorm',-2*log(0.5d0),ra,eps10)
  status = fgsl_sf_hyperg_2f1_renorm_e(1.0_fgsl_double,1.0_fgsl_double,&
       2.0_fgsl_double,0.5_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_hyperg_2f1_renorm_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_hyperg_2f1_renorm_e',&
       -2*log(0.5d0),sfres%val,sfres%err)
  ra = fgsl_sf_hyperg_2f1_conj_renorm(1.0_fgsl_double,0.0_fgsl_double,&
       2.0_fgsl_double,0.5_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_hyperg_2f1_conj_renorm',&
       -2*log(0.5d0),ra,eps10)
  status = fgsl_sf_hyperg_2f1_conj_renorm_e(1.0_fgsl_double,0.0_fgsl_double,&
       2.0_fgsl_double,0.5_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_hyperg_2f1_conj_renorm_e:status',&
       fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_hyperg_2f1_conj_renorm_e',&
       -2*log(0.5d0),sfres%val,sfres%err)
  ra = fgsl_sf_hyperg_2f0(1.0_fgsl_double,1.0_fgsl_double,-1.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_hyperg_2f0',&
       2.193839343955202859D-01*exp(1.d0),ra,eps10)
  status = fgsl_sf_hyperg_2f0_e(1.0_fgsl_double,1.0_fgsl_double,&
       -1.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_hyperg_2f0_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_hyperg_2f0_e',&
      2.193839343955202859D-01*exp(1.d0),sfres%val,sfres%err)
!
! Laguerre functions
!
  ra = fgsl_sf_laguerre_1(0.0_fgsl_double,2.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_laguerre_1',&
       -1.0d0,ra,eps10)
  status = fgsl_sf_laguerre_1_e(0.0_fgsl_double,2.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_laguerre_1_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_laguerre_1_e',&
       -1.d0,sfres%val,sfres%err)
  ra = fgsl_sf_laguerre_2(0.0_fgsl_double,2.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_laguerre_2',&
       -1.0d0,ra,eps10)
  status = fgsl_sf_laguerre_2_e(0.0_fgsl_double,2.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_laguerre_2_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_laguerre_2_e',&
       -1.d0,sfres%val,sfres%err)
  ra = fgsl_sf_laguerre_3(0.0_fgsl_double,2.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_laguerre_3',&
       -1/3.0d0,ra,eps10)
  status = fgsl_sf_laguerre_3_e(0.0_fgsl_double,2.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_laguerre_3_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_laguerre_3_e',&
       -1/3.d0,sfres%val,sfres%err)
  ra = fgsl_sf_laguerre_n(3,0.0_fgsl_double,2.0_fgsl_double)
  call unit_assert_equal_within('fgsl_sf_laguerre_n',&
       -1/3.0d0,ra,eps10)
  status = fgsl_sf_laguerre_n_e(3,0.0_fgsl_double,2.0_fgsl_double,sfres)
  call unit_assert_equal('fgsl_sf_laguerre_n_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_laguerre_n_e',&
       -1/3.d0,sfres%val,sfres%err)
!
! Lambert functions
!
  ra = fgsl_sf_lambert_w0(-1/exp(1.0d0))
  call unit_assert_equal_within('fgsl_sf_lambert_w0',&
       -1.0d0,ra,eps10)
  status = fgsl_sf_lambert_w0_e(-1/exp(1.0d0),sfres)
  call unit_assert_equal('fgsl_sf_lambert_w0_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_lambert_w0_e',&
       -1.d0,sfres%val,sfres%err)
  ra = fgsl_sf_lambert_wm1(-1/exp(1.0d0))
  call unit_assert_equal_within('fgsl_sf_lambert_wm1',&
       -1.0d0,ra,eps10)
  status = fgsl_sf_lambert_wm1_e(-1/exp(1.0d0),sfres)
  call unit_assert_equal('fgsl_sf_lambert_wm1_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_lambert_wm1_e',&
       -1.d0,sfres%val,sfres%err)
!
! Legendre and conical functions
!
  ra = fgsl_sf_legendre_p1(2.0d0)
  call unit_assert_equal_within('fgsl_sf_legendre_p1',2.0d0,ra,eps10)
  status = fgsl_sf_legendre_p1_e(2.0d0,sfres)
  call unit_assert_equal('fgsl_sf_legendre_p1_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_legendre_p1_e',&
       2.d0,sfres%val,sfres%err)
  ra = fgsl_sf_legendre_p2(2.0d0)
  call unit_assert_equal_within('fgsl_sf_legendre_p2',5.5d0,ra,eps10)
  status = fgsl_sf_legendre_p2_e(2.0d0,sfres)
  call unit_assert_equal('fgsl_sf_legendre_p2_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_legendre_p2_e',&
       5.5d0,sfres%val,sfres%err)
  ra = fgsl_sf_legendre_p3(2.0d0)
  call unit_assert_equal_within('fgsl_sf_legendre_p3',17.d0,ra,eps10)
  status = fgsl_sf_legendre_p3_e(2.0d0,sfres)
  call unit_assert_equal('fgsl_sf_legendre_p3_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_legendre_p3_e',&
       17.d0,sfres%val,sfres%err)
  ra = fgsl_sf_legendre_pl(2,1.0d0)
  call unit_assert_equal_within('fgsl_sf_legendre_pl',1.0d0,ra,eps10)
  status = fgsl_sf_legendre_pl_e(2,1.0d0,sfres)
  call unit_assert_equal('fgsl_sf_legendre_pl_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_legendre_pl_e',&
       1.0d0,sfres%val,sfres%err)
  ra = fgsl_sf_legendre_pl_array(1,1.0d0,ra_arr_3)
  call unit_assert_equal_within('fgsl_sf_legendre_pl_array',&
       1.0d0,ra_arr_3(1),eps10)
  call unit_assert_equal_within('fgsl_sf_legendre_pl_array',&
       1.0d0,ra_arr_3(2),eps10)
  ra = fgsl_sf_legendre_pl_deriv_array(1,1.0d0,ra_arr_3,ra_arr_3_der)
  call unit_assert_equal_within('fgsl_sf_legendre_pl_array:0',&
       1.0d0,ra_arr_3(1),eps10)
  call unit_assert_equal_within('fgsl_sf_legendre_pl_array:1',&
       1.0d0,ra_arr_3(2),eps10)
  call unit_assert_equal_within('fgsl_sf_legendre_pl_array:Der0',&
       0.0d0,ra_arr_3_der(1),eps10)
  call unit_assert_equal_within('fgsl_sf_legendre_pl_array:Der1',&
       1.0d0,ra_arr_3_der(2),eps10)
  ra = fgsl_sf_legendre_q0(2.0d0)
  call unit_assert_equal_within('fgsl_sf_legendre_q0',0.5d0*log(3.0d0),ra,eps10)
  status = fgsl_sf_legendre_q0_e(2.0d0,sfres)
  call unit_assert_equal('fgsl_sf_legendre_q0_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_legendre_q0_e',&
       0.5d0*log(3.0d0),sfres%val,sfres%err)
  ra = fgsl_sf_legendre_q1(2.0d0)
  call unit_assert_equal_within('fgsl_sf_legendre_q1',log(3.0d0)-1,ra,eps10)
  status = fgsl_sf_legendre_q1_e(2.0d0,sfres)
  call unit_assert_equal('fgsl_sf_legendre_q1_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_legendre_q1_e',&
       log(3.0d0)-1,sfres%val,sfres%err)
  ra = fgsl_sf_legendre_ql(0,2.0d0)
  call unit_assert_equal_within('fgsl_sf_legendre_ql',0.5d0*log(3.0d0),ra,eps10)
  status = fgsl_sf_legendre_ql_e(0,2.0d0,sfres)
  call unit_assert_equal('fgsl_sf_legendre_ql_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_legendre_ql_e',&
       0.5d0*log(3.0d0),sfres%val,sfres%err)
  ra = fgsl_sf_legendre_plm(2,1,0.5d0)
  call unit_assert_equal_within('fgsl_sf_legendre_plm',-0.75d0*sqrt(3.0d0),ra,eps10)
  status = fgsl_sf_legendre_plm_e(2,1,0.5d0,sfres)
  call unit_assert_equal('fgsl_sf_legendre_plm_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_legendre_plm_e',&
       -0.75d0*sqrt(3.0d0),sfres%val,sfres%err)
  x21=sqrt(5.0d0/(24.0d0*m_pi))
  x11=sqrt(3.0d0/(8.0d0*m_pi))
  ra = fgsl_sf_legendre_sphplm(2,1,0.5d0)
  call unit_assert_equal_within('fgsl_sf_legendre_sphplm',&
       -x21*0.75d0*sqrt(3.0d0),ra,eps10)
  status = fgsl_sf_legendre_sphplm_e(2,1,0.5d0,sfres)
  call unit_assert_equal('fgsl_sf_legendre_sphplm_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_legendre_sphplm_e',&
       -x21*0.75d0*sqrt(3.0d0),sfres%val,sfres%err)
!
! Conical functions:
! need values
  ra = fgsl_sf_conicalp_half(1.0d0,2.0d0)
  call unit_assert_equal_within('fgsl_sf_conicalp_half',&
       0.1522450487467270d0,ra,eps10)
  status = fgsl_sf_conicalp_half_e(1.0d0,2.0d0,sfres)
  call unit_assert_equal('fgsl_sf_conicalp_half_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_conicalp_half_e',&
       0.1522450487467270d0,sfres%val,sfres%err)
  ra = fgsl_sf_conicalp_mhalf(1.0d0,2.0d0)
  call unit_assert_equal_within('fgsl_sf_conicalp_mhalf',&
       0.5868339135394002d0,ra,eps10)
  status = fgsl_sf_conicalp_mhalf_e(1.0d0,2.0d0,sfres)
  call unit_assert_equal('fgsl_sf_conicalp_mhalf_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_conicalp_mhalf_e',&
       0.5868339135394002d0,sfres%val,sfres%err)
  ra = fgsl_sf_conicalp_0(1.0d0,2.0d0)
  call unit_assert_equal_within('fgsl_sf_conicalp_0',&
       0.5564135489350757d0,ra,eps10)
  status = fgsl_sf_conicalp_0_e(1.0d0,2.0d0,sfres)
  call unit_assert_equal('fgsl_sf_conicalp_0_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_conicalp_0_e',&
       0.5564135489350757d0,sfres%val,sfres%err)
  ra = fgsl_sf_conicalp_1(1.0d0,2.0d0)
  call unit_assert_equal_within('fgsl_sf_conicalp_1',&
       -0.5433190759960184d0,ra,eps10)
  status = fgsl_sf_conicalp_1_e(1.0d0,2.0d0,sfres)
  call unit_assert_equal('fgsl_sf_conicalp_1_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_conicalp_1_e',&
       -0.5433190759960184d0,sfres%val,sfres%err)
  ra = fgsl_sf_conicalp_sph_reg(1,1.0d0,2.0d0)
  call unit_assert_equal_within('fgsl_sf_conicalp_sph_reg',&
       0.2626861935782107d0,ra,eps10)
  status = fgsl_sf_conicalp_sph_reg_e(1,1.0d0,2.0d0,sfres)
  call unit_assert_equal('fgsl_sf_conicalp_sph_reg_e:status',&
       fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_conicalp_sph_reg_e',&
       0.2626861935782107d0,sfres%val,sfres%err)
  ra = fgsl_sf_conicalp_cyl_reg(1,1.0d0,2.0d0)
  call unit_assert_equal_within('fgsl_sf_conicalp_cyl_reg',&
       0.4346552607968133d0,ra,eps10)
  status = fgsl_sf_conicalp_cyl_reg_e(1,1.0d0,2.0d0,sfres)
  call unit_assert_equal('fgsl_sf_conicalp_cyl_reg_e:status',&
       fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_conicalp_cyl_reg_e',&
       0.4346552607968133d0,sfres%val,sfres%err)
  ra = fgsl_sf_legendre_h3d_0(1.0d0,2.0d0)
  call unit_assert_equal_within('fgsl_sf_legendre_h3d_0',&
       0.2507120000699062d0,ra,eps10)
  status = fgsl_sf_legendre_h3d_0_e(1.0d0,2.0d0,sfres)
  call unit_assert_equal('fgsl_sf_legendre_h3d_0_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_legendre_h3d_0_e',&
       0.2507120000699062d0,sfres%val,sfres%err)
  ra = fgsl_sf_legendre_h3d_1(1.0d0,2.0d0)
  call unit_assert_equal_within('fgsl_sf_legendre_h3d_1',&
       0.2650289172078014d0,ra,eps10)
  status = fgsl_sf_legendre_h3d_1_e(1.0d0,2.0d0,sfres)
  call unit_assert_equal('fgsl_sf_legendre_h3d_1_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_legendre_h3d_1_e',&
       0.2650289172078014d0,sfres%val,sfres%err)
  ra = fgsl_sf_legendre_h3d(1,1.0d0,2.0d0)
  call unit_assert_equal_within('fgsl_sf_legendre_h3d',&
       0.2650289172078014d0,ra,eps10)
  status = fgsl_sf_legendre_h3d_e(1,1.0d0,2.0d0,sfres)
  call unit_assert_equal('fgsl_sf_legendre_h3d_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_legendre_h3d_e',&
       0.2650289172078014d0,sfres%val,sfres%err)
  status = fgsl_sf_legendre_h3d_array(1,1.0d0,2.0d0,ra_arr_3)
  call unit_assert_equal('fgsl_sf_legendre_h3d_0_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_legendre_h3d_0',&
       0.2507120000699062d0,ra_arr_3(1),eps10)
  call unit_assert_equal_within('fgsl_sf_legendre_h3d_e',&
       0.2650289172078014d0,ra_arr_3(2),eps10)
!
! Some standard functions
!
  ra = fgsl_sf_log(2.0d0)
  call unit_assert_equal_within('fgsl_sf_log',&
       log(2.0d0),ra,eps10)
  status = fgsl_sf_log_e(2.0d0,sfres)
  call unit_assert_equal('fgsl_sf_log_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_log_e',&
       log(2.0d0),sfres%val,sfres%err)
  status = fgsl_sf_complex_log_e(2.0d0,0.0d0,sfres,sfres_2)
  call unit_assert_equal('fgsl_sf_complex_log_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_complex_log_e',&
       log(2.0d0),sfres%val,eps10)
  call unit_assert_equal_within('fgsl_sf_complex_log_e',&
       0.0d0,sfres_2%val,eps10)
  ra = fgsl_sf_log_abs(2.0d0)
  call unit_assert_equal_within('fgsl_sf_log_abs',&
       abs(log(2.0d0)),ra,eps10)
  status = fgsl_sf_log_abs_e(2.0d0,sfres)
  call unit_assert_equal('fgsl_sf_log_abs_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_log_abs_e',&
       abs(log(2.0d0)),sfres%val,sfres%err)
  ra = fgsl_sf_log_1plusx(1.0d0)
  call unit_assert_equal_within('fgsl_sf_log_1plusx',&
       log(2.0d0),ra,eps10)
  status = fgsl_sf_log_1plusx_e(1.0d0,sfres)
  call unit_assert_equal('fgsl_sf_log_1plusx_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_log_1plusx_e',&
       log(2.0d0),sfres%val,sfres%err)
  ra = fgsl_sf_log_1plusx_mx(1.0d0)
  call unit_assert_equal_within('fgsl_sf_log_1plusx_mx',&
       log(2.0d0)-1,ra,eps10)
  status = fgsl_sf_log_1plusx_mx_e(1.0d0,sfres)
  call unit_assert_equal('fgsl_sf_log_1plusx_mx_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_log_1plusx_mx_e',&
       log(2.0d0)-1,sfres%val,sfres%err)
  ra = fgsl_sf_log(2.0d0)
  call unit_assert_equal_within('fgsl_sf_log',&
       log(2.0d0),ra,eps10)
!
! Digamma, Trigamma, Polygamma
!
  ra = fgsl_sf_psi_int(2)
  call unit_assert_equal_within('fgsl_sf_psi_int',&
       0.4227843350984671d0,ra,eps10)
  status = fgsl_sf_psi_int_e(2,sfres)
  call unit_assert_equal('fgsl_sf_psi_int_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_psi_int_e',&
       0.4227843350984671d0,sfres%val,sfres%err)
  ra = fgsl_sf_psi(2.0d0)
  call unit_assert_equal_within('fgsl_sf_psi',&
       0.4227843350984671d0,ra,eps10)
  status = fgsl_sf_psi_e(2.0d0,sfres)
  call unit_assert_equal('fgsl_sf_psi_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_psi_e',&
       0.4227843350984671d0,sfres%val,sfres%err)
  ra = fgsl_sf_psi_1piy(2.0d0)
  call unit_assert_equal_within('fgsl_sf_psi_1piy',&
       0.7145915153739806d0,ra,eps10)
  status = fgsl_sf_psi_1piy_e(2.0d0,sfres)
  call unit_assert_equal('fgsl_sf_psi_1piy_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_psi_1piy_e',&
       0.7145915153739806d0,sfres%val,sfres%err)
  ra = fgsl_sf_psi_1_int(2)
  call unit_assert_equal_within('fgsl_sf_psi_1_int',&
       0.6449340668482265d0,ra,eps10)
  status = fgsl_sf_psi_1_int_e(2,sfres)
  call unit_assert_equal('fgsl_sf_psi_1_int_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_psi_1_int_e',&
       0.6449340668482265d0,sfres%val,sfres%err)
  ra = fgsl_sf_psi_1(2.0d0)
  call unit_assert_equal_within('fgsl_sf_psi_1',&
       0.6449340668482265d0,ra,eps10)
  status = fgsl_sf_psi_1_e(2.0d0,sfres)
  call unit_assert_equal('fgsl_sf_psi_1_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_psi_1_e',&
       0.6449340668482265d0,sfres%val,sfres%err)
  ra = fgsl_sf_psi_n(1,2.0d0)
  call unit_assert_equal_within('fgsl_sf_psi_n',&
       0.6449340668482265d0,ra,eps10)
  status = fgsl_sf_psi_n_e(1,2.0d0,sfres)
  call unit_assert_equal('fgsl_sf_psi_n_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_psi_n_e',&
       0.6449340668482265d0,sfres%val,sfres%err)
!
! Synchrotron and Transport
!
  ra = fgsl_sf_synchrotron_1(2.0d0)
  call unit_assert_equal_within('fgsl_sf_synchrotron_1',&
       0.3016359028507383d0,ra,eps10)
  status = fgsl_sf_synchrotron_1_e(2.0d0,sfres)
  call unit_assert_equal('fgsl_sf_synchrotron_1_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_synchrotron_1_e',&
       0.3016359028507383d0,sfres%val,sfres%err)
  ra = fgsl_sf_synchrotron_2(2.0d0)
  call unit_assert_equal_within('fgsl_sf_synchrotron_2',&
       0.2496778549762562d0,ra,eps10)
  status = fgsl_sf_synchrotron_2_e(2.0d0,sfres)
  call unit_assert_equal('fgsl_sf_synchrotron_2_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_synchrotron_2_e',&
       0.2496778549762562d0,sfres%val,sfres%err)
  ra = fgsl_sf_transport_2(2.0d0)
  call unit_assert_equal_within('fgsl_sf_transport_2',&
       1.8017185674405778d0,ra,eps10)
  status = fgsl_sf_transport_2_e(2.0d0,sfres)
  call unit_assert_equal('fgsl_sf_transport_2_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_transport_2_e',&
       1.8017185674405778d0,sfres%val,sfres%err)
  ra = fgsl_sf_transport_3(2.0d0)
  call unit_assert_equal_within('fgsl_sf_transport_3',&
       1.7063547219458659d0,ra,eps10)
  status = fgsl_sf_transport_3_e(2.0d0,sfres)
  call unit_assert_equal('fgsl_sf_transport_3_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_transport_3_e',&
       1.7063547219458659d0,sfres%val,sfres%err)
  ra = fgsl_sf_transport_4(2.0d0)
  call unit_assert_equal_within('fgsl_sf_transport_4',&
       2.2010881024333409d0,ra,eps10)
  status = fgsl_sf_transport_4_e(2.0d0,sfres)
  call unit_assert_equal('fgsl_sf_transport_4_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_transport_4_e',&
       2.2010881024333409d0,sfres%val,sfres%err)
  ra = fgsl_sf_transport_5(2.0d0)
  call unit_assert_equal_within('fgsl_sf_transport_5',&
       3.2292901663684050d0,ra,eps10)
  status = fgsl_sf_transport_5_e(2.0d0,sfres)
  call unit_assert_equal('fgsl_sf_transport_5_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_transport_5_e',&
       3.2292901663684050d0,sfres%val,sfres%err)
!
! Trigonometric functions
! not available as Fortran intrinsics
  ra = fgsl_sf_hypot(3.0d0,4.0d0)
  call unit_assert_equal_within('fgsl_sf_hypot',&
       5.0d0,ra,eps10)
  status = fgsl_sf_hypot_e(3.0d0,4.0d0,sfres)
  call unit_assert_equal('fgsl_sf_hypot_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_hypot_e',&
       5.0d0,sfres%val,sfres%err)
  ra = fgsl_sf_sinc(0.5d0)
  call unit_assert_equal_within('fgsl_sf_sinc',&
       2/m_pi,ra,eps10)
  status = fgsl_sf_sinc_e(0.5d0,sfres)
  call unit_assert_equal('fgsl_sf_sinc_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_sinc_e',&
       2/m_pi,sfres%val,sfres%err)
  status = fgsl_sf_complex_sin_e(m_pi/2,0.0d0,sfres,sfres_2)
  call unit_assert_equal('fgsl_sf_complex_sin_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_complex_sin_e',&
       1.0d0,sfres%val,eps10)
  call unit_assert_equal_within('fgsl_sf_complex_sin_e',&
       0.0d0,sfres_2%val,eps10)
  status = fgsl_sf_complex_cos_e(m_pi,0.0d0,sfres,sfres_2)
  call unit_assert_equal('fgsl_sf_complex_cos_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_complex_cos_e',&
       -1.0d0,sfres%val,eps10)
  call unit_assert_equal_within('fgsl_sf_complex_cos_e',&
       0.0d0,sfres_2%val,eps10)
  status = fgsl_sf_complex_logsin_e(m_pi/2,0.0d0,sfres,sfres_2)
  call unit_assert_equal('fgsl_sf_complex_logsin_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_complex_logsin_e',&
       0.0d0,sfres%val,eps10)
  call unit_assert_equal_within('fgsl_sf_complex_logsin_e',&
       0.0d0,sfres_2%val,eps10)
  ra = fgsl_sf_lnsinh(0.5d0)
  call unit_assert_equal_within('fgsl_sf_lnsinh',&
       log(sinh(0.5d0)),ra,eps10)
  status = fgsl_sf_lnsinh_e(0.5d0,sfres)
  call unit_assert_equal('fgsl_sf_lnsinh_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_lnsinh_e',&
       log(sinh(0.5d0)),sfres%val,eps15)
  ra = fgsl_sf_lncosh(0.5d0)
  call unit_assert_equal_within('fgsl_sf_lncosh',&
       log(cosh(0.5d0)),ra,eps10)
  status = fgsl_sf_lncosh_e(0.5d0,sfres)
  call unit_assert_equal('fgsl_sf_lncosh_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_lncosh_e',&
       log(cosh(0.5d0)),sfres%val,eps15)
  status = fgsl_sf_polar_to_rect(1.0d0,m_pi/2,sfres,sfres_2)
  call unit_assert_equal('fgsl_sf_polar_to_rect_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_polar_to_rect_e',&
       0.0d0,sfres%val,eps10)
  call unit_assert_equal_within('fgsl_sf_polar_to_rect_e',&
       1.0d0,sfres_2%val,eps10)
  status = fgsl_sf_rect_to_polar(0.0d0,1.0d0,sfres,sfres_2)
  call unit_assert_equal('fgsl_sf_rect_to_polar_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_rect_to_polar_e',&
       1.0d0,sfres%val,eps10)
  call unit_assert_equal_within('fgsl_sf_rect_to_polar_e',&
       m_pi/2,sfres_2%val,eps10)
  ra = fgsl_sf_angle_restrict_symm(-1.5d0*m_pi)
  call unit_assert_equal_within('fgsl_sf_angle_restrict_symm',&
       m_pi/2,ra,eps10)
  ra = -1.5d0*m_pi
  status = fgsl_sf_angle_restrict_symm_e(ra)
  call unit_assert_equal('fgsl_sf_angle_restrict_symm_e:status',&
       fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_angle_restrict_symm_e',&
       m_pi/2,ra,eps10)
  ra = fgsl_sf_angle_restrict_pos(-1.5d0*m_pi)
  call unit_assert_equal_within('fgsl_sf_angle_restrict_pos',&
       m_pi/2,ra,eps10)
  ra = -1.5d0*m_pi
  status = fgsl_sf_angle_restrict_pos_e(ra)
  call unit_assert_equal('fgsl_sf_angle_restrict_pos_e:status',&
       fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_angle_restrict_pos_e',&
       m_pi/2,ra,eps10)
  status = fgsl_sf_sin_err_e(m_pi/2,eps7,sfres)
  call unit_assert_equal('fgsl_sf_sin_err_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_sin_err_e',&
       1.0d0,sfres%val,sfres%err)
  status = fgsl_sf_cos_err_e(m_pi,eps7,sfres)
  call unit_assert_equal('fgsl_sf_cos_err_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_cos_err_e',&
       -1.0d0,sfres%val,sfres%err)
!
! Zeta functions
!
  ra = fgsl_sf_zeta_int(2)
  call unit_assert_equal_within('fgsl_sf_zeta_int',&
       1.644934066848226406d0,ra,eps10)
  status = fgsl_sf_zeta_int_e(2,sfres)
  call unit_assert_equal('fgsl_sf_zeta_int_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_zeta_int_e',&
       1.644934066848226406d0,sfres%val,sfres%err)
! NOTE: inconsistency between int and dble results (sfres%err too small!)
  ra = fgsl_sf_zeta(2.0d0)
  call unit_assert_equal_within('fgsl_sf_zeta',&
       1.644934066848227516d0,ra,eps10)
  status = fgsl_sf_zeta_e(2.0d0,sfres)
  call unit_assert_equal('fgsl_sf_zeta_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_zeta_e',&
       1.644934066848227516d0,sfres%val,sfres%err)
  ra = fgsl_sf_zetam1_int(2)
  call unit_assert_equal_within('fgsl_sf_zetam1_int',&
       0.644934066848226406d0,ra,eps10)
  status = fgsl_sf_zetam1_int_e(2,sfres)
  call unit_assert_equal('fgsl_sf_zetam1_int_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_zetam1_int_e',&
       0.644934066848226406d0,sfres%val,sfres%err)
! NOTE: inconsistency between int and dble results (sfres%err too small!)
  ra = fgsl_sf_zetam1(2.0d0)
  call unit_assert_equal_within('fgsl_sf_zetam1',&
       0.644934066848227516d0,ra,eps10)
  status = fgsl_sf_zetam1_e(2.0d0,sfres)
  call unit_assert_equal('fgsl_sf_zetam1_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_zetam1_e',&
       0.644934066848227516d0,sfres%val,sfres%err)
  ra = fgsl_sf_hzeta(2.0d0,1.0d0)
  call unit_assert_equal_within('fgsl_sf_hzeta',&
       1.644934066848227516d0,ra,eps10)
  status = fgsl_sf_hzeta_e(2.0d0,1.0d0,sfres)
  call unit_assert_equal('fgsl_sf_hzeta_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_hzeta_e',&
       1.644934066848227516d0,sfres%val,sfres%err)
  ra = fgsl_sf_eta_int(2)
  call unit_assert_equal_within('fgsl_sf_eta_int',&
       0.5d0*1.644934066848226406d0,ra,eps10)
  status = fgsl_sf_eta_int_e(2,sfres)
  call unit_assert_equal('fgsl_sf_eta_int_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_eta_int_e',&
       0.5d0*1.644934066848226406d0,sfres%val,sfres%err)
! NOTE: inconsistency between int and dble results (sfres%err too small!)
  ra = fgsl_sf_eta(2.0d0)
  call unit_assert_equal_within('fgsl_sf_eta',&
       0.5d0*1.644934066848227516d0,ra,eps10)
  status = fgsl_sf_eta_e(2.0d0,sfres)
  call unit_assert_equal('fgsl_sf_eta_e:status',fgsl_success,status)
  call unit_assert_equal_within('fgsl_sf_eta_e',&
       0.5d0*1.644934066848227516d0,sfres%val,sfres%err)
!
! Done
!
  call unit_finalize()
end program specfunc
