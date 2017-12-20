  module variableKind
    !! Defines variable kinds
    use, intrinsic :: iso_fortran_env, only: i8=>int8, i16=>int16, i32=>int32, i64=>int64
    use, intrinsic :: iso_fortran_env, only: r32=>real32, r64=>real64
    integer(i32), parameter :: cLen = 1024 !! Default character length for temporaries
  end module
