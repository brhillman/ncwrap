module ncwrap
  ! Overload read routines
  interface read_field
    module procedure &
          read_char_vec, read_logical_vec, &
          read_real_scalar, &
          read_real_1d_field, read_real_2d_field, &
          read_real_3d_field, read_real_4d_field, &
          read_integer_scalar, &
          read_integer_1d_field, read_integer_2d_field, &
          read_integer_3d_field, read_integer_4d_field
  end interface

contains

  subroutine stop_on_err(msg)
    use iso_fortran_env, only : error_unit
    character(len=*), intent(in) :: msg

    if(msg /= "") then
      write(error_unit, *) msg
      stop
    end if
  end subroutine

subroutine read_integer_scalar(ncid, varName, vardata)
    integer,          intent(in) :: ncid
    character(len=*), intent(in) :: varName
    integer, intent(inout)       :: vardata 

    integer :: varid

    if(nf90_inq_varid(ncid, trim(varName), varid) /= NF90_NOERR) &
      call stop_on_err("read_field: can't find variable " // trim(varName))
    if(nf90_get_var(ncid, varid, vardata)  /= NF90_NOERR) &
      call stop_on_err("read_field: can't read variable " // trim(varName))

  end subroutine read_integer_scalar
  !--------------------------------------------------------------------------------------------------------------------
  subroutine read_integer_1d_field(ncid, varName, vardata)
    integer,          intent(in) :: ncid
    character(len=*), intent(in) :: varName
    integer, allocatable, intent(inout)  :: vardata(:)

    integer :: varsizes(1)
    integer :: varid

    ! Check sizes and allocate if needed
    varsizes = get_data_size(ncid, varName, 1)
    if (allocated(vardata)) then
       if (any(shape(vardata) /= varsizes)) then
          call stop_on_err("read_field: variable " // trim(varName) // " size is inconsistent." )
       end if
    else
       allocate(vardata(varsizes(1)))
    end if

    ! Get variable ID from name
    if(nf90_inq_varid(ncid, trim(varName), varid) /= NF90_NOERR) &
      call stop_on_err("read_field: can't find variable " // trim(varName))

    ! Read data
    if(nf90_get_var(ncid, varid, vardata)  /= NF90_NOERR) &
      call stop_on_err("read_field: can't read variable " // trim(varName))

  end subroutine read_integer_1d_field
  !--------------------------------------------------------------------------------------------------------------------
  subroutine read_integer_2d_field(ncid, varName, vardata)
    integer, intent(in) :: ncid
    character(len=*), intent(in) :: varName
    integer, allocatable, intent(inout) :: vardata(:,:)

    integer :: varsizes(2)
    integer :: varid

    ! Check sizes and allocate if needed
    varsizes = get_data_size(ncid, varName, 2)
    if (allocated(vardata)) then
       if (any(shape(vardata) /= varsizes)) then
          call stop_on_err("read_field: variable " // trim(varName) // " size is inconsistent." )
       end if
    else
       allocate(vardata(varsizes(1), varsizes(2)))
    end if

    ! Get variable ID from name
    if(nf90_inq_varid(ncid, trim(varName), varid) /= NF90_NOERR) &
      call stop_on_err("read_field: can't find variable " // trim(varName))

    ! Read data
    if(nf90_get_var(ncid, varid, vardata)  /= NF90_NOERR) &
      call stop_on_err("read_field: can't read variable " // trim(varName))

  end subroutine read_integer_2d_field
  !--------------------------------------------------------------------------------------------------------------------
  subroutine read_integer_3d_field(ncid, varName, vardata)
    integer, intent(in) :: ncid
    character(len=*), intent(in) :: varName
    integer, allocatable, intent(inout) :: vardata(:,:,:)

    integer :: varsizes(3)
    integer :: varid

    ! Check sizes and allocate if needed
    varsizes = get_data_size(ncid, varName, 3)
    if (allocated(vardata)) then
       if (any(shape(vardata) /= varsizes)) then
          print *, 'shape(vardata) = ', shape(vardata), 'varsizes = ', varsizes
          call stop_on_err("read_field: variable " // trim(varName) // " size is inconsistent." )
       end if
    else
       allocate(vardata(varsizes(1), varsizes(2), varsizes(3)))
    end if

    ! Get variable ID from name
    if(nf90_inq_varid(ncid, trim(varName), varid) /= NF90_NOERR) &
      call stop_on_err("read_field: can't find variable " // trim(varName))

    ! Read data
    if(nf90_get_var(ncid, varid, vardata)  /= NF90_NOERR) &
      call stop_on_err("read_field: can't read variable " // trim(varName))

  end subroutine read_integer_3d_field
  !--------------------------------------------------------------------------------------------------------------------
  subroutine read_integer_4d_field(ncid, varName, vardata)
    integer, intent(in) :: ncid
    character(len=*), intent(in) :: varName
    integer, allocatable, intent(inout) :: vardata(:,:,:,:)

    integer :: varsizes(4)
    integer :: varid

    ! Check sizes and allocate if needed
    varsizes = get_data_size(ncid, varName, 4)
    if (allocated(vardata)) then
       if (any(shape(vardata) /= varsizes)) then
          call stop_on_err("read_field: variable " // trim(varName) // " size is inconsistent." )
       end if
    else
       allocate(vardata(varsizes(1), varsizes(2), varsizes(3), varsizes(4)))
    end if

    ! Get variable ID from name
    if(nf90_inq_varid(ncid, trim(varName), varid) /= NF90_NOERR) &
      call stop_on_err("read_field: can't find variable " // trim(varName))

    ! Read data
    if(nf90_get_var(ncid, varid, vardata)  /= NF90_NOERR) &
      call stop_on_err("read_field: can't read variable " // trim(varName))

  end subroutine read_integer_4d_field
  !--------------------------------------------------------------------------------------------------------------------
  subroutine read_real_scalar(ncid, varName, vardata)
    integer, intent(in) :: ncid
    character(len=*), intent(in) :: varName
    real(wp), intent(inout) :: vardata 

    integer :: varid

    if(nf90_inq_varid(ncid, trim(varName), varid) /= NF90_NOERR) &
      call stop_on_err("read_field: can't find variable " // trim(varName))
    if(nf90_get_var(ncid, varid, vardata)  /= NF90_NOERR) &
      call stop_on_err("read_field: can't read variable " // trim(varName))

  end subroutine read_real_scalar
  !--------------------------------------------------------------------------------------------------------------------
  subroutine read_real_1d_field(ncid, varName, vardata)
    integer, intent(in) :: ncid
    character(len=*), intent(in) :: varName
    real(wp), allocatable, intent(inout) :: vardata(:)

    integer :: varsizes(1)
    integer :: varid

    ! Check sizes and allocate if needed
    varsizes = get_data_size(ncid, varName, 1)
    if (allocated(vardata)) then
       if (any(shape(vardata) /= varsizes)) then
          call stop_on_err("read_field: variable " // trim(varName) // " size is inconsistent." )
       end if
    else
       allocate(vardata(varsizes(1)))
    end if

    ! Get variable ID from name
    if(nf90_inq_varid(ncid, trim(varName), varid) /= NF90_NOERR) &
      call stop_on_err("read_field: can't find variable " // trim(varName))

    ! Read data
    if(nf90_get_var(ncid, varid, vardata)  /= NF90_NOERR) &
      call stop_on_err("read_field: can't read variable " // trim(varName))

  end subroutine read_real_1d_field
  !--------------------------------------------------------------------------------------------------------------------
  subroutine read_real_2d_field(ncid, varName, vardata)
    integer, intent(in) :: ncid
    character(len=*), intent(in) :: varName
    real(wp), allocatable, intent(inout) :: vardata(:,:)

    integer :: varsizes(2)
    integer :: varid

    ! Check sizes and allocate if needed
    varsizes = get_data_size(ncid, varName, 2)
    if (allocated(vardata)) then
       if (any(shape(vardata) /= varsizes)) then
          call stop_on_err("read_field: variable " // trim(varName) // " size is inconsistent." )
       end if
    else
       allocate(vardata(varsizes(1), varsizes(2)))
    end if

    ! Get variable ID from name
    if(nf90_inq_varid(ncid, trim(varName), varid) /= NF90_NOERR) &
      call stop_on_err("read_field: can't find variable " // trim(varName))

    ! Read data
    if(nf90_get_var(ncid, varid, vardata)  /= NF90_NOERR) &
      call stop_on_err("read_field: can't read variable " // trim(varName))

  end subroutine read_real_2d_field
  !--------------------------------------------------------------------------------------------------------------------
  subroutine read_real_3d_field(ncid, varName, vardata)
    integer, intent(in) :: ncid
    character(len=*), intent(in) :: varName
    real(wp), allocatable, intent(inout) :: vardata(:,:,:)

    integer :: varsizes(3)
    integer :: varid

    ! Check sizes and allocate if needed
    varsizes = get_data_size(ncid, varName, 3)
    if (allocated(vardata)) then
       if (any(shape(vardata) /= varsizes)) then
          call stop_on_err("read_field: variable " // trim(varName) // " size is inconsistent." )
       end if
    else
       allocate(vardata(varsizes(1), varsizes(2), varsizes(3)))
    end if

    ! Get variable ID from name
    if(nf90_inq_varid(ncid, trim(varName), varid) /= NF90_NOERR) &
      call stop_on_err("read_field: can't find variable " // trim(varName))

    ! Read data
    if(nf90_get_var(ncid, varid, vardata)  /= NF90_NOERR) &
      call stop_on_err("read_field: can't read variable " // trim(varName))

  end subroutine read_real_3d_field
  !--------------------------------------------------------------------------------------------------------------------
  subroutine read_real_4d_field(ncid, varName, vardata)
    integer, intent(in) :: ncid
    character(len=*), intent(in) :: varName
    real(wp), allocatable, intent(inout) :: vardata(:,:,:,:)

    integer :: varsizes(4)
    integer :: varid

    ! Check sizes and allocate if needed
    varsizes = get_data_size(ncid, varName, 4)
    if (allocated(vardata)) then
       if (any(shape(vardata) /= varsizes)) then
          call stop_on_err("read_field: variable " // trim(varName) // " size is inconsistent." )
       end if
    else
       allocate(vardata(varsizes(1), varsizes(2), varsizes(3), varsizes(4)))
    end if

    ! Get variable ID from name
    if(nf90_inq_varid(ncid, trim(varName), varid) /= NF90_NOERR) &
      call stop_on_err("read_field: can't find variable " // trim(varName))

    ! Read data
    if(nf90_get_var(ncid, varid, vardata)  /= NF90_NOERR) &
      call stop_on_err("read_field: can't read variable " // trim(varName))

  end subroutine read_real_4d_field
  !--------------------------------------------------------------------------------------------------------------------
  subroutine read_logical_vec(ncid, varName, vardata)
    integer, intent(in) :: ncid
    character(len=*), intent(in) :: varName
    logical, allocatable, intent(inout) :: vardata(:)
    integer, allocatable :: vardata_tmp(:)

    integer :: varid
    integer :: nx, ix
    integer :: var_sizes(1)

    ! Get variable ID from name
    if(nf90_inq_varid(ncid, trim(varName), varid) /= NF90_NOERR) &
      call stop_on_err("read_logical_vec: can't find variable " // trim(varName))

    ! Get variable dimension sizes
    var_sizes = get_data_size(ncid, varName, 1)

    ! Allocate space for temporary variable
    allocate(vardata_tmp(var_sizes(1)))

    ! Read temporary variable
    if(nf90_get_var(ncid, varid, vardata_tmp)  /= NF90_NOERR) &
      call stop_on_err("read_logical_vec: can't read variable " // trim(varName))

    ! Check if vardata is already allocated; if it is, check sizes. If not,
    ! allocate now.
    if (allocated(vardata)) then
       if (any(shape(vardata) /= var_sizes)) then
          call stop_on_err("read_logical_vec: inconsistent sizes for " // trim(varName))
       end if
    else
       allocate(vardata(var_sizes(1)))
    end if

    ! Convert temporary variable to logical
    do ix = 1, var_sizes(1)
      if (vardata_tmp(ix) .eq. 0) then
        vardata(ix) = .false.
      else
        vardata(ix) = .true.
      endif
    enddo

    deallocate(vardata_tmp)
  end subroutine read_logical_vec
  !--------------------------------------------------------------------------------------------------------------------
  subroutine read_char_vec(ncid, varName, vardata)
    integer, intent(in) :: ncid
    character(len=*), intent(in) :: varName
    character(len=*), allocatable, intent(inout) :: vardata(:)

    ! var_sizes needs to be length 2, because one dimension is reserved for the
    ! character length.
    integer :: var_sizes(2)
    integer :: varid

    ! Get variable ID from name
    if(nf90_inq_varid(ncid, trim(varName), varid) /= NF90_NOERR) then
      call stop_on_err("read_char_vec: can't find variable " // trim(varName))
    end if

    ! Get dimension sizes; 1st dimension is string length
    var_sizes = get_data_size(ncid, varName, 2)

    ! Check if variable is allocated; if not, allocate. If allocated, check that
    ! sizes match variable sizes on disk. Note that var_sizes(1) is the string
    ! length, so we want to compare against var_sizes(2).
    if (allocated(vardata)) then
       if (size(vardata, 1) /= var_sizes(2)) then
          call stop_on_err("read_char_vec: inconsistent sizes for " // trim(varName))
       end if
    else
       allocate(vardata(var_sizes(2)))
    end if

    ! Finally, read data
    if(nf90_get_var(ncid, varid, vardata)  /= NF90_NOERR) &
      call stop_on_err("read_char_vec: can't read variable " // trim(varName))

  end subroutine read_char_vec
  !--------------------------------------------------------------------------------------------------------------------
  function var_exists(ncid, varName)
    !
    ! Does this variable exist (have a valid var_id) in the open netCDF file?
    !
    integer, intent(in) :: ncid
    character(len=*), intent(in) :: varName
    logical :: var_exists

    integer :: varId
    var_exists = nf90_inq_varid(ncid, trim(varName), varid) == NF90_NOERR
  end function var_exists
  !--------------------------------------------------------------------------------------------------------------------
  function get_dim_length(ncid, dimname)
    !
    ! Get the length of a dimension from an open netCDF file
    !  This is unfortunately a two-step process
    !
    integer, intent(in) :: ncid
    character(len=*), intent(in) :: dimname
    integer :: get_dim_length

    integer :: dimid

    if(nf90_inq_dimid(ncid, trim(dimname), dimid) == NF90_NOERR) then
      if(nf90_inquire_dimension(ncid, dimid, len=get_dim_length) /= NF90_NOERR) get_dim_length = 0
    else
      get_dim_length = 0
    end if

  end function get_dim_length
  !--------------------------------------------------------------------------------------------------------------------
  function get_data_size(ncid, varName, n)
    !
    ! Returns the extents of a netcdf variable on disk
    !
    integer, intent(in) :: ncid
    character(len=*), intent(in) :: varName
    integer, intent(in) :: n
    integer :: get_data_size(n)

    integer :: i
    integer :: varid, ndims, dimids(n)

    get_data_size(n) = -1
    if(nf90_inq_varid(ncid, trim(varName), varid) /= NF90_NOERR) &
      call stop_on_err("get_data_size: can't find variable " // trim(varName))
    if(nf90_inquire_variable(ncid, varid, ndims = ndims) /= NF90_NOERR) &
      call stop_on_err("get_data_size: can't get information for variable " // trim(varName))
    if(ndims /= n) &
      call stop_on_err("get_data_size:  variable " // trim(varName) // " has the wrong number of dimensions" )
    if(nf90_inquire_variable(ncid, varid, dimids = dimids) /= NF90_NOERR) &
      call stop_on_err("get_data_size: can't read dimension ids for variable " // trim(varName))
    do i = 1, n
      if(nf90_inquire_dimension(ncid, dimids(i), len = get_data_size(i)) /= NF90_NOERR) &
        call stop_on_err("get_data_size: can't get dimension lengths for variable " // trim(varName))
    end do

  end function get_data_size
  !--------------------------------------------------------------------------------------------------------------------

end module
end module
