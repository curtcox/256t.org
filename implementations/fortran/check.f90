program cid_check
  use, intrinsic :: iso_fortran_env, only: int8, int32, int64
  implicit none

  character(len=*), parameter :: cids_dir = 'cids'
  character(len=*), parameter :: list_file = '/tmp/fortran-cid-files.txt'
  character(len=*), parameter :: hash_file = '/tmp/fortran-sha512.txt'

  call execute_command_line('ls ' // cids_dir // ' > ' // list_file)

  call process_files()

contains

  subroutine process_files()
    character(len=200) :: line
    character(len=:), allocatable :: expected, actual
    integer :: ios, list_unit, mismatch_count, total

    mismatch_count = 0
    total = 0

    open(newunit=list_unit, file=list_file, status='old', action='read', iostat=ios)
    if (ios /= 0) then
      write(*,'(A)') 'Unable to read CID list.'
      stop 1
    end if

    do
      read(list_unit, '(A)', iostat=ios) line
      if (ios /= 0) exit
      actual = trim(line)
      if (len_trim(actual) == 0) cycle
      total = total + 1

      expected = compute_cid(cids_dir // '/' // actual)

      if (expected /= actual) then
        mismatch_count = mismatch_count + 1
        if (len_trim(expected) == 0) then
          write(*,'(A)') '- ' // trim(actual) // ' could not be validated'
        else
          write(*,'(A)') '- ' // trim(actual) // ' should be ' // trim(expected)
        end if
      end if
    end do
    close(list_unit)

    call execute_command_line('rm -f ' // list_file)
    call execute_command_line('rm -f ' // hash_file)

    if (mismatch_count == 0) then
      write(*,'(A,I0,A)') 'All ', total, ' CID files match their contents.'
    else
      write(*,'(A,I0,A)') 'Found ', mismatch_count, ' CID mismatches.'
      stop 1
    end if
  end subroutine process_files

  function compute_cid(path) result(cid)
    character(len=*), intent(in) :: path
    character(len=:), allocatable :: cid
    integer(int8), allocatable :: content(:)
    character(len=:), allocatable :: prefix, suffix
    logical :: ok

    content = read_file_bytes(path, ok)
    if (.not. ok) then
      cid = ''
      return
    end if

    prefix = encode_length(size(content, kind=int64))

    if (size(content) <= 64) then
      suffix = to_base64url(content)
    else
      suffix = to_base64url(sha512_bytes(path))
    end if

    cid = prefix // suffix
  end function compute_cid

  function read_file_bytes(path, success) result(bytes)
    character(len=*), intent(in) :: path
    logical, intent(out), optional :: success
    integer(int8), allocatable :: bytes(:)
    integer :: unit, ios
    integer(int64) :: size_bytes

    if (present(success)) success = .false.

    open(newunit=unit, file=path, access='stream', form='unformatted', status='old', action='read', iostat=ios)
    if (ios /= 0) then
      bytes = [integer(int8) ::]
      return
    end if

    inquire(unit, size=size_bytes)
    if (size_bytes < 0) then
      bytes = [integer(int8) ::]
      close(unit)
      return
    end if

    allocate(bytes(size_bytes))
    read(unit) bytes
    close(unit)

    if (present(success)) success = .true.
  end function read_file_bytes

  function encode_length(len64) result(prefix)
    integer(int64), intent(in) :: len64
    character(len=:), allocatable :: prefix
    integer(int8) :: bytes(8)
    integer(int64) :: temp
    integer :: i

    temp = len64
    do i = 8, 1, -1
      bytes(i) = int(iand(temp, int(z'FF', int64)), int8)
      temp = ishft(temp, -8)
    end do

    prefix = to_base64url(bytes)
  end function encode_length

  function sha512_bytes(path) result(hash)
    character(len=*), intent(in) :: path
    integer(int8), allocatable :: hash(:)
    character(len=200) :: line
    integer :: ios, hash_unit, space_pos

    call execute_command_line('sha512sum ' // path // ' > ' // hash_file, exitstat=ios)
    if (ios /= 0) then
      hash = [integer(int8) ::]
      return
    end if

    open(newunit=hash_unit, file=hash_file, status='old', action='read', iostat=ios)
    if (ios /= 0) then
      hash = [integer(int8) ::]
      return
    end if

    read(hash_unit, '(A)', iostat=space_pos) line
    close(hash_unit)

    if (space_pos /= 0) then
      hash = [integer(int8) ::]
      return
    end if

    space_pos = index(line, ' ')
    if (space_pos == 0) space_pos = len_trim(line) + 1

    hash = hex_to_bytes(trim(line(:space_pos-1)))
  end function sha512_bytes

  function hex_to_bytes(hex) result(bytes)
    character(len=*), intent(in) :: hex
    integer(int8), allocatable :: bytes(:)
    integer :: hex_len, i, val
    character(len=2) :: pair

    hex_len = len_trim(hex)
    if (mod(hex_len, 2) /= 0) then
      bytes = [integer(int8) ::]
      return
    end if

    allocate(bytes(hex_len/2))
    do i = 1, hex_len, 2
      pair = hex(i:i+1)
      val = hex_digit_value(pair(1:1)) * 16 + hex_digit_value(pair(2:2))
      bytes((i+1)/2) = int(val, int8)
    end do
  end function hex_to_bytes

  function hex_digit_value(ch) result(val)
    character(len=1), intent(in) :: ch
    integer :: val

    select case (ch)
    case ('0':'9')
      val = iachar(ch) - iachar('0')
    case ('a':'f')
      val = 10 + iachar(ch) - iachar('a')
    case ('A':'F')
      val = 10 + iachar(ch) - iachar('A')
    case default
      val = 0
    end select
  end function hex_digit_value

  function to_base64url(data) result(encoded)
    integer(int8), intent(in) :: data(:)
    character(len=:), allocatable :: encoded
    character(len=*), parameter :: alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_' 
    integer :: n, i, pos
    integer(int32) :: b0, b1, b2, i0, i1, i2, i3
    character(len=:), allocatable :: buffer

    n = size(data)
    if (n == 0) then
      encoded = ''
      return
    end if

    allocate(character(len=((n + 2) / 3) * 4) :: buffer)
    pos = 1

    do i = 1, n, 3
      b0 = iand(int(data(i), int32), int(z'FF', int32))
      if (i + 1 <= n) then
        b1 = iand(int(data(i + 1), int32), int(z'FF', int32))
      else
        b1 = 0
      end if
      if (i + 2 <= n) then
        b2 = iand(int(data(i + 2), int32), int(z'FF', int32))
      else
        b2 = 0
      end if

      i0 = ishft(b0, -2)
      i1 = ior(ishft(iand(b0, 3), 4), ishft(b1, -4))
      i2 = ior(ishft(iand(b1, 15), 2), ishft(b2, -6))
      i3 = iand(b2, 63)

      buffer(pos:pos) = alphabet(i0 + 1:i0 + 1); pos = pos + 1
      buffer(pos:pos) = alphabet(i1 + 1:i1 + 1); pos = pos + 1
      if (i + 1 <= n) then
        buffer(pos:pos) = alphabet(i2 + 1:i2 + 1); pos = pos + 1
      end if
      if (i + 2 <= n) then
        buffer(pos:pos) = alphabet(i3 + 1:i3 + 1); pos = pos + 1
      end if
    end do

    encoded = buffer(:pos-1)
  end function to_base64url

end program cid_check
