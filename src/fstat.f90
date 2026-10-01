!> fstat - quick descriptive statistics for a column of numbers.
!>
!>   fstat [-c COL] [-d DELIM] [-b BINS] [-q] [FILE]
!>
!> Reads FILE (or stdin), takes column COL (default 1) split on DELIM
!> (default: comma, tab or space), and prints a summary and histogram.
!> Lines whose field is not a number (headers, blanks) are skipped.
program fstat
  use, intrinsic :: iso_fortran_env, only: input_unit, error_unit
  use stats
  implicit none

  integer :: col = 1, nbins = 10, unit, ios, nskip = 0, n = 0, i, nargs
  character(len=1) :: delim = ' '
  logical :: auto_delim = .true., quiet = .false.
  character(len=:), allocatable :: path, line, field
  character(len=256) :: arg
  real(dp), allocatable :: x(:), tmp(:)
  real(dp) :: v
  type(summary_t) :: s

  ! ---- arguments --------------------------------------------------
  nargs = command_argument_count()
  i = 1
  path = ''
  do while (i <= nargs)
     call get_command_argument(i, arg)
     select case (trim(arg))
     case ('-c')
        i = i + 1; call get_command_argument(i, arg); read (arg, *, iostat=ios) col
        if (ios /= 0 .or. col < 1) call die('-c needs a column number >= 1')
     case ('-b')
        i = i + 1; call get_command_argument(i, arg); read (arg, *, iostat=ios) nbins
        if (ios /= 0 .or. nbins < 1) call die('-b needs a bin count >= 1')
     case ('-d')
        i = i + 1; call get_command_argument(i, arg)
        delim = arg(1:1); auto_delim = .false.
        if (trim(arg) == '\t') delim = achar(9)
     case ('-q')
        quiet = .true.
     case ('-h', '--help')
        call usage(); stop
     case default
        if (arg(1:1) == '-') call die('unknown option ' // trim(arg))
        path = trim(arg)
     end select
     i = i + 1
  end do

  if (len(path) > 0) then
     open (newunit=unit, file=path, status='old', action='read', iostat=ios)
     if (ios /= 0) call die('cannot open ' // path)
  else
     unit = input_unit
  end if

  ! ---- read -------------------------------------------------------
  allocate (x(1024))
  do
     call read_line(unit, line, ios)
     if (ios /= 0) exit
     field = nth_field(line, col)
     read (field, *, iostat=ios) v
     if (ios /= 0 .or. len_trim(field) == 0) then
        nskip = nskip + 1
        cycle
     end if
     if (n == size(x)) then
        allocate (tmp(2 * size(x)))
        tmp(1:n) = x
        call move_alloc(tmp, x)
     end if
     n = n + 1
     x(n) = v
  end do

  if (n == 0) call die('no numeric values found')
  s = summarize(x(1:n))

  ! ---- report -----------------------------------------------------
  print '(a, t11, i0, a, i0, a)', 'count', s%n, '   (skipped ', nskip, ')'
  call show('sum', s%sum)
  call show('min', s%min)
  call show('max', s%max)
  call show('mean', s%mean)
  call show('stddev', s%stddev)
  call show('p25', s%p25)
  call show('median', s%median)
  call show('p75', s%p75)
  call show('p90', s%p90)
  call show('p99', s%p99)
  if (.not. quiet) call show_histogram(x(1:n), s)

contains

  subroutine show(label, value)
    character(*), intent(in) :: label
    real(dp), intent(in) :: value
    print '(a, t11, g0.6)', label, value
  end subroutine show

  subroutine show_histogram(x, s)
    real(dp), intent(in) :: x(:)
    type(summary_t), intent(in) :: s
    integer :: counts(nbins), b, bar
    real(dp) :: width
    character(len=24) :: lo_s, hi_s
    character(len=40) :: bar_s
    counts = histogram(x, nbins, s%min, s%max)
    width = (s%max - s%min) / nbins
    print '(a)', ''
    do b = 1, nbins
       write (lo_s, '(g0.4)') s%min + (b - 1) * width
       write (hi_s, '(g0.4)') s%min + b * width
       bar = nint(40.0 * counts(b) / maxval(counts))
       bar_s = repeat('#', bar)
       print '(a12, a, a12, a, a, 1x, i0)', adjustr(lo_s(1:12)), ' .. ', &
            adjustl(hi_s(1:12)), ' |', bar_s, counts(b)
    end do
  end subroutine show_histogram

  !> Read a whole line of any length.
  subroutine read_line(u, line, ios)
    integer, intent(in) :: u
    character(len=:), allocatable, intent(out) :: line
    integer, intent(out) :: ios
    character(len=512) :: chunk
    integer :: sz
    line = ''
    do
       read (u, '(a)', advance='no', iostat=ios, size=sz) chunk
       line = line // chunk(1:sz)
       if (ios /= 0) exit
    end do
    if (is_iostat_eor(ios)) ios = 0
  end subroutine read_line

  !> The k-th field of a line. In auto mode, commas, tabs and runs of
  !> spaces all separate fields.
  function nth_field(line, k) result(f)
    character(*), intent(in) :: line
    integer, intent(in) :: k
    character(len=:), allocatable :: f
    integer :: i, start, idx
    logical :: in_field
    f = ''
    idx = 0
    start = 1
    in_field = .false.
    do i = 1, len(line) + 1
       if (i > len(line)) then
          if (in_field .or. .not. auto_delim) then
             idx = idx + 1
             if (idx == k) f = trim(adjustl(line(start:)))
          end if
          exit
       end if
       if (is_sep(line(i:i))) then
          if (in_field .or. .not. auto_delim) then
             idx = idx + 1
             if (idx == k) then
                f = trim(adjustl(line(start:i - 1)))
                return
             end if
          end if
          in_field = .false.
          start = i + 1
       else if (.not. in_field) then
          in_field = .true.
          start = i
       end if
    end do
  end function nth_field

  logical function is_sep(c)
    character, intent(in) :: c
    if (auto_delim) then
       is_sep = c == ',' .or. c == ' ' .or. c == achar(9)
    else
       is_sep = c == delim
    end if
  end function is_sep

  subroutine usage()
    print '(a)', 'usage: fstat [-c COL] [-d DELIM] [-b BINS] [-q] [FILE]'
    print '(a)', '  -c COL    column to summarise, 1-based (default 1)'
    print '(a)', '  -d DELIM  field delimiter (default: comma, tab or spaces)'
    print '(a)', '  -b BINS   histogram bins (default 10)'
    print '(a)', '  -q        no histogram'
  end subroutine usage

  subroutine die(msg)
    character(*), intent(in) :: msg
    write (error_unit, '(a, a)') 'fstat: ', msg
    error stop 2
  end subroutine die

end program fstat
