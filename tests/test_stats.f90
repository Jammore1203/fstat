program test_stats
  use stats
  implicit none
  type(summary_t) :: s
  real(dp) :: a(7)
  integer :: failures = 0

  s = summarize([2.0_dp, 4.0_dp, 4.0_dp, 4.0_dp, 5.0_dp, 5.0_dp, 7.0_dp, 9.0_dp])
  call check('mean', near(s%mean, 5.0_dp))
  call check('sample stddev', near(s%stddev, 2.138089935_dp))
  call check('median of even count', near(s%median, 4.5_dp))
  call check('min/max', near(s%min, 2.0_dp) .and. near(s%max, 9.0_dp))
  call check('p25 interpolates', near(s%p25, 4.0_dp))
  call check('p75 interpolates', near(s%p75, 5.5_dp))

  s = summarize([42.0_dp])
  call check('single value: stddev 0', near(s%stddev, 0.0_dp) .and. near(s%median, 42.0_dp))

  a = [5.0_dp, -1.0_dp, 3.0_dp, 3.0_dp, 10.0_dp, 0.0_dp, 2.5_dp]
  call sort(a)
  call check('merge sort', all(a(1:6) <= a(2:7)) .and. near(a(1), -1.0_dp))

  ! Welford stays accurate where the naive sum-of-squares formula breaks down
  s = summarize([1.0e9_dp + 4, 1.0e9_dp + 7, 1.0e9_dp + 13, 1.0e9_dp + 16])
  call check('stable variance with large offset', near(s%stddev, 5.477225575_dp))

  call check('histogram counts everything', &
       sum(histogram([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp], 3, 1.0_dp, 4.0_dp)) == 4)
  call check('max lands in last bin', &
       all(histogram([0.0_dp, 10.0_dp], 2, 0.0_dp, 10.0_dp) == [1, 1]))

  if (failures > 0) then
     print '(i0, a)', failures, ' test(s) FAILED'
     error stop 1
  end if
  print '(a)', 'All stats tests passed.'

contains

  logical function near(a, b)
    real(dp), intent(in) :: a, b
    near = abs(a - b) < 1.0e-6_dp
  end function near

  subroutine check(label, ok)
    character(*), intent(in) :: label
    logical, intent(in) :: ok
    if (ok) then
       print '(a, a)', '  PASS  ', label
    else
       print '(a, a)', '  FAIL  ', label
       failures = failures + 1
    end if
  end subroutine check
end program test_stats
