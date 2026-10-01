!> Descriptive statistics on a vector of reals.
module stats
  implicit none
  private

  integer, parameter, public :: dp = kind(1.0d0)

  type, public :: summary_t
     integer  :: n = 0
     real(dp) :: min, max, sum, mean, stddev
     real(dp) :: p25, median, p75, p90, p99
  end type summary_t

  public :: summarize, percentile, sort, histogram

contains

  !> Summary of x. Uses Welford's algorithm for a numerically stable variance.
  function summarize(x) result(s)
    real(dp), intent(in) :: x(:)
    type(summary_t) :: s
    real(dp), allocatable :: sorted(:)
    real(dp) :: mean, m2, delta
    integer :: i

    s%n = size(x)
    if (s%n == 0) return

    mean = 0.0_dp
    m2 = 0.0_dp
    do i = 1, s%n
       delta = x(i) - mean
       mean = mean + delta / i
       m2 = m2 + delta * (x(i) - mean)
    end do

    s%mean = mean
    s%stddev = 0.0_dp
    if (s%n > 1) s%stddev = sqrt(m2 / (s%n - 1))
    s%sum = sum(x)
    s%min = minval(x)
    s%max = maxval(x)

    sorted = x
    call sort(sorted)
    s%p25 = percentile(sorted, 25.0_dp)
    s%median = percentile(sorted, 50.0_dp)
    s%p75 = percentile(sorted, 75.0_dp)
    s%p90 = percentile(sorted, 90.0_dp)
    s%p99 = percentile(sorted, 99.0_dp)
  end function summarize

  !> p-th percentile of already-sorted data, linear interpolation
  !> between closest ranks (the same method as numpy's default).
  pure function percentile(sorted, p) result(v)
    real(dp), intent(in) :: sorted(:), p
    real(dp) :: v, rank, frac
    integer :: lo
    rank = 1.0_dp + (p / 100.0_dp) * (size(sorted) - 1)
    lo = int(rank)
    frac = rank - lo
    if (lo >= size(sorted)) then
       v = sorted(size(sorted))
    else
       v = sorted(lo) + frac * (sorted(lo + 1) - sorted(lo))
    end if
  end function percentile

  !> In-place bottom-up merge sort (stable, O(n log n) worst case).
  subroutine sort(a)
    real(dp), intent(inout) :: a(:)
    real(dp), allocatable :: buf(:)
    integer :: n, width, lo, mid, hi, i, j, k
    n = size(a)
    allocate(buf(n))
    width = 1
    do while (width < n)
       lo = 1
       do while (lo <= n - width)
          mid = lo + width - 1
          hi = min(lo + 2 * width - 1, n)
          i = lo; j = mid + 1; k = lo
          do while (i <= mid .and. j <= hi)
             if (a(i) <= a(j)) then
                buf(k) = a(i); i = i + 1
             else
                buf(k) = a(j); j = j + 1
             end if
             k = k + 1
          end do
          buf(k:k + mid - i) = a(i:mid)
          k = k + mid - i + 1
          buf(k:k + hi - j) = a(j:hi)
          a(lo:hi) = buf(lo:hi)
          lo = lo + 2 * width
       end do
       width = 2 * width
    end do
  end subroutine sort

  !> Count x into nbins equal-width bins spanning [lo, hi].
  pure function histogram(x, nbins, lo, hi) result(counts)
    real(dp), intent(in) :: x(:), lo, hi
    integer, intent(in) :: nbins
    integer :: counts(nbins), i, b
    counts = 0
    do i = 1, size(x)
       if (hi > lo) then
          b = 1 + int((x(i) - lo) / (hi - lo) * nbins)
       else
          b = 1
       end if
       b = min(max(b, 1), nbins)
       counts(b) = counts(b) + 1
    end do
  end function histogram

end module stats
