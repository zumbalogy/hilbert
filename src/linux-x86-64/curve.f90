subroutine test(pic) bind(c, name='test')
  !use, intrinsic :: iso_c_binding, only : c_double
  implicit none
  integer :: pic(1920 * 1282) ! a(*) also works
  integer :: i
  ! integer, value :: x

  do i = 1, size(pic)
     pic(i) = pic(i) * 2
  end do

end subroutine test

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1

function gray_encode(bn)
  implicit none
  integer :: bn
  integer :: gray_encode

  gray_encode = xor(bn, bn / 2)
end function gray_encode

function gray_decode(n)
  implicit none
  integer :: n
  integer :: sh, dv, n2

  sh = 1
  dv = rshift(n, sh)
  n2 = xor(n, dv)

  do while (dv > 1)
     sh = lshift(sh, 1)
  end do

  gray_decode = n2
end function gray_decode
