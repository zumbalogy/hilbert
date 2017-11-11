subroutine test(pic) bind(c)
  !use, intrinsic :: iso_c_binding, only : c_double
  implicit none

  integer :: pic(256 * 256)
  integer :: pic_size
  integer :: i
  integer :: r, g, b
  integer :: p, p2
  real :: w, x, y, z

  w = 0.003
  x = 28.0
  y = 10.0
  z = 2.6666

  pic_size = size(pic)

  do i = 1, pic_size
     p = pic(i)

     ! 0x00ff0000 == 16711680
     r = rshift(iand(p, 16711680), 16)
     ! 0x0000ff00 == 65280
     g = rshift(iand(p, 65280), 8)
     ! 0x000000ff == 225
     b = iand(p, 255)

     ! -16777216 = -1000000 hex
     p2 = -16777216
     p2 = ior(p2, lshift(iand(r, 255), 16))
     p2 = ior(p2, lshift(iand(g, 255), 8))
     p2 = ior(p2, iand(b, 255))

     pic(i) = p2
  end do

end subroutine test
