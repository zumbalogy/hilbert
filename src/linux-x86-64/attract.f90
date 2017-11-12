subroutine test(pic) bind(c)
  !use, intrinsic :: iso_c_binding, only : c_double
  implicit none

  ! integer :: pic(1920 * 1282)
  integer :: pic(256 * 256)
  integer :: pic_size
  integer :: i
  integer :: r, g, b, r2, g2, b2
  integer :: p, p2
  real :: w, x, y, z
  real :: j, k

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


     if ((r + b + g * 10) < pic(i + 1)) then
        r2 = int(sin(real(i) * g) + (1 * sin(real(r * x))))
        g2 = int(cos(real(g)) + (r * sin(real(i + z))))
        b2 = int(sin(real(i) * y) + (0.1 * sin(real(i * w))))
     else
        r2 = int(x + (1 * x * (r - b + 3)))
        g2 = int(g + i)
        b2 = int(sin(real(i) * 3) * (z * sin(real(i))))
     end if


     r = r2
     g = g2
     b = b2

     ! -16777216 = -1000000 hex
     p2 = -16777216
     p2 = ior(p2, lshift(iand(r, 255), 16))
     p2 = ior(p2, lshift(iand(g, 255), 8))
     p2 = ior(p2, iand(b, 255))

     pic(i) = p2
  end do

end subroutine test

! (defn clifford [[x y]]
!   (let [x2 (+ (Math/sin (* @a y)) (* @c (Math/cos (* @a x))))
!         y2 (+ (Math/sin (* @b x)) (* @d (Math/cos (* @b y))))]
!     [x2 y2]))


! (defn lorenz [[x y z]]
!   (let [x2 (+ x (* 0.01 @b (- y x)))
!         y2 (+ y (* 0.01 (- (* x (- 28 z)) y)))
!         z2 (+ z (* 0.01 (- (* y x) (* @d z))))]
!     [x2 y2 z2]))
