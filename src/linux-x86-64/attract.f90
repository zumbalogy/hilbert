subroutine test(pic) bind(c)
  !use, intrinsic :: iso_c_binding, only : c_double
  implicit none
  integer :: pic(256 * 256)
  integer :: pic2(256 * 256)
  integer :: pic_size
  integer :: i, p
  integer :: r, g, b
  integer(16) :: p2
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
     b = iand(p, 225)



     ! 0xFF000000 = 4278190080
     p2 = 4278190080.0
     p2 = ior(p2, lshift(iand(r, 255), 16))
     p2 = ior(p2, lshift(iand(g, 255), 8))
     p2 = ior(p2, iand(g, 255))

     pic(i) = p2
  end do

end subroutine test

 ! ([r g b]
 !    `(-> 0xFF000000
 !       (bit-or (bit-shift-left (bit-and (long ~r) 0xFF) 16))
 !       (bit-or (bit-shift-left (bit-and (long ~g) 0xFF) 8))
 !       (bit-or (bit-and (long ~b) 0xFF))))

! (def a (atom 0.003))
! (def b (atom 28))
! (def c (atom 10))
! (def d (atom 2.6666))

! (defn clifford [[x y]]
!   (let [x2 (+ (Math/sin (* @a y)) (* @c (Math/cos (* @a x))))
!         y2 (+ (Math/sin (* @b x)) (* @d (Math/cos (* @b y))))]
!     [x2 y2]))
