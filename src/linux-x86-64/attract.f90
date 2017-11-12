subroutine test(pic) bind(c)
  !use, intrinsic :: iso_c_binding, only : c_double
  implicit none

  ! integer :: pic(1920 * 1282)
  integer :: pic(300 * 300)
  integer :: pic_size, pic_width, pic_height
  integer :: i
  integer :: r, g, b, r2, g2, b2
  integer :: p, p2
  integer :: scale, index, j_scale, k_scale
  real :: w, x, y, z
  real :: j, k, j2, k2, j_index, k_index


  w = 1.533
  x = 11.8
  y = 1.62
  z = 0.2


  j = 0.0
  k = 0.0

  j_scale = 50
  k_scale = 50

  pic_width = 300
  pic_height = 300

  pic_size = size(pic)

  do i = 1, pic_size
     pic(i) = -16777216
  end do

  do i = 1, 5000000

     j2 = sin(real(w * k)) + (y * cos(real(w * j)))
     k2 = sin(real(y * j)) + (z * cos(real(y * k)))

     ! Center the attractor
     j_index = j2 + 3
     k_index = k2 + 3

     ! Scale it between 0 and 300
     j_index = int(j_index * j_scale)
     k_index = int(k_index * k_scale)


     index = j_index + (k_index * 300)

     if (index > 0) then
        if (index < pic_size) then
           pic(index) = min(pic(index) + 40, 0)
        end if
     end if

     ! pic(index) = 0

     j = j2
     k = k2
     ! p = pic(i)

     ! ! 0x00ff0000 == 16711680
     ! r = rshift(iand(p, 16711680), 16)
     ! ! 0x0000ff00 == 65280
     ! g = rshift(iand(p, 65280), 8)
     ! ! 0x000000ff == 225
     ! b = iand(p, 255)




     ! ! -16777216 = -1000000 hex
     ! p2 = -16777216
     ! p2 = ior(p2, lshift(iand(r, 255), 16))
     ! p2 = ior(p2, lshift(iand(g, 255), 8))
     ! p2 = ior(p2, iand(b, 255))

     ! pic(i) = p2
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
