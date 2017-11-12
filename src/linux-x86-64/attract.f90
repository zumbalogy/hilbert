subroutine test(pic, input_iteration) bind(c)
  !use, intrinsic :: iso_c_binding, only : c_double
  implicit none

  ! integer :: pic(1920 * 1282)
  integer :: pic(5000 * 5000)
  integer :: pic_size, pic_width, pic_height
  integer :: i
  integer :: r, g, b, r2, g2, b2
  integer :: p, p2
  integer :: scale, index, j_scale, k_scale
  integer, value :: input_iteration
  real :: w, x, y, z
  real (8) :: j, k, j2, k2, j_index, k_index

  w = -1.7
  x =  real(1 + (input_iteration * 0.01))
  y = -1.9
  z = -0.4

  j = 0.0
  k = 0.0

  j_scale = 800
  k_scale = 800

  pic_width = 5000
  pic_height = 5000

  pic_size = size(pic)

  do i = 1, pic_size
     pic(i) = -16777216
  end do

  do i = 1, 80000000

     j2 = sin(real(w * k)) + (y * cos(real(w * j)))
     k2 = sin(real(y * j)) + (z * cos(real(x * k)))

     ! Center the attractor
     j_index = j2 + 3
     k_index = k2 + 3

     ! Scale it between 0 and 300
     j_index = int(j_index * j_scale)
     k_index = int(k_index * k_scale)


     index = j_index + (k_index * pic_width)

     if (index > 0) then
        if (index < pic_size) then

           p = pic(index)

           ! 0x00ff0000 == 16711680
           r = rshift(iand(p, 16711680), 16)
           ! 0x0000ff00 == 65280
           g = rshift(iand(p, 65280), 8)
           ! 0x000000ff == 225
           b = iand(p, 255)

           if (mod(i, 3) == 0) then
              r = min(255, r + 3)
           else if (mod(i, 3) == 1) then
              g = min(255, g + 1)
           else
              b = min(255, b + 2)
           end if

           ! -16777216 = -1000000 hex
           p2 = -16777216
           p2 = ior(p2, lshift(iand(r, 255), 16))
           p2 = ior(p2, lshift(iand(g, 255), 8))
           p2 = ior(p2, iand(b, 255))

           pic(index) = p2
        end if
     end if
     j = j2
     k = k2
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
