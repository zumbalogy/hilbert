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
  real (16) :: w, x, y, z
  real (16) :: j, k, j2, k2, l, l2, j_index, k_index

  ! w = -1.7
  ! x =  (-1) + (4 * 0.01130001)
  ! ! x = -0.943500001
  ! y = -1.9
  ! z = -0.4

  ! a = 10, b = 28, c = 8 / 3. Another is a = 28, b = 46.92, c = 4

  !! lorenz
  x = 10.0
  y = 28.0
  z = 8.0 / 3.0

  j = 0.1
  k = 0.0
  l = 0.0

  j_scale = 0.1
  k_scale = 0.1

  pic_width = 5000
  pic_height = 5000

  pic_size = size(pic)

  do i = 1, pic_size
     pic(i) = -16777216
  end do

  do i = 1, 20

     ! !! clifford
     ! j2 = sin(real(w * k)) + (y * cos(real(w * j)))
     ! k2 = sin(real(y * j)) + (z * cos(real(x * k)))

     ! !! duffing
     ! j2 = j + (w * y)
     ! k2 = k + (w * ((j - (j * j * j) - (y * k)) + (z * cos(l))))
     ! l2 = l + w

     !! lorenz
     j2 = j + (0.01 * x * (k - j))
     k2 = k + (0.01 * ((j * (b - l)) - k))
     l2 = l + (0.01 * ((j * k) - (z * l)))



     ! Center the attractor
     j_index = j2 + 3
     k_index = k2 + 3

     ! Scale it
     j_index = int(j_index * j_scale)
     k_index = int(k_index * k_scale)


     index = j_index + (k_index * pic_width)



     print*, index

     if (index > 0) then
        if (index < pic_size) then


           p = pic(index)

           ! 0x00ff0000 == 16711680
           r = rshift(iand(p, 16711680), 16)
           ! 0x0000ff00 == 65280
           g = rshift(iand(p, 65280), 8)
           ! 0x000000ff == 225
           b = iand(p, 255)

           ! if (mod(i, 3) == 0) then
           !    r = min(255, r + 3)
           ! else if (mod(i, 3) == 1) then
           !    g = min(255, g + 2)
           ! end if

           ! r = min(255, r + 1)
           ! b = min(255, b + 1)

           b = 255
           r = 255

           ! if (r > 253) then
           !    b = min(255, b + 70)
           ! else
           !    r = r + 2
           ! end if

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
