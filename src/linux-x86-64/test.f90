! subroutine test(pic) bind(c)
!   use to_and_from_coord_module
!   !use, intrinsic :: iso_c_binding, only : c_double
!   implicit none
!   integer :: pic(1920 * 1282)
!   integer :: i, pic_size, max_h, max_color_h, width, height
!   integer :: p, x, y, r, g, b, h, x2, y2, h2, p2
!   integer:: coords(2)

!   pic_size = size(pic)
!   width = int(sqrt(real(pic_size)))

!   max_h = 1000
!   print*, size(pic)

!   do i = 1, size(pic) - 100
!      p = pic(i)
!      x = mod(i, width)
!      y = i / width
!      h = coord_to_int([x, y])
!      coords = int_to_coord(mod(h + 1000, max_h), 2)
!      h2 = coords(1) + (width * coords(2))
!      p2 = pic(mod(h2, pic_size))
!      pic(i) = p2
!   end do

! end subroutine test
