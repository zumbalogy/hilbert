subroutine test(pic) bind(c)
  !use, intrinsic :: iso_c_binding, only : c_double
  implicit none
  integer :: pic(1920 * 1282) ! a(*) also works
  integer :: i
  ! integer, value :: x

  do i = 1, size(pic)
     pic(i) = pic(i) * 2
  end do

end subroutine test

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! TODO: some of these integers can be single bytes (or 2 bits even)
! TODO: try pure and intent markers


function gray_encode(bn) bind(c)
  implicit none
  integer, value :: bn
  integer :: gray_encode

  gray_encode = ieor(bn, bn / 2)
end function gray_encode

function gray_decode(n)
  implicit none
  integer :: gray_decode
  integer :: n
  integer :: sh, dv, n2

  sh = 1
  dv = rshift(n, sh)
  n2 = xor(n, dv)

  do while (dv > 1)
     sh = lshift(sh, 1)
     dv = rshift(n2, sh)
     n2 = xor(n2, dv)
  end do

  gray_decode = n2
end function gray_decode

function gray_decode_travel(start, end, maskk, g) bind(c)
  implicit none
  integer :: gray_decode_travel
  integer, value :: start, end, maskk, g
  integer :: travel_bit, modulus, rg
  integer :: gray_decode

  travel_bit = ieor(start, end)
  modulus = maskk + 1
  rg = ieor(g, start) * (modulus / (2 * travel_bit))

  gray_decode_travel = gray_decode(iand(maskk, ior(rg, rg / modulus)))
end function gray_decode_travel

function gray_encode_travel(start, end, maskk, i) bind(c)
  implicit none
  integer :: gray_encode_travel
  integer, value :: start, end, maskk, i
  integer :: travel_bit, modulus, g
  integer :: gray_encode

  travel_bit = ieor(start, end)
  modulus = maskk + 1
  g = (travel_bit * 2) * ieor(i, i / 2) ! use gray_encode(i) here or get rid of it or something, i guess

  gray_encode_travel = ieor(start, iand(maskk, ior(g, g / modulus)))
end function gray_encode_travel

function child_start_end(p_start, p_end, maskk, i)
  ! consider making this 2 funcitons, one for start, one for end
  ! or one function, but call it twice with dif values
  implicit none
  integer :: child_start_end(2)
  integer :: p_start, p_end, maskk, i
  integer :: start_i, end_i, child_start, child_end
  integer :: gray_encode_travel

  start_i = max(0, iand(-2, i - 1))
  end_i = min(maskk, ior(1, i + i))
  child_start = gray_encode_travel(p_start, p_end, maskk, start_i)
  child_end = gray_encode_travel(p_start, p_end, maskk, end_i)

  child_start_end = [child_start, child_end]
end function child_start_end

function transpose_bits(srcs, ndests)
  implicit none
  integer :: transpose_bits(ndests)
  integer :: dests(ndests)
  integer :: ndests
  integer :: srcs(:)
  ! TODO: have srcs have a proper bound
  integer :: nsrcs, dest, j, k, sk

  nsrcs = size(srcs)

  do j = ndests - 1, -1, -1
     dest = 0
     do k = 1, nsrcs
        sk = srcs(k)
        dest = (dest * 2) + mod(sk, 2)
        srcs(k) = sk / 2
     end do
     dests(j) = dest
  end do

  transpose_bits = dests
end function transpose_bits


function logg(a, b) bind(c)
  ! TODO: look into making this more percise
  implicit none
  real(8) :: logg
  integer, value :: a, b

  logg = log(real(a)) / log(real(b))
end function logg


function unpack_cords(coords)
  implicit none
  integer, allocatable :: unpack_cords(:)
  integer :: coords(2)
  integer :: biggest, nchunks
  real(8) :: logg
  integer, allocatable :: transpose_bits

  biggest = maxval(coords)
  ! specifying kind maybe better RESULT = CEILING(A [, KIND])
  nchunks = max(1, ceiling(logg(1 + biggest, 2)))

  unpack_cords = transpose_bits(coords, nchunks)

  ! I think ill have to use subroutines for things that return arrays
  ! or put it place where it can see transpose bits is a func
end function unpack_cords

function unpack_index(i, nd)
  implicit none
  integer :: i, nd
  real(8) :: logg
  integer :: j, k, p
  integer :: nchunks
  integer, allocatable :: chunks(:)
  integer, allocatable :: unpack_index(:)

  p = 2 ** nd
  nchunks = max(1, ceiling(log(real(1 + i)) / log(real(2))))

  chunks = (/ (I, I = 1, 10) /)

  unpack_index = chunks

end function unpack_index
