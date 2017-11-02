! TODO: some of these integers can be single bytes (or 2 bits even)
! TODO: try pure and intent markers

module gray_module
contains

  function gray_encode(bn)
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

  function gray_decode_travel(start, end, maskk, g)
    implicit none
    integer :: gray_decode_travel
    integer :: start, end, maskk, g
    integer :: travel_bit, modulus, rg
    integer :: gray_decode
    integer :: n
    integer :: sh, dv, n2

    travel_bit = ieor(start, end)
    modulus = maskk + 1

    if (0 == travel_bit) then
       ! TODO: something better here than this
       ! pretty sure this should never actually be zero
       ! so its probably a bug
       travel_bit = travel_bit + 1
    end if

    rg = ieor(g, start) * (modulus / (2 * travel_bit))

    n = (iand(maskk, ior(rg, rg / modulus)))


    sh = 1
    dv = rshift(n, sh)
    n2 = xor(n, dv)

    do while (dv > 1)
       sh = lshift(sh, 1)
       dv = rshift(n2, sh)
       n2 = xor(n2, dv)
    end do


    gray_decode_travel = n2


    !gray_decode_travel = gray_decode(iand(maskk, ior(rg, rg / modulus)))
  end function gray_decode_travel

  function gray_encode_travel(start, end, maskk, i)
    implicit none
    integer :: gray_encode_travel
    integer :: start, end, maskk, i
    integer :: travel_bit, modulus, g
    integer :: gray_encode

    travel_bit = ieor(start, end)
    modulus = maskk + 1
    g = (travel_bit * 2) * ieor(i, i / 2) ! use gray_encode(i) here or get rid of it or something, i guess

    gray_encode_travel = ieor(start, iand(maskk, ior(g, g / modulus)))
  end function gray_encode_travel

end module gray_module

function child_start_end(p_start, p_end, maskk, i)
  ! consider making this 2 funcitons, one for start, one for end
  ! or one function, but call it twice with dif values
  use gray_module
  implicit none
  integer :: child_start_end(2)
  integer :: p_start, p_end, maskk, i
  integer :: start_i, end_i, child_start, child_end

  start_i = max(0, iand(-2, i - 1))
  end_i = min(maskk, ior(1, i + i))
  child_start = gray_encode_travel(p_start, p_end, maskk, start_i)
  child_end = gray_encode_travel(p_start, p_end, maskk, end_i)

  child_start_end = [child_start, child_end]
end function child_start_end

function child_start(p_start, p_end, maskk, i)
  ! consider making this 2 funcitons, one for start, one for end
  ! or one function, but call it twice with dif values
  use gray_module
  implicit none
  integer :: child_start
  integer :: p_start, p_end, maskk, i
  integer :: start_i

  start_i = max(0, iand(-2, i - 1))
  child_start = gray_encode_travel(p_start, p_end, maskk, start_i)
end function child_start

function child_end(p_start, p_end, maskk, i)
  ! consider making this 2 funcitons, one for start, one for end
  ! or one function, but call it twice with dif values
  use gray_module
  implicit none
  integer :: child_end
  integer :: p_start, p_end, maskk, i
  integer :: end_i

  end_i = min(maskk, ior(1, i + i))
  child_end = gray_encode_travel(p_start, p_end, maskk, end_i)

end function child_end

module transpose_bits_module
contains

  function transpose_bits(srcs, ndests)
    implicit none
    integer, allocatable :: transpose_bits(:) ! this is suspect
    integer, allocatable :: dests(:)
    integer :: ndests
    integer, allocatable :: srcs(:)
    ! TODO: have srcs have a proper bound
    integer :: nsrcs, dest, j, k, sk

    ! nsrcs = size(srcs)

    ! do j = ndests - 1, -1, -1
    !    dest = 0
    !    do k = 1, nsrcs
    !       sk = srcs(k)
    !       dest = (dest * 2) + mod(sk, 2)
    !       srcs(k) = sk / 2
    !    end do
    !    dests(j) = dest
    ! end do

    ! transpose_bits = dests
    transpose_bits = [1,2]
  end function transpose_bits

end module transpose_bits_module


function logg(a, b) bind(c)
  ! TODO: look into making this more percise
  implicit none
  real(8) :: logg
  integer, value :: a, b

  logg = log(real(a)) / log(real(b))
end function logg


module unpack_coords_module
contains

  function unpack_coords(coords)
    use transpose_bits_module
    implicit none
    integer, allocatable :: unpack_coords(:)
    integer, allocatable :: coords(:)
    integer :: biggest, nchunks
    real(8) :: logg

    biggest = maxval(coords)
    ! specifying kind maybe better RESULT = CEILING(A [, KIND])
    nchunks = max(1, ceiling(logg(1 + biggest, 2)))

    unpack_coords = transpose_bits(coords, nchunks)

  end function unpack_coords

end module unpack_coords_module

module unpack_index_module
contains

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

end module unpack_index_module


module pack_index_module
contains

  function pack_index(chunks, nd)
    implicit none
    integer(8) :: pack_index
    integer :: chunks(:)
    integer :: nd
    integer :: p, i
    integer :: out

    p = 2 ** nd

    out = 0

    do i = 1, size(chunks)
       out =  out + (chunks(i) * p)
    end do

    pack_index = out

  end function pack_index
end module pack_index_module

function initial_end(nchunks, nd)
  implicit none
  integer :: initial_end
  integer :: nchunks, nd

  initial_end = 2 ** mod((-1 * nchunks) - 1, nd)
end function initial_end

subroutine test(pic) bind(c)
  !use, intrinsic :: iso_c_binding, only : c_double
  implicit none
  integer :: pic(1920 * 1282)
  integer :: pic2(1920 * 1282)
  integer :: i, pic_size, max_h, max_color_h, width, height
  integer :: p, x, y, r, g, b, h, x2, y2, h2, p2
  integer, allocatable :: coords(:)

  pic2 = pic
  pic_size = size(pic)
  width = int(sqrt(real(pic_size)))

  coords = [1, 2]
  print*, 1
  max_h = coord_to_int(coords)
  print*, 2

  do i = 1, 10
     ! p = pic(i)
     ! x = mod(i, width)
     ! y = i / width
     ! ! h = coord_to_int([x, y])
     ! ! coords = int_to_coord(mod(h + 1000, max_h), 2)
     ! coords = int_to_coord(mod(100 * i, pic_size), 2)
     ! h2 = coords(1) + (width * coords(2))
     ! p2 = pic(mod(h2, pic_size))
     p2 = 100
     pic(i) = p2
  end do

contains

  function int_to_coord(i, nd)
    use transpose_bits_module
    use unpack_index_module
    use gray_module
    implicit none
    integer, allocatable :: int_to_coord(:)
    integer, allocatable :: index_chunks(:)
    integer, allocatable :: coord_chunks(:)

    integer :: child_start
    integer :: child_end
    integer :: initial_end

    integer :: i, nd
    integer :: nchunks, mask, start, endd
    integer :: j, chunk, start2, endd2

    index_chunks = unpack_index(i, nd)
    nchunks = size(index_chunks)
    mask = (2 ** nd) - 1
    start = 0
    endd = initial_end(nchunks, nd)
    coord_chunks(:) = 0

    do j = 1, nchunks ! is this off by one?
       chunk = index_chunks(j)
       start2 = child_start(start, endd, mask, chunk)
       endd2 = child_end(start, endd, mask, chunk)
       coord_chunks(j) = gray_encode_travel(start, endd, mask, chunk)
       start = start2
       endd = endd2
    end do

    int_to_coord = transpose_bits(coord_chunks, nd)
  end function int_to_coord


  function coord_to_int(coords)
    use unpack_coords_module
    use pack_index_module
    use gray_module
    implicit none
    integer :: coord_to_int
    integer, allocatable :: coord_chunks(:)
    integer :: index_chunks(size(coord_chunks))

    integer :: child_start
    integer :: child_end
    integer :: initial_end

    integer, allocatable :: coords(:)
    integer :: nd, nchunks, mask, start, start2, endd, endd2
    integer :: j, k

    print*, 3243232

    nd = size(coords)
    coord_chunks = unpack_coords(coords)
    nchunks = size(coord_chunks)
    mask = (2 ** nd) - 1
    start = 0
    endd = initial_end(nchunks, nd)
    index_chunks(:) = 0 ! might want to allocate this size and not just do it in the loop


    print*, "this is number 55"
    do j = 1, nchunks
       print*, j
       k = gray_decode_travel(start, endd, mask, coord_chunks(j))
       print*, k
       start2 = child_start(start, endd, mask, k)
       print*, start2
       endd2 = child_end(start, endd, mask, k)
       print*, endd2, "qqqqqqqqqqqqqq"
       index_chunks(j) = k
       print*, nchunks, "sdfd"
       start = start2
       endd = endd2
    end do

    print*, "do i get this far?"

    coord_to_int = pack_index(index_chunks, nd)
  end function coord_to_int

end subroutine test
