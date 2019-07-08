!for age30~69
module mod_sterror
  use mod_gmm
  implicit none

contains

  real(8) function sterror(gmm,sterror)!    integer(8), intent(in) ::
    real(8), intent(in) :: gmm(7)
!    integer(8), intent(in) :: Anum, Wnum, AIMEnum, Bi
!    real(8), intent(in) :: V(:,:,:,:,:)
    real(8), intent(out) :: sterror(7)

    ! adjusted age index: age - mappage + 1
    integer(8) :: i
    real(8) :: gmm_i_p(7), gmm_i_n(7),gmm_dif_t(7), gmm_dif(7), product_a(7,7)

!    integer(8) :: Aidx, Widx, AIMEidx, Bidx
!    integer(8) :: Ai, Wi, AIMEi
    real(8) :: beta_1_p, beta_2_p, beta_3_p, beta_4_p, beta_5_p, beta_6_p, beta_7_p
    real(8) :: beta_1_n, beta_2_n, beta_3_n, beta_4_n, beta_5_n, beta_6_n, beta_7_n

    gmm_dif = 0.0_8
    do i = 1,7,1
      beta_i_p = 0.0_8
      beta_i_p_(i) = gmm(i) + 0.000001_8
      beta_i_n = 0.0_8
      beta_i_n_(i) = gmm(i) - 0.000001_8
      gmm_i_p = gmm(beta_i_p)
      gmm_i_n = gmm(beta_i_n)
      gmm_dif(i) = (gmm_i_p - gmm_i_n) / 0.000002_8
    end  do

    gmm_dif_t = transpose(gmm_dif)
    product_a = matmul(gmm_dif_t, gmm_dif)
    gmm_inv = inv(product)

    do i = 1,7,1
        sderror(i) = gmm_inv(i,i)
    end do

end function


! The following is from  "http://fortranwiki.org/fortran/show/Matrix+inversion"
function inv(A) result(Ainv)
  real(dp), dimension(:,:), intent(in) :: A
  real(dp), dimension(size(A,1),size(A,2)) :: Ainv

  real(dp), dimension(size(A,1)) :: work  ! work array for LAPACK
  integer, dimension(size(A,1)) :: ipiv   ! pivot indices
  integer :: n, info

  ! External procedures defined in LAPACK
  external DGETRF
  external DGETRI

  ! Store A in Ainv to prevent it from being overwritten by LAPACK
  Ainv = A
  n = size(A,1)

  ! DGETRF computes an LU factorization of a general M-by-N matrix A
  ! using partial pivoting with row interchanges.
  call DGETRF(n, n, Ainv, n, ipiv, info)

  if (info /= 0) then
     stop 'Matrix is numerically singular!'
  end if

  ! DGETRI computes the inverse of a matrix using the LU factorization
  ! computed by DGETRF.
  call DGETRI(n, Ainv, n, ipiv, work, n, info)

  if (info /= 0) then
     stop 'Matrix inversion failed!'
  end if
end function inv








end module mod_interp
