program test_gmcf_devel

    implicit none
    integer, dimension(NMODELS,NMODELS) :: gmcfConnectivityMatrix = reshape( (/  0, 1,  1, 0 /) , shape(gmcfConnectivityMatrix) )
    integer, parameter :: OCEAN_MODEL = 1 
    integer, parameter :: ATMOSPHERE_MODEL = 2
    integer, parameter :: GMCF_TIME_QUANTUM_MIN = 20
    integer, parameter :: GMCF_TIME_QUANTUM_SEC = GMCF_TIME_QUANTUM_MIN*60
    integer, parameter :: GMCF_N_SYNC_CONFIGS = 2
    ! For convenience, let's name the configurations
    integer, parameter :: OCEAN_SYNC_PERIOD = 10*24*3
    integer, parameter :: OCEAN_SYNC_OFFSET = 1 ! entirely ad-hoc
    integer, parameter :: ATMOSPHERE_SYNC_PERIOD = OCEAN_SYNC_PERIOD*2
    integer, parameter :: ATMOSPHERE_SYNC_OFFSET = 7 ! entirely ad-hoc
!    integer, dimension(GMCF_N_SYNC_CONFIGS,2) :: gmcfSyncConfigurations = reshape( (/  OCEAN_SYNC_PERIOD, ATMOSPHERE_SYNC_PERIOD, OCEAN_SYNC_OFFSET, ATMOSPHERE_SYNC_OFFSET /) , shape(gmcfSyncConfigurations) )
integer, dimension(NMODELS,GMCF_N_SYNC_CONFIGS,2) :: gmcfSyncConfigurations = reshape( (/  OCEAN_SYNC_PERIOD, OCEAN_SYNC_PERIOD, ATMOSPHERE_SYNC_PERIOD, ATMOSPHERE_SYNC_PERIOD, OCEAN_SYNC_OFFSET, OCEAN_SYNC_OFFSET, ATMOSPHERE_SYNC_OFFSET, ATMOSPHERE_SYNC_OFFSET /) , shape(gmcfSyncConfigurations) )

    integer :: i,j,k
    do k=1,2
    print *, "Search for model ",k
    do j=1,GMCF_N_SYNC_CONFIGS
        do i=1,2
        print *, gmcfSyncConfigurations(k,j,i)
        end do
    end do
    end do
end program test_gmcf_devel
    
