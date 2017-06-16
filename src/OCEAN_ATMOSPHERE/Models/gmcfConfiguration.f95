! The idea is this config file contains information necessary for the coupling process but not specific to any of the models
! Model-specific configurations should go into gmcfAPI<model> or maybe better in gmcfConfiguration<model>

module gmcfConfiguration
    implicit none
    integer, dimension(NMODELS,NMODELS) :: gmcfConnectivityMatrix = reshape( (/  0, 1,  1, 0 /) , shape(gmcfConnectivityMatrix) )

    ! We termine the smallest quantum of time, i.e. the greatest common denominator of all periods
    ! Note that the scenario should specify the unit and we'll have to deal with this later
    integer, parameter :: GMCF_TIME_QUANTUM = 20 ! minutes

    integer, parameter :: GMCF_TEMPERATURE=1,GMCF_WIND_PROFILE=2, GMCF_OCEAN_ID=1, GMCF_ATMOSPHERE_ID=2

    integer, parameter :: GMCF_N_SYNC_CONFIGS = 2
    ! For convenience, let's name the configurations <from model>_<to model>_<data>
    integer, parameter :: ATMOSPHERE_OCEAN_WIND_PROFILE = 1
    integer, parameter :: OCEAN_ATMOSPHERE_TEMP = 2
    integer, parameter :: OCEAN_SYNC_PERIOD = 10*24*3
    integer, parameter :: OCEAN_SYNC_OFFSET = 1 ! entirely ad-hoc
    integer, parameter :: ATMOSPHERE_SYNC_PERIOD = OCEAN_SYNC_PERIOD
    integer, parameter :: ATMOSPHERE_SYNC_OFFSET = 7 ! entirely ad-hoc

!    Model1, Config 1, sync: OCEAN_SYNC_PERIOD,      Model2, Config 1, sync: OCEAN_SYNC_PERIOD,
!                      offs: OCEAN_SYNC_OFFSET,                              OCEAN_SYNC_OFFSET,
!    Model1, Config 2, sync: ATMOSPHERE_SYNC_PERIOD, Model2, Config 2, sync ATMOSPHERE_SYNC_PERIOD,
!                            ATMOSPHERE_SYNC_OFFSET,                        ATMOSPHERE_SYNC_OFFSET

    integer, dimension(NMODELS,GMCF_N_SYNC_CONFIGS,2) :: gmcfSyncConfigurations = reshape( (/  OCEAN_SYNC_PERIOD, OCEAN_SYNC_PERIOD, ATMOSPHERE_SYNC_PERIOD, ATMOSPHERE_SYNC_PERIOD, OCEAN_SYNC_OFFSET, OCEAN_SYNC_OFFSET, ATMOSPHERE_SYNC_OFFSET, ATMOSPHERE_SYNC_OFFSET /) , shape(gmcfSyncConfigurations) )


end module gmcfConfiguration
